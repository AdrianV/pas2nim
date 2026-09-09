#
#           pas2nimony - Pascal to Nimony translator
#
# Parser for the Delphi/Pascal dialect. Ports the translation decisions of
# the old pas2nim parser (pasparse.nim) but produces this tool's own AST
# and keeps identifiers with their declared spelling.
#
# Compared to the old parser, this one additionally:
#  - records every declared name for case canonicalization
#  - records class members (fields/routines/ctors) for self-qualification
#  - rewrites `Second.create(args)` into `create(Second(), args)` (nimony
#    does not accept subtype typedesc constraints in templates)
#  - defers property accessor generation until all class members are known

import std/[strutils, tables, syncio, os]
import pasast, paslex, passym

const
  MaxLineLength = 80

type
  TContextKind* = enum conExpr, conStmt, conTypeDesc

type
  TExtraInfo* = enum eiConstructor, eiPublic
  TExtraStmt* = ref object
    node*: Node
    flags*: set[TExtraInfo]

  TPropDecl* = ref object
    ## a property declaration; accessors are generated in a post-pass
    cls, name*: string          ## spellings
    typ*: Node                  ## property type
    params*: Node               ## array property: param defs (nkIdentDefs)
    readId, writeId*: string    ## accessor names ("" if absent)
    isDefault*: bool
    isPublic*: bool

  UnitSet* = ref object
    files*: Table[string, bool]   ## unit files already parsed for symbols

  TParser* = object
    lex*: TLexer
    tok*: TToken
    aheadTok*: TToken
    hasAhead*: bool
    section*: TSection
    arrayLows*: Table[string, int]   # Pascal 1-based array low bounds
    inParamList*: bool
    context*: TContextKind
    visibility*: TVisibility
    selfClass*: string          ## spelling of the current class ("" if none)
    searchPaths*: seq[string]   ## --path: dirs for uses resolution
    sourceDefines*: Table[string, bool] ## {$define}d in this source
    flags*: set[TParserFlag]
    syms*: SymTab
    extra*: seq[TExtraStmt]
    props*: seq[TPropDecl]
    outerProcName*: string      ## enclosing routine (for `inherited`)
    outerIsMethod*: bool        ## enclosing routine is a virtual method
    outerParams*: seq[string]   ## param names of the enclosing routine
    module*: Node               ## the nkStmtList built so far
    varTypes*: Table[string, string] ## lowercase var name -> "set"|"other"
    paramTypes*: Table[string, string] ## current routine's param name -> mapped type
    fieldTypes*: Table[string, string] ## "class.field" (lowercase) -> mapped type
    paramClassTypes*: Table[string, string]
    classFieldTypes*: Table[string, string]  ## "cls.field" -> class spelling
    curLabels*: seq[string]     ## declared labels of the current routine
    intfSigs*: Table[string, seq[Node]]  ## interface key -> base-method defs
    classVarHoist*: seq[Node]   ## class var decls hoisted to module level
    recordTypes*: Table[string, bool]  ## lowercase record type names
    arrayAliases*: Table[string, string] ## array alias -> element spelling
    curTypeParams*: seq[string]  ## params of the type being declared
    genericArgDepth*: int  ## > 0 while parsing `<...>` generic args
    withTemps*: seq[string]     ## hidden per-with temporaries by depth
    withClasses*: seq[string]   ## class spelling per with depth
    withDepth*: int             ## active with-scope count
    withCounter*: int           ## unique temp-name source
    methodPtrTypes*: Table[string, Node] ## method-ptr type name -> its formal params
    methodPtrVars*: Table[string, Node] ## var/field/param/prop name -> its formal params
    thunkCounter*: int                 ## synthesized event-thunk serial
    absorbed*: UnitSet               ## shared unit-absorption cycle guard
    unitFiles*: Table[string, string] ## lowercase unit name -> module file stem
    classOfProc*: string        ## class the current routine belongs to
    qualClass*: string          ## class context of the self-qualify pass
    nestedProcs*: seq[string]   ## nested routine names of the current proc
    arrayTypeLows*: Table[string, int]  ## alias type name -> declared low

# ---------------------------------------------------------------------------
# token plumbing

proc getTokP(p: var TParser) =
  if p.hasAhead:
    p.tok = p.aheadTok
    p.hasAhead = false
  else:
    getTok(p.lex, p.tok)

proc peekTok*(p: var TParser): TToken =
  if not p.hasAhead:
    getTok(p.lex, p.aheadTok)
    p.hasAhead = true
  result = p.aheadTok

proc removeNextTok(p: var TParser) =
  if p.hasAhead:
    p.tok = p.aheadTok
    p.hasAhead = false
  else:
    getTokP(p)

proc parLineInfo(p: TParser): TLineInfo = p.tok.info

proc parError*(p: TParser, msg: string) =
  write(stderr, renderInfo(p.tok.info) & " Error: " & msg & "\n")
  quit(1)

var gWarned: Table[string, bool] = initTable[string, bool]()

proc parWarning(p: var TParser, key, msg: string) =
  ## non-fatal diagnostic, deduplicated per process (the pipeline
  ## re-parses sources, so parser-local state would repeat the same
  ## site); under --strict it escalates to an error
  if pfStrictDirectives in p.flags:
    write(stderr, renderInfo(p.tok.info) & " Error: " & msg & "\n")
    quit(1)
  if not gWarned.getOrDefault(key, false):
    gWarned[key] = true
    write(stderr, renderInfo(p.tok.info) & " Warning: " & msg & "\n")

proc skipCom(p: var TParser) =
  while p.tok.xkind == pxComment:
    getTokP(p)

proc eat(p: var TParser, xkind: TTokKind) =
  if p.tok.xkind == xkind: getTokP(p)
  else: parError(p, "expected " & tokKindToStr(xkind) & " but got: " & $p.tok)

proc opt(p: var TParser, xkind: TTokKind) =
  if p.tok.xkind == xkind: getTokP(p)

proc newNodeP(kind: NodeKind, p: TParser): Node =
  newNode(kind, p.tok.info)

proc newIdentNameNodeP(name: string, p: TParser): Node =
  newIdentNode(name, p.tok.info)

proc openParser*(p: var TParser, filename: string, flags: set[TParserFlag],
                 defines: seq[string] = @[],
                 searchPaths: seq[string] = @[]) =
  p.lex = TLexer()
  p.lex.openLexer(filename)
  p.flags = flags
  p.syms = initSymTab()
  for d in defines:
    # conditional symbols are case-insensitive (Delphi model); a
    # `-d:FOO=bar` style value defines the name only
    let n = d.toLowerAscii
    let eq = n.find('=')
    p.syms.defines[if eq >= 0: n[0 ..< eq] else: n] = true
  p.searchPaths = searchPaths
  p.sourceDefines = initTable[string, bool]()
  var us = UnitSet()
  us.files = initTable[string, bool]()
  p.absorbed = us
  p.unitFiles = initTable[string, string]()
  p.module = newNode(nkStmtList, TLineInfo(line: 0, col: 0, file: filename))
  # predeclared System-unit exception class (M4-2): instances ride
  # systempas' current-exception slot; ErrorCode stays the transport.
  # The Nim spelling is PasException (Nim's system reserves
  # `Exception`); the Pascal name aliases it in the registries
  p.syms.registerClass("PasException", "", true)
  p.syms.addField("PasException", "Message")
  # the Pascal spelling - the ctor registration seeds the names
  # registry (first declaration wins), and a lowercase `create` here
  # would leak into every `TStringList.Create` emission
  p.syms.addCtor("PasException", "Create")
  p.syms.returnsValue["pasexccreate"] = true
  p.syms.names["exception"] = "PasException"
  var excAlias = p.syms.classes.getOrDefault("pasexception")
  p.syms.classes["exception"] = excAlias
  getTokP(p)

proc closeParser*(p: var TParser) =
  closeLexer(p.lex)

proc parseExpr*(p: var TParser): Node
proc parseStmt*(p: var TParser): Node
proc parseTypeDesc*(p: var TParser, definition: Node): Node
proc parseRoutine*(p: var TParser; noBody: bool): Node
proc parseIdentColonEquals*(p: var TParser; withVis: bool): Node
proc parseTypeSection*(p: var TParser): Node
proc parseConstSection*(p: var TParser): Node
proc parseVarSection*(p: var TParser): Node
proc parseProperty*(p: var TParser): Node
proc parseRecordOrObject*(p: var TParser, kind: NodeKind,
                          definition: Node): Node
proc parseRoutineSpecifiers*(p: var TParser, noBody: var bool,
                             isVirtual: var bool; isOverride: var bool;
                             sawReintroduce: var bool): Node
proc genPropertyAccessors*(p: var TParser, module: Node)

proc exSymbol*(n: Node, isPublic: bool): Node =
  ## mark an ident as exported (rendered as `ident*`)
  n.exported = isPublic
  result = n

# ---------------------------------------------------------------------------
# compiler directives {$...}

proc parseStmtList(p: var TParser): Node

proc isHandledDirective(p: TParser): bool =
  result = false
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    case p.tok.ident.toLowerAscii
    of "else", "endif": result = false
    else: result = true

proc definedExpr(p: var TParser): Node =
  result = newNodeP(nkCall, p)
  result.add(newIdentNameNodeP("defined", p))
  if p.tok.xkind == pxSymbol:
    result.add(newIdentNode(p.tok.ident, p.tok.info))
    getTokP(p)
  else:
    parError(p, "identifier expected in directive")

proc parseIfDirAux(p: var TParser, result: Node) =
  result[0].add(parseStmtList(p))
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    let endMarker = succ(p.tok.xkind)
    if p.tok.ident.toLowerAscii == "else":
      let s = newNodeP(nkElse, p)
      while p.tok.xkind != pxEof and p.tok.xkind != endMarker: getTokP(p)
      p.eat(endMarker)
      s.add(parseStmtList(p))
      result.add(s)
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      let endMarker2 = succ(p.tok.xkind)
      if p.tok.ident.toLowerAscii == "endif":
        while p.tok.xkind != pxEof and p.tok.xkind != endMarker2: getTokP(p)
        p.eat(endMarker2)
      else:
        parError(p, "{$endif} expected")

proc parseCondName(p: var TParser): string =
  ## the conditional symbol inside `{$ifdef NAME}` (case-insensitive)
  result = ""
  if p.tok.xkind == pxSymbol:
    result = p.tok.ident.toLowerAscii
    getTokP(p)
  else:
    parError(p, "identifier expected in conditional directive")

proc skipCondBranch(p: var TParser, endMarker: TTokKind): bool =
  ## skip tokens up to the branch end; true when a depth-0 `{$else}`
  ## was reached (its token is KEPT for the caller), false after the
  ## matching `{$endif}` was consumed. Nesting-aware: inner
  ## {$ifdef}/{$if} groups are traversed.
  var depth = 0
  while p.tok.xkind != pxEof:
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      let em = succ(p.tok.xkind)
      case p.tok.ident.toLowerAscii
      of "ifdef", "ifndef", "if":
        inc depth
      of "else":
        if depth == 0:
          return true
      of "endif":
        if depth == 0:
          getTokP(p)
          p.eat(em)
          return false
        dec depth
      else: discard
    getTokP(p)
  result = false

proc parseCondDir(p: var TParser, endMarker: TTokKind, negate: bool): Node =
  ## Delphi-model conditional compilation: the condition is evaluated
  ## at parse time (CLI -d: defines + {$define}); the dead branch is
  ## skipped at the token level, so its units never absorb and its
  ## declarations never register. The taken branch's statements are
  ## returned as a flattened nkStmtList (both renderers inline it).
  getTokP(p)                    # skip `{$ifdef` / `{$ifndef`
  let name = parseCondName(p)
  let defined = p.syms.defines.getOrDefault(name, false)
  let taken = if negate: not defined else: defined
  p.eat(endMarker)              # closing brace of the directive
  result = newNodeP(nkStmtList, p)
  result.strVal = "#condsplice"
  template consumeDir() {.dirty.} =
    let emX = succ(p.tok.xkind)
    getTokP(p)
    p.eat(emX)
  if taken:
    skipCom(p)
    while true:
      if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        case p.tok.ident.toLowerAscii
        of "else":
          # the dead alternative is skipped to the matching {$endif}
          consumeDir()
          discard skipCondBranch(p, endMarker)
          break
        of "endif":
          consumeDir()
          break
        else:
          result.add(parseStmt(p))
      elif p.tok.xkind == pxEof:
        break
      else:
        result.add(parseStmt(p))
        p.opt(pxSemiColon)
        skipCom(p)
  else:
    if skipCondBranch(p, endMarker):
      # the {$else} branch is live: consume the kept token first
      consumeDir()
      skipCom(p)
      while true:
        if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
          if p.tok.ident.toLowerAscii == "endif":
            consumeDir()
            break
          else:
            result.add(parseStmt(p))
            p.opt(pxSemiColon)
            skipCom(p)
        elif p.tok.xkind == pxEof:
          break
        else:
          result.add(parseStmt(p))
          p.opt(pxSemiColon)
          skipCom(p)
  if result.len == 0:
    result = newNodeP(nkEmpty, p)

proc parseIfdefDir(p: var TParser, endMarker: TTokKind): Node =
  parseCondDir(p, endMarker, false)

proc parseIfndefDir(p: var TParser, endMarker: TTokKind): Node =
  parseCondDir(p, endMarker, true)

proc parseIfDir(p: var TParser, endMarker: TTokKind): Node =
  result = newNodeP(nkWhenExpr, p)
  let branch = newNodeP(nkElifBranch, p)
  getTokP(p)                    # skip `{$if`
  branch.add(parseExpr(p))
  result.add(branch)
  p.eat(endMarker)
  parseIfDirAux(p, result)

proc parseDirective(p: var TParser): Node =
  result = emptyNode(p.tok.info)
  if not (p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}): return
  let endMarker = succ(p.tok.xkind)
  if p.tok.ident.len > 0:
    case p.tok.ident.toLowerAscii
    of "if": result = parseIfDir(p, endMarker)
    of "ifdef": result = parseIfdefDir(p, endMarker)
    of "ifndef": result = parseIfndefDir(p, endMarker)
    of "define":
      getTokP(p)
      let nm = parseCondName(p)
      p.syms.defines[nm] = true
      p.sourceDefines[nm] = true
      p.eat(endMarker)
    of "undef":
      getTokP(p)
      let nm = parseCondName(p)
      p.syms.defines[nm] = false
      p.eat(endMarker)
    else:
      # skip unknown compiler directive
      while p.tok.xkind != pxEof and p.tok.xkind != endMarker: getTokP(p)
      p.eat(endMarker)
  else:
    p.eat(endMarker)

# ---------------------------------------------------------------------------
# uses clause

proc absorbUnit*(p: var TParser, unitName: string) =
  ## register the used unit's declarations (classes, ctors, routines,
  ## canonical spellings) by parsing the unit file next to this one;
  ## the --path: search paths are consulted when the unit is not next
  ## to the importer
  let dir = parentDir(p.lex.filename)
  var unitFile = ""
  var dirs: seq[string] = @[]
  if dir.len > 0: dirs.add(dir)
  dirs.add(".")
  for sp in p.searchPaths:
    if sp notin dirs: dirs.add(sp)
  for cand in [unitName, unitName.toLowerAscii,
               unitName[0].toUpperAscii & unitName[1..^1].toLowerAscii]:
    for d in dirs:
      let f = d / cand & ".pas"
      if fileExists(f):
        unitFile = f
        break
    if unitFile.len > 0: break
  if unitFile.len == 0 or p.absorbed.files.hasKey(unitFile):
    return
  p.unitFiles[unitName.toLowerAscii] = splitFile(unitFile).name
  p.absorbed.files[unitFile] = true
  var up = default(TParser)
  # CLI defines propagate into units; source-level {$define}s stay
  # local to the unit that made them (Delphi-like scoping)
  var inheritedDefines: seq[string] = @[]
  for d, v in p.syms.defines:
    if v and not p.sourceDefines.hasKey(d):
      inheritedDefines.add(d)
  for sp in p.searchPaths:
    up.searchPaths.add(sp)
  openParser(up, unitFile, p.flags, inheritedDefines)
  up.absorbed = p.absorbed   # shared ref: cycle guard works across units
  discard parseUnit(up)
  closeParser(up)
  for k, ci in up.syms.classes:
    if not p.syms.classes.hasKey(k):
      p.syms.classes[k] = ci
  for k, v in up.syms.names:
    if not p.syms.names.hasKey(k):
      p.syms.names[k] = v


proc scanNimExports(ln: string; s: var SymTab) =
  ## register the exported identifier at the head of one Nim source
  ## line (`proc* name`, `proc name*(`, `const name* =`, `Foo* =`)
  var t = ln
  var k = 0
  while k < t.len and t[k] in {' ', '\t'}: inc k
  if k > 0: t = t[k ..< t.len]
  if t.len == 0 or t[0] == '#': return
  # bare exported type/const: `Name* = ...`, `Name* {.p.} = ...`
  var i = 0
  var name = ""
  while i < t.len and t[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
    name.add(t[i]); inc i
  if name.len > 0 and i < t.len and t[i] == '*':
    var j = i + 1
    while j < t.len and t[j] in {' ', '\t'}: inc j
    if j < t.len and t[j] in {'=', '{', '[', '.'}:
      s.declareName(name)
      # exported ref-object type: register as a class so receiver-typed
      # logic (varTypes "class:<t>", per-class method return keys) sees
      # shim types like TStringList
      if t.find("ref object") >= 0:
        s.registerClass(name, "", true)
      return
  # declaration keywords: `proc* name` / `proc name*(`
  const kws = ["proc", "func", "iterator", "template", "converter",
               "macro", "const", "var", "let"]
  for kw in kws:
    if not t.startsWith(kw): continue
    i = kw.len
    var kwStar = false
    if i < t.len and t[i] == '*':
      inc i
      kwStar = true
    while i < t.len and t[i] in {' ', '\t', '*'}:
      if t[i] == '*': kwStar = true
      inc i
    name = ""
    while i < t.len and t[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
      name.add(t[i]); inc i
    if name.len > 0:
      var exported = kwStar
      if i < t.len and t[i] == '*':
        exported = true
      if exported and name notin kws:
        s.declareName(name)
        if kw == "proc" or kw == "func":
          # method? `proc Add*(self: TStringList; ...)` - key the
          # return info per class so a user routine with the same
          # name cannot shadow the shim method's signature
          var sq = t.find("self:")
          var cty = ""
          if sq >= 0:
            var v = sq + 5
            while v < t.len and t[v] in {' ', '\t'}: inc v
            while v < t.len and t[v] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
              cty.add(t[v]); inc v
          # parameter count (top-level `;`-separated params, shim
          # style) - a 0 entry enables the paren-less call rule
          var argc = 0
          var pdepth = 0
          var sawParam = false
          var pq = i
          while pq < t.len:
            if t[pq] == '(':
              inc pdepth
            elif t[pq] == ')':
              dec pdepth
              if pdepth == 0: break
            elif pdepth == 1:
              if t[pq] == ';':
                inc argc
              elif t[pq] notin {' ', '\t'} and not sawParam:
                sawParam = true
            inc pq
          if sawParam: inc argc
          s.routineArgs[name.toLowerAscii] = argc
          # value-returning? walk the param list's parens; a `: Type`
          # after the depth-0 close means the routine returns a value
          # (needed for the statement-position discard wrapper)
          var depth = 0
          var q = i
          while q < t.len:
            if t[q] == '(':
              inc depth
            elif t[q] == ')':
              dec depth
              if depth == 0:
                var r = q + 1
                while r < t.len and t[r] in {' ', '\t'}: inc r
                if r < t.len and t[r] == ':':
                  s.returnsValue[name.toLowerAscii] = true
                  if cty.len > 0:
                    s.returnsValue[cty.toLowerAscii & "." &
                                   name.toLowerAscii] = true
                  var v = r + 1
                  while v < t.len and t[v] in {' ', '\t'}: inc v
                  var ty = ""
                  while v < t.len and t[v] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
                    ty.add(t[v])
                    inc v
                  if ty.toLowerAscii == "bool":
                    s.returnsBool[name.toLowerAscii] = true
                elif cty.len > 0:
                  # a void method: the per-class key must say false
                  s.returnsValue[cty.toLowerAscii & "." &
                                 name.toLowerAscii] = false
                break
            inc q
    break

proc absorbNimModule(p: var TParser, modpath: string) =
  ## register the exported identifiers of a bridged nimony module
  ## (`nim.std.strutils`) so call sites can use Pascal casing: the
  ## scan is a crude line-based pass over the module source, so
  ## macro-generated exports are missed (they then need the exact
  ## nimony spelling)
  var cands: seq[string] = @[]
  let dir = parentDir(p.lex.filename)
  if dir.len > 0: cands.add(dir / modpath & ".nim")
  # our own shim units: <pas2nimony>/runtime/ (bin -> pas2nimony)
  let rtDir = parentDir(parentDir(getAppFilename()))
  if rtDir.len > 0:
    cands.add(rtDir / "runtime" / modpath & ".nim")
  # the pinned toolchain checkout: <repo>/nimony/lib, anchored at the
  # compiler binary's location (pas2nimony/bin -> ../..)
  try:
    # bin/pas2nimony -> pas2nimony -> repo -> parent: the toolchain
    # checkout sits beside the repo (same layout as build.sh's
    # NIMONY=../../nimony/bin/nimony, which is relative to pas2nimony)
    let appDir = parentDir(parentDir(parentDir(parentDir(getAppFilename()))))
    if appDir.len > 0:
      cands.add(appDir / "nimony" / "lib" / modpath & ".nim")
  except Exception:
    discard
  for cand in cands:
    if fileExists(cand):
      var contents = ""
      try:
        contents = readFile(cand)
      except Exception:
        discard
      var cur = 0
      while cur < contents.len:
        var eol = cur
        while eol < contents.len and contents[eol] != '\n': inc eol
        scanNimExports(contents[cur ..< eol], p.syms)
        cur = eol + 1
      break

proc declDirective(p: var TParser): bool =
  ## tolerate compiler directives between declarations (type/const/var
  ## sections, routine local decls, uses clauses, class bodies).
  ## Conditionals evaluate at parse time, branch by branch: a taken
  ## branch's tokens flow through the enclosing loop unchanged, a dead
  ## branch is skipped at the token level, and the trailing `{$else}` /
  ## `{$endif}` tokens of an already-handled group are consumed here.
  ## Other directives (EXTERNALSYM, pragmas...) are consumed and
  ## ignored. true when the loop should continue.
  if p.tok.xkind notin {pxCurlyDirLe, pxStarDirLe}: return false
  let endMarker = succ(p.tok.xkind)
  case p.tok.ident.toLowerAscii
  of "ifdef", "ifndef", "if":
    let negate = p.tok.ident.toLowerAscii == "ifndef"
    getTokP(p)                  # skip the directive name
    var name = ""
    if p.tok.xkind == pxSymbol:
      # `{$IF DEFINED(X)}` - the only supported {$IF} form here
      if p.tok.ident.toLowerAscii == "defined":
        getTokP(p)
        p.eat(pxParLe)
        name = parseCondName(p)
        p.eat(pxParRi)
      else:
        name = parseCondName(p)
    else:
      parError(p, "identifier expected in conditional directive")
    let defined = p.syms.defines.getOrDefault(name, false)
    let taken = if negate: not defined else: defined
    p.eat(endMarker)            # closing brace
    if taken:
      # the live branch's tokens flow through the enclosing loop; its
      # trailing {$else}/{$endif} are handled by the cases below
      skipCom(p)
      return true
    if skipCondBranch(p, endMarker):
      # the dead branch ran to a {$else}: that branch is LIVE - consume
      # the directive so its tokens flow through the loop
      getTokP(p)
      p.eat(endMarker)
      skipCom(p)
    # else: skipCondBranch consumed the {$endif}
    return true
  of "else":
    # the dead alternative of a previously taken branch: consume the
    # {$else} token, then skipCondBranch runs to the {$endif}
    getTokP(p)
    p.eat(endMarker)
    discard skipCondBranch(p, endMarker)
    skipCom(p)
    return true
  of "endif":
    # the closing token of a branch whose taken side flowed here
    getTokP(p)
    p.eat(endMarker)
    skipCom(p)
    return true
  else:
    discard parseDirective(p)
    return true

proc parseUsesStmt*(p: var TParser): Node =
  result = newNodeP(nkImportStmt, p)
  getTokP(p)                  # skip `uses`
  skipCom(p)
  var any = false
  while true:
    if p.tok.xkind == pxEof: break
    skipCom(p)
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # `{$IFDEF X} Unit, {$ENDIF}` inside the uses clause
      if declDirective(p): continue
    if p.tok.xkind == pxComma:
      # comma-first continuation: `,\n  NextUnit` (Delphi style)
      getTokP(p)
      continue
    if p.tok.xkind != pxSymbol:
      parError(p, "identifier expected in uses clause")
    var unitName = p.tok.ident
    var parts = @[unitName]
    getTokP(p)
    skipCom(p)
    # dotted unit names: keep the last component (System.SysUtils -> sysutils)
    while p.tok.xkind == pxDot:
      getTokP(p)
      skipCom(p)
      if p.tok.xkind == pxSymbol:
        unitName = p.tok.ident
        parts.add(unitName)
        getTokP(p)
        skipCom(p)
      else:
        parError(p, "identifier expected after '.' in uses clause")
    if parts[0].toLowerAscii == "nim":
      # nimony-module bridge: `nim.std.strutils` (or generally
      # `nim.<package.path>.<mod>`) imports the nimony module directly.
      # Its API is used as-is, with the exact nimony spellings and no
      # Delphi fidelity promises.
      var path = ""
      for i in 1 ..< parts.len:
        if i > 1: path.add("/")
        path.add(parts[i])
      if path.len == 0:
        parError(p, "module path expected after `nim.` in uses clause")
      else:
        result.add(newIdentNode(path, p.tok.info))
        absorbNimModule(p, path)
        any = true
    else:
      case unitName.toLowerAscii
      of "strutils":
        # our Delphi-shaped shim unit (M3)
        result.add(newIdentNode("passtrutils", p.tok.info))
        absorbNimModule(p, "passtrutils")
        any = true
      of "math":
        result.add(newIdentNode("pasmath", p.tok.info))
        absorbNimModule(p, "pasmath")
        any = true
      of "dateutils":
        # Delphi DateUtils naming layer over the TDateTime core (M3)
        result.add(newIdentNode("pasdateutils", p.tok.info))
        absorbNimModule(p, "pasdateutils")
        any = true
      of "classes":
        # TStringList shim (M3); the rest of Classes is future work
        result.add(newIdentNode("pasclasses", p.tok.info))
        absorbNimModule(p, "pasclasses")
        any = true
      of "sysutils", "si_strings", "system", "windows":
        # our runtime shim (systempas) provides the Delphi RTL helpers;
        # `windows` resolves there too for the handful of API procs the
        # shim implements (GetTickCount) - the rest fails loudly at
        # semcheck, which is honest for a Linux front-end
        result.add(newIdentNode("systempas", p.tok.info))
        any = true
      else:
        # own unit: absorb its declarations, then import the module
        absorbUnit(p, unitName)
        # the import name must match the translated FILE name (Linux is
        # case-sensitive; Pascal unit/file casing may differ)
        let canonical = p.unitFiles.getOrDefault(unitName.toLowerAscii,
            p.syms.canonical(unitName))
        result.add(newIdentNode(canonical, p.tok.info))
        any = true
    # the comma is optional (comma-first continuations exist); the
    # loop head tolerates directives and further commas. `;` (and
    # Eof) end the clause - anything else falls to the loop head,
    # which either consumes a token or errors out.
    p.opt(pxComma)
    if p.tok.xkind in {pxSemiColon, pxEof}: break
    continue
  if not any:
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# no imports"

# ---------------------------------------------------------------------------
# expressions

proc getPrecedence(kind: TTokKind): int =
  case kind
  of pxDiv, pxMod, pxStar, pxSlash, pxShl, pxShr, pxAnd: result = 5
  of pxPlus, pxMinus, pxOr, pxXor: result = 4
  of pxIn, pxEquals, pxLe, pxLt, pxGe, pxGt, pxNeq, pxIs: result = 3
  of pxAs: result = 5
  else: result = -1

proc exprListAux(p: var TParser, endTok, sepTok: TTokKind, result: Node) =
  getTokP(p)
  skipCom(p)
  while true:
    if p.tok.xkind == endTok:
      getTokP(p)
      break
    if p.tok.xkind == pxEof:
      parError(p, tokKindToStr(endTok) & " expected")
      break
    var a = parseExpr(p)
    skipCom(p)
    if p.tok.xkind == pxColon:
      # Delphi write/Str width[:precision] argument: e:w[:p] is
      # lowered to pasW(e, w[, p]) and rendered by the shim
      getTokP(p)
      skipCom(p)
      let c = newNode(nkCall, a.info)
      c.add(newIdentNode("pasW", a.info))
      c.add(a)
      c.add(parseExpr(p))
      skipCom(p)
      if p.tok.xkind == pxColon:
        getTokP(p)
        skipCom(p)
        c.add(parseExpr(p))
        skipCom(p)
      a = c
    if p.tok.xkind == pxComma or p.tok.xkind == pxSemiColon:
      getTokP(p)
      skipCom(p)
    result.add(a)

proc qualifiedIdent*(p: var TParser): Node =
  if p.tok.xkind == pxSymbol:
    result = newIdentNode(p.tok.ident, p.tok.info)
  else:
    parError(p, "identifier expected, got " & $p.tok)
    return emptyNode(p.tok.info)
  getTokP(p)
  skipCom(p)
  if p.tok.xkind == pxDot:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxSymbol:
      let a = result
      result = newNode(nkDotExpr, a.info)
      result.add(a)
      result.add(newIdentNode(p.tok.ident, p.tok.info))
      getTokP(p)
    else:
      parError(p, "identifier expected after '.'")

proc rangeExpr*(p: var TParser): Node =
  let a = parseExpr(p)
  if p.tok.xkind == pxDotDot:
    result = newNodeP(nkRange, p)
    result.add(a)
    getTokP(p)
    skipCom(p)
    result.add(parseExpr(p))
  else:
    result = a

proc decIndex(p: var TParser, n: Node): Node =
  ## 1-based Delphi index -> 0-based: fold literal 1 to 0, else `idx - 1`
  if n.kind == nkIntLit and n.intVal == 1:
    result = newIntNode(nkIntLit, 0, n.info)
  else:
    result = newNode(nkInfix, n.info)
    result.add(newIdentNode("-", n.info))
    result.add(n)
    result.add(newIntNode(nkIntLit, 1, n.info))

proc finishGenericInstantiation(p: var TParser, head: Node,
    headConsumed: bool): Node =
  ## consume `<T, U>` after a generic type name -> nkIndexExpr and
  ## register the instance as a class sharing the generic's sets;
  ## `headConsumed` is false in type positions (the name is current),
  ## true in the postfix loop (the head was already consumed)
  result = newNode(nkIndexExpr, head.info)
  result.add(head)
  if not headConsumed:
    getTokP(p)
    skipCom(p)
  p.eat(pxLt)
  skipCom(p)
  inc p.genericArgDepth
  var key = head.strVal.toLowerAscii & "<"
  var spell = head.strVal & "<"
  var first = true
  while p.tok.xkind != pxGt and p.tok.xkind != pxEof:
    if not first:
      key.add(",")
      spell.add(", ")
    let t = parseTypeDesc(p, emptyNode(p.tok.info))
    result.add(t)
    if t.kind == nkIdent:
      let m = rtlSpelling(t.strVal.toLowerAscii)
      key.add(m)
      spell.add(m)
    else:
      key.add("?")
      spell.add("?")
    first = false
    if p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
  key.add(">")
  spell.add(">")
  dec p.genericArgDepth
  p.eat(pxGt)
  skipCom(p)
  p.syms.registerSpecializedAlias(head.strVal, spell)

proc parseGenericParams(p: var TParser): Node =
  ## `<K, V: class, constructor>` -> nkBracket [K, V]; the
  ## constraint list after a param's colon is dropped (v1)
  result = newNode(nkBracket, p.tok.info)
  getTokP(p)
  skipCom(p)
  p.curTypeParams = @[]
  while p.tok.xkind != pxGt and p.tok.xkind != pxEof:
    if p.tok.xkind != pxSymbol:
      parError(p, "type parameter name expected, got " & $p.tok)
    let nm = p.tok.ident
    p.curTypeParams.add(nm)
    result.add(newIdentNode(nm, p.tok.info))
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxColon:
      # drop the constraint: scan to the next comma/semicolon/angle-close
      while p.tok.xkind != pxEof and
          p.tok.xkind notin {pxComma, pxSemiColon, pxGt}:
        getTokP(p)
        skipCom(p)
    if p.tok.xkind == pxComma or p.tok.xkind == pxSemiColon:
      getTokP(p)
      skipCom(p)
  p.eat(pxGt)
  skipCom(p)

proc mappedTypeName(p: var TParser, ty: Node): string =
  ## mapped spelling of a simple type node; "" when unknown/complex
  result = ""
  var t = ty
  if t.kind == nkVarTy and t.len > 0: t = t[0]
  if t.kind == nkIdent:
    result = rtlSpelling(t.strVal.toLowerAscii)
  elif t.kind == nkIndexExpr and t.len > 1 and t[0].kind == nkIdent:
    result = t[0].strVal.toLowerAscii & "<"
    for i in 1 ..< t.len:
      if i > 1: result.add(",")
      if t[i].kind == nkIdent:
        result.add(rtlSpelling(t[i].strVal.toLowerAscii))
      else:
        result.add("?")
    result.add(">")

proc stringBaseType(p: var TParser, base: Node): string =
  ## resolved type spelling of a simply-indexed base (var, param, field);
  ## "" when the base is not tracked
  result = ""
  case base.kind
  of nkIdent:
    let k = base.strVal.toLowerAscii
    result = p.paramTypes.getOrDefault(k)
    if result.len == 0:
      result = p.varTypes.getOrDefault(k)
    if result.len == 0 and p.selfClass.len > 0:
      # bare field access inside a method body (`acc[i]` means self.acc[i])
      result = p.fieldTypes.getOrDefault(p.selfClass.toLowerAscii & "." & k)
  of nkDotExpr:
    if base[0].kind == nkIdent and base[1].kind == nkIdent:
      result = p.fieldTypes.getOrDefault(
        base[0].strVal.toLowerAscii & "." & base[1].strVal.toLowerAscii)
  else:
    discard

proc bracketExprList(p: var TParser, first: Node): Node =
  result = newNode(nkIndexExpr, first.info)
  result.add(first)
  getTokP(p)
  skipCom(p)
  while true:
    if p.tok.xkind == pxBracketRi:
      getTokP(p)
      break
    if p.tok.xkind == pxEof:
      parError(p, "] expected")
      break
    let a = rangeExpr(p)
    skipCom(p)
    if p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
    result.add(a)
  # Delphi strings are 1-based; nimony's `string` is 0-based
  if result.len == 2 and stringBaseType(p, result[0]) == "string":
    result[1] = decIndex(p, result[1])

proc identOrLiteral(p: var TParser): Node =
  case p.tok.xkind
  of pxSymbol:
    result = newIdentNode(p.tok.ident, p.tok.info)
    getTokP(p)
  of pxIntLit:
    result = newIntNode(nkIntLit, p.tok.iNumber, p.tok.info)
    getTokP(p)
  of pxInt64Lit:
    result = newIntNode(nkInt64Lit, p.tok.iNumber, p.tok.info)
    getTokP(p)
  of pxFloatLit:
    result = newFloatNode(p.tok.fNumber, p.tok.info)
    getTokP(p)
  of pxStrLit:
    if p.tok.literal.len != 1:
      result = newStrNode(p.tok.literal, p.tok.info)
    else:
      result = newCharNode(p.tok.literal, p.tok.info)
    getTokP(p)
  of pxNil:
    result = newNode(nkNilLit, p.tok.info)
    getTokP(p)
  of pxParLe:
    # () constructor; array constructor if no `key: value` pairs
    result = newNodeP(nkPar, p)
    getTokP(p)
    skipCom(p)
    var hasColon = false
    while p.tok.xkind != pxParRi and p.tok.xkind != pxEof:
      let a = parseExpr(p)
      skipCom(p)
      if p.tok.xkind == pxColon:
        hasColon = true
        getTokP(p)
        skipCom(p)
        let b = parseExpr(p)
        let pair = newNode(nkCall, a.info)
        pair.add(newIdentNode("kv", a.info))
        pair.add(a)
        pair.add(b)
        result.add(pair)
      else:
        result.add(a)
      if p.tok.xkind == pxComma:
        getTokP(p)
        skipCom(p)
    p.eat(pxParRi)
    if not hasColon and result.len > 1:
      result.kind = nkBracket   # array constructor
  of pxBracketLe:
    # [] constructor; a set literal when a range is involved
    result = newNodeP(nkBracket, p)
    getTokP(p)
    skipCom(p)
    while p.tok.xkind != pxBracketRi and p.tok.xkind != pxEof:
      let a = rangeExpr(p)
      if a.kind == nkRange:
        result.kind = nkCurly   # definitely a set literal
      p.opt(pxComma)
      skipCom(p)
      result.add(a)
    p.eat(pxBracketRi)
  else:
    parError(p, "expression expected, got " & $p.tok)
    getTokP(p)
    result = emptyNode(p.tok.info)

proc parseAnonymousMethod(p: var TParser): Node =
  ## Delphi anonymous method: `procedure(X: Integer) begin ... end`
  ## -> lambda (nkProcDef with an empty name); nimony lowers it to a
  ## closure and captures outer variables by reference
  let info = p.tok.info
  getTokP(p)                    # procedure/function
  skipCom(p)
  let params = p.parseParamList()
  p.opt(pxSemiColon)
  skipCom(p)
  if p.tok.xkind == pxColon:
    getTokP(p)
    skipCom(p)
    params[0] = parseTypeDesc(p, emptyNode(p.tok.info))
    skipCom(p)
  p.eat(pxBegin)
  skipCom(p)
  let body = newNodeP(nkStmtList, p)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    let s = parseStmt(p)
    if s.kind != nkEmpty: body.add(s)
    if p.tok.xkind == pxSemiColon:
      getTokP(p)
      skipCom(p)
  p.eat(pxEnd)
  skipCom(p)
  var def = newNode(nkProcDef, info)
  def.add(emptyNode(info))      # anonymous: no name
  def.add(emptyNode(info))      # typevars
  var np = newNode(nkFormalParams, info)
  np.add(params[0])             # return type slot
  for i in 1 ..< params.len:
    np.add(params[i])
  def.add(np)
  def.add(emptyNode(info))      # pragmas
  def.add(emptyNode(info))      # exceptions
  def.add(body)
  result = def

proc primary(p: var TParser): Node =
  # prefix operators
  if p.tok.xkind in {pxNot, pxMinus, pxPlus}:
    result = newNodeP(nkPrefix, p)
    result.add(newIdentNode($p.tok, p.tok.info))
    getTokP(p)
    result.add(primary(p))
    return
  elif p.tok.xkind == pxAt:
    result = newNodeP(nkAddr, p)
    getTokP(p)
    result.add(primary(p))
    return
  elif p.tok.xkind in {pxProcedure, pxFunction}:
    # Delphi anonymous method literal
    return parseAnonymousMethod(p)
  elif p.tok.xkind == pxInherited:
    # `Result := inherited Add(x)` - the value form; the statement
    # form's discard wrapper must be unwrapped
    result = parseInherited(p)
    if result.kind == nkDiscardStmt and result.len > 0:
      result = result[0]
    if result.kind == nkEmpty:
      # v1: an inherited value against a no-op root (parentless) or an
      # unknown shim parent has no real target; lower to 0 - the
      # parseInherited side already warned for the shim-gap case
      result = newNode(nkIntLit, result.info)
      result.intVal = BiggestInt(0)
    return
  result = p.withQualify(identOrLiteral(p))
  while true:
    case p.tok.xkind
    of pxParLe:
      let a = result
      result = newNode(nkCall, a.info)
      result.add(a)
      exprListAux(p, pxParRi, pxEquals, result)
      # Delphi class operator Explicit: a type-cast call over a
      # record argument whose type registered an Explicit to this
      # target type lowers to the conversion proc
      if result.len == 2 and result[0].kind == nkIdent and
          result[1].kind == nkIdent:
        let argVt = p.varTypes.getOrDefault(
            result[1].strVal.toLowerAscii)
        if argVt.startsWith("record:"):
          let rcls = argVt[7 .. ^1]
          let targetKey = result[0].strVal.toLowerAscii
          let op = p.syms.getConvOp(rcls, "explicit", rcls, targetKey)
          if op.len > 0:
            result[0] = newIdentNode(op, result[0].info)
      # a 1-char Pascal literal passed to a callee is a string in
      # almost every signature; char-arg procs keep the char
      if result.len > 1 and not (a.kind == nkIdent and
          a.strVal.toLowerAscii in ["stringofchar", "ord", "chr"]):
        for ai in 1 ..< result.len:
          if result[ai].kind == nkCharLit:
            let sd = newNode(nkStrLit, result[ai].info)
            sd.strVal = result[ai].strVal
            result[ai] = sd
      if a.kind == nkIdent and a.strVal in p.nestedProcs:
        # nested routines see `self` implicitly
        result.add(newIdentNode("self", a.info))
      result = mapStringBuiltins(p, result)
      result = lowerFormatArrayOfConst(p, result)
      result = p.rewriteMethodPtrCall(result)
    of pxDot:
      let a = result
      result = newNode(nkDotExpr, a.info)
      # a unit-qualified name (`DateUtils.Now`) must spell the module
      # the way the uses clause imported it
      if a.kind == nkIdent:
        let mapped = p.unitModuleSpelling(a.strVal)
        if mapped == a.strVal:
          result.add(a)
        else:
          result.add(newIdentNode(mapped, a.info))
      else:
        result.add(a)
      getTokP(p)               # skip '.'
      skipCom(p)
      if p.tok.xkind == pxSymbol:
        result.add(newIdentNode(p.tok.ident, p.tok.info))
        getTokP(p)
      else:
        parError(p, "identifier expected after '.'")
    of pxLt:
      # generic instantiation in an expression: `TFoo<int>.Create`;
      # only when the head is a declared generic type, which
      # disambiguates from the `<` operator
      if result.kind == nkIdent and p.syms.isGenericClass(result.strVal):
        result = finishGenericInstantiation(p, result, true)
      else:
        break
    of pxHat:
      let a = result
      result = newNode(nkDeref, a.info)
      result.add(a)
      getTokP(p)
    of pxBracketLe:
      result = bracketExprList(p, result)
    else: break
  # Delphi calls parameterless functions without parentheses in
  # expression position (`d1 := Now`); a known 0-arg routine that was
  # not followed by `(` or a procedural context becomes a call. A
  # variable of the same name shadows the routine (locals resolve
  # first in Delphi).
  if result.kind == nkIdent or (result.kind == nkDotExpr and
      result.len == 2 and result[1].kind == nkIdent):
    var callee = ""
    if result.kind == nkIdent:
      callee = result.strVal
    else:
      callee = result[1].strVal
    if callee.len > 0 and
        p.syms.routineArgs.getOrDefault(callee.toLowerAscii, -1) == 0 and
        not p.varTypes.hasKey(callee.toLowerAscii):
      let c = newNode(nkCall, result.info)
      c.add(result)
      result = c

proc inSetLiteralElems(n: Node): bool =
  ## every element of the set constructor is an ordinal literal or a
  ## range over ordinal literals
  result = true
  for s in n.sons:
    case s.kind
    of nkIntLit, nkCharLit: discard
    of nkPrefix:
      result = result and s.len == 2 and s[1].kind in {nkIntLit, nkCharLit}
    of nkRange:
      for r in s.sons:
        if r.kind notin {nkIntLit, nkCharLit} and
            not (r.kind == nkPrefix and r.len == 2 and
                r[1].kind in {nkIntLit, nkCharLit}):
          return false
    else: return false

proc buildInComparisons(p: var TParser, x, setN: Node): Node =
  ## `x in [a..b, c, ...]` with an all-literal set lowers to a
  ## comparison chain - avoids nimony's set typing (a set over int
  ## literals would be set[int] and int is too wide for a set element)
  result = emptyNode(setN.info)
  if setN.len == 0:
    result = newIdentNode("false", setN.info)
    return
  var acc = emptyNode(setN.info)
  for s in setN.sons:
    var one = emptyNode(s.info)
    if s.kind == nkRange:
      one = newNodeP(nkInfix, p)
      let ge = newNodeP(nkInfix, p)
      ge.add(newIdentNode(">=", s.info))
      ge.add(x)
      ge.add(s[0])
      let le = newNodeP(nkInfix, p)
      le.add(newIdentNode("<=", s.info))
      le.add(x)
      le.add(s[1])
      let pg = newNode(nkPar, s.info)
      pg.add(ge)
      let pl = newNode(nkPar, s.info)
      pl.add(le)
      one.add(newIdentNode("and", s.info))
      one.add(pg)
      one.add(pl)
    else:
      one = newNodeP(nkInfix, p)
      one.add(newIdentNode("==", s.info))
      one.add(x)
      one.add(s)
    if acc.kind == nkEmpty:
      acc = one
    else:
      let o = newNodeP(nkInfix, p)
      o.add(newIdentNode("or", s.info))
      let pa = newNode(nkPar, acc.info)
      pa.add(acc)
      let pb = newNode(nkPar, s.info)
      pb.add(one)
      o.add(pa)
      o.add(pb)
      acc = o
  result = acc

proc lowestExprAux(p: var TParser, v: var Node, limit: int): TTokKind =
  v = primary(p)
  var op = p.tok.xkind
  var opPred = getPrecedence(op)
  if p.context == conTypeDesc and op == pxEquals:
    # in a type-desc context `=` never starts an infix: `T = default`
    # belongs to the const/param declaration, not the type
    result = op
    return
  if p.genericArgDepth > 0 and op == pxGt:
    # inside `<...>` the `>` closes the bracket; it is not the
    # greater-than operator
    result = op
    return
  while opPred > limit:
    let node = newNodeP(nkInfix, p)
    let opNode = newIdentNode($p.tok, p.tok.info)
    getTokP(p)
    case op
    of pxEquals:
      opNode.strVal = "=="
    of pxNeq:
      opNode.strVal = "!="
    of pxShr:
      # FPC/Delphi shr is LOGICAL (zero fill) in the operand's own
      # width; nimony's shr is arithmetic - shift the unsigned twin at
      # the declared width, then restore the Pascal result type. The
      # delphiShr shim remains the fallback for untyped operands.
      skipCom(p)
      var v2s = emptyNode(p.tok.info)
      discard lowestExprAux(p, v2s, opPred)
      let lhsTy = rhsExprType(p, v)
      if lhsTy in ["int8", "uint8", "int16", "uint16", "int32",
                   "uint32", "int64", "uint64"]:
        let uw = if lhsTy.startsWith("u"): lhsTy
                 else: "u" & lhsTy
        let inner = newNode(nkInfix, node.info)
        inner.add(newIdentNode("shr", node.info))
        let cv = newNode(nkCall, node.info)
        cv.add(newIdentNode(uw, node.info))
        cv.add(v)
        let cw = newNode(nkCall, v2s.info)
        cw.add(newIdentNode(uw, v2s.info))
        cw.add(v2s)
        inner.add(cv)
        inner.add(cw)
        if lhsTy == uw:
          v = inner
        else:
          let outer = newNode(nkCall, node.info)
          outer.add(newIdentNode(lhsTy, node.info))
          outer.add(inner)
          v = outer
      else:
        node.kind = nkCall
        node.sons = @[]
        node.add(newIdentNode("delphiShr", node.info))
        node.add(v)
        node.add(v2s)
        v = node
      op = p.tok.xkind
      opPred = getPrecedence(op)
      continue
    else:
      discard
    skipCom(p)
    var v2 = emptyNode(p.tok.info)
    let nextop = lowestExprAux(p, v2, opPred)
    if op == pxIs:
      # Delphi `x is T`: runtime subclass test; nil belongs to no class
      # (nimony's `of` answers true for nil, hence the guard)
      let info = node.info
      node.sons = @[]
      node.add(newIdentNode("and", info))
      let neNil = newNode(nkInfix, info)
      neNil.add(newIdentNode("!=", info))
      neNil.add(v)
      neNil.add(newIdentNode("nil", info))
      let lhs = newNode(nkPar, info)
      lhs.add(neNil)
      let ofChk = newNode(nkInfix, info)
      ofChk.add(newIdentNode("of", info))
      ofChk.add(v)
      ofChk.add(v2)
      let rhs = newNode(nkPar, info)
      rhs.add(ofChk)
      node.add(lhs)
      node.add(rhs)
    elif op == pxAs:
      # Delphi `x as T`: checked cast via systempas.pasAs - nil stays
      # nil, a failed check yields nil (the raising variant would mark
      # every transitive caller {.raises.}; documented divergence)
      let info = node.info
      node.kind = nkCall
      node.sons = @[]
      let callee = newNode(nkIndexExpr, info)
      callee.add(newIdentNode("pasAs", info))
      callee.add(v2)
      node.add(callee)
      node.add(v)
    else:
      if op == pxSlash:
        # Pascal `/` is real division; nimony's `/` accepts floats
        # only - convert non-float operands explicitly
        if rhsExprType(p, v) notin ["float32", "float64"]:
          let c = newNode(nkCall, v.info)
          c.add(newIdentNode("float64", v.info))
          c.add(v)
          v = c
      if op == pxIn and v2.kind == nkCurly and inSetLiteralElems(v2):
        # membership over a literal set -> comparisons (no set typing)
        v = buildInComparisons(p, v, v2)
        op = nextop
        opPred = getPrecedence(nextop)
        continue
      node.add(opNode)
      node.add(v)
      if op == pxSlash:
        if rhsExprType(p, v2) notin ["float32", "float64"]:
          let c = newNode(nkCall, v2.info)
          c.add(newIdentNode("float64", v2.info))
          c.add(v2)
          v2 = c
      node.add(v2)
    v = p.rewriteMethodPtrNilCmp(node)
    op = nextop
    opPred = getPrecedence(nextop)
  result = op

proc parseExpr*(p: var TParser): Node =
  let oldcontext = p.context
  if p.context != conTypeDesc:
    # a type-desc context must survive into sub-expressions: `T = v`
    # in const/param declarations belongs to the declaration
    p.context = conExpr
  if p.tok.xkind == pxCommand:
    result = parseDirective(p)
  else:
    var v = emptyNode(p.tok.info)
    discard lowestExprAux(p, v, -1)
    result = v
  p.context = oldcontext

proc parseExprStmt*(p: var TParser): Node =
  let info = parLineInfo(p)
  let a = parseExpr(p)
  if p.tok.xkind == pxAsgn:
    getTokP(p)
    skipCom(p)
    let b = parseExpr(p)
    result = newNode(nkAsgn, info)
    result.add(a)
    result.add(b)
  else:
    result = a
# ---------------------------------------------------------------------------
# types

proc parseEnum*(p: var TParser): Node =
  # (a, b, c) or (a = 1, b = 5)
  result = newNodeP(nkEnumTy, p)
  getTokP(p)                    # skip (
  skipCom(p)
  while p.tok.xkind != pxParRi and p.tok.xkind != pxEof:
    if p.tok.xkind != pxSymbol:
      parError(p, "identifier expected in enum, got " & $p.tok)
    let fieldName = p.tok.ident
    let fieldInfo = p.tok.info
    getTokP(p)
    skipCom(p)
    p.syms.declareName(fieldName)
    if p.tok.xkind == pxEquals:
      getTokP(p)
      skipCom(p)
      let val = parseExpr(p)
      let fld = newNode(nkEnumFieldDef, fieldInfo)
      fld.add(newIdentNode(fieldName, fieldInfo))
      fld.add(val)
      result.add(fld)
    else:
      result.add(newIdentNode(fieldName, fieldInfo))
    p.opt(pxComma)
    skipCom(p)
  p.eat(pxParRi)

proc parseRecordCase*(p: var TParser): Node =
  # `case FTag: Integer of ...` inside a record/object. Variant
  # records overlay storage in Delphi; v1 renders ALL branches as
  # plain fields (flat layout) - typical use writes one branch and
  # reads it back, which behaves identically. The result is an
  # nkRecList so both renderers stay uniform.
  result = newNodeP(nkRecList, p)
  var caseFields = newNodeP(nkRecCase, p)
  getTokP(p)                    # skip `case`
  skipCom(p)
  # discriminant: `name: Type` or the tagless `Type of` form
  if p.tok.xkind != pxSymbol:
    parError(p, "identifier expected for discriminant")
  var discName = p.tok.ident
  let first = p.tok.ident
  let discInfo = p.tok.info
  getTokP(p)
  var discTy: Node
  if p.tok.xkind == pxColon:
    # named discriminant: the symbol was the name
    getTokP(p)
    p.syms.declareName(discName)
    discTy = parseTypeDesc(p, emptyNode(p.tok.info))
  else:
    # `case Integer of` - tagless variant record; the symbol was the
    # type. The record renders flat, so a synthetic discriminant
    # keeps the node shape uniform (v1: ident types only)
    discName = "pasVariantTag"
    p.syms.declareName(discName)
    discTy = newIdentNode(first, discInfo)
  let disc = newNode(nkIdentDefs, discInfo)
  disc.add(newIdentNode(discName, discInfo))
  disc.add(discTy)
  disc.add(emptyNode(discInfo))
  result.add(disc)
  p.eat(pxOf)
  skipCom(p)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    var branch: Node
    if p.tok.xkind == pxElse:
      branch = newNodeP(nkElse, p)
      getTokP(p)
    else:
      branch = newNodeP(nkOfBranch, p)
      while p.tok.xkind != pxEof and p.tok.xkind != pxColon:
        branch.add(rangeExpr(p))
        p.opt(pxComma)
        skipCom(p)
      p.eat(pxColon)
    skipCom(p)
    # fields of this branch: `a, b: Typ;` or a nested `()`
    if p.tok.xkind == pxParLe:
      # nested case in parens - parse fields recursively
      getTokP(p)
      let body = newNode(nkRecList, p.tok.info)
      while p.tok.xkind != pxParRi and p.tok.xkind != pxEof:
        let defs = parseIdentColonEquals(p, false)
        body.add(defs)
        p.opt(pxSemiColon)
        skipCom(p)
      p.eat(pxParRi)
      branch.add(body)
    else:
      let body = parseIdentColonEquals(p, false)
      branch.add(body)
    caseFields.add(branch)
    p.opt(pxSemiColon)
    skipCom(p)
  # flatten: every branch's fields are appended unconditionally
  # (the discriminant itself was already added as a plain field)
  for i in 0 ..< caseFields.len:
    let b = caseFields[i]
    for j in 0 ..< b[b.len - 1].len:
      result.add(b[b.len - 1][j])

proc genSelfType(p: var TParser): Node =
  if p.selfClass.len > 0:
    let ci = p.syms.classes.getOrDefault(p.selfClass.toLowerAscii)
    if ci.typeParams.len > 0:
      result = newNode(nkIndexExpr, p.tok.info)
      result.add(newIdentNode(p.selfClass, p.tok.info))
      for t in ci.typeParams:
        result.add(newIdentNode(t, p.tok.info))
    else:
      result = newIdentNode(p.selfClass, p.tok.info)
  else:
    result = emptyNode(p.tok.info)

proc genSelfParam(p: var TParser; isVar: bool): Node =
  ## `self: MyClass` or `self: var MyClass`
  let d = newNodeP(nkIdentDefs, p)
  d.add(newIdentNameNodeP("self", p))
  if isVar:
    let vt = newNodeP(nkVarTy, p)
    vt.add(genSelfType(p))
    d.add(vt)
  else:
    d.add(genSelfType(p))
  d.add(emptyNode(p.tok.info))
  result = d

proc parseParamList*(p: var TParser): Node =
  ## returns nkFormalParams; may be empty (no parens)
  result = newNodeP(nkFormalParams, p)
  result.add(emptyNode(p.tok.info))  # return type at position 0
  if p.tok.xkind == pxParLe:
    getTokP(p)
    skipCom(p)
    p.inParamList = true
    while p.tok.xkind != pxParRi and p.tok.xkind != pxEof:
      var isVar = false
      if p.tok.xkind == pxVar:
        isVar = true
        getTokP(p)
      elif p.tok.xkind == pxOut:
        # Delphi `out` params: v1 lowers them like `var` - the callee
        # sees the caller's variable; initialization semantics
        # (callee must not read before writing) are not modeled
        isVar = true
        getTokP(p)
      elif p.tok.xkind == pxConst:
        # treat `const` params as plain params; the mutability distinction
        # does not matter for the generated code
        getTokP(p)
      skipCom(p)
      # names
      var names: seq[string] = @[]
      while true:
        if p.tok.xkind != pxSymbol:
          parError(p, "identifier expected in params, got " & $p.tok)
        names.add(p.tok.ident)
        getTokP(p)
        skipCom(p)
        if p.tok.xkind == pxComma:
          getTokP(p)
          skipCom(p)
        else:
          break
      var lastType = emptyNode(p.tok.info)
      if p.tok.xkind == pxColon:
        getTokP(p)
        skipCom(p)
        lastType = parseTypeDesc(p, emptyNode(p.tok.info))
        skipCom(p)
      # optional default value
      var def = emptyNode(p.tok.info)
      if p.tok.xkind == pxEquals:
        getTokP(p)
        skipCom(p)
        def = parseExpr(p)
        skipCom(p)
        if def.kind == nkCharLit:
          # nimony defaults must match the declared type: a 1-char
          # Pascal literal is a char, string params need a string
          let sd = newNode(nkStrLit, def.info)
          sd.strVal = def.strVal
          def = sd
      for n in names:
        p.syms.declareName(n)
        if lastType.kind == nkIdent:
          let pty = lastType.strVal.toLowerAscii
          if p.recordTypes.hasKey(pty):
            p.varTypes[n.toLowerAscii] = "record:" & lastType.strVal
          else:
            let rtl = rtlSpelling(pty)
            if rtl.len > 0:
              p.varTypes[n.toLowerAscii] = rtl
        let d = newNode(nkIdentDefs, p.tok.info)
        d.add(newIdentNode(n, p.tok.info))
        if isVar:
          let vt = newNode(nkVarTy, p.tok.info)
          if lastType.kind != nkEmpty: vt.add(lastType)
          else: vt.add(newIdentNode("untyped", p.tok.info))
          d.add(vt)
        elif lastType.kind != nkEmpty:
          # Delphi value params are locally mutable; a method-pointer
          # param's evProc/evObj fields are assigned in the body, so
          # lower them as var params
          if lastType.kind == nkIdent and
              p.methodPtrTypes.hasKey(lastType.strVal.toLowerAscii):
            let vt = newNode(nkVarTy, p.tok.info)
            vt.add(lastType)
            d.add(vt)
          else:
            d.add(lastType)
        else:
          d.add(newIdentNode("untyped", p.tok.info))
        if def.kind != nkEmpty: d.add(def) else: d.add(emptyNode(p.tok.info))
        result.add(d)
      if p.tok.xkind == pxSemiColon:
        getTokP(p)
        skipCom(p)
    p.inParamList = false
    p.eat(pxParRi)
    skipCom(p)

proc parseRoutineType*(p: var TParser): Node =
  # `procedure of object` / `function(x: int): int of object`
  result = newNodeP(nkProcTy, p)
  getTokP(p)                    # skip procedure/function
  skipCom(p)
  let params = p.parseParamList()
  result.add(params)
  skipCom(p)
  var isClosure = false
  # `of object` closure marker
  if p.tok.xkind == pxOf:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxObject:
      isClosure = true
      getTokP(p)
      skipCom(p)
    else:
      parError(p, "object expected after `of` in procedure type")
  # return type
  if p.tok.xkind == pxColon:
    getTokP(p)
    skipCom(p)
    let ret = parseTypeDesc(p, emptyNode(p.tok.info))
    # the return type lives in the params' slot 0 (the procTy's own
    # son 0 is the params); overwriting it broke 2-son invariants
    result[0][0] = ret
  if isClosure:
    let pragmas = newNode(nkPragma, p.tok.info)
    pragmas.add(newIdentNode("closure", p.tok.info))
    result.add(pragmas)
  else:
    result.add(emptyNode(p.tok.info))

# ---------------------------------------------------------------------------
# declarations

proc parseIdentColonEquals*(p: var TParser; withVis: bool): Node =
  ## `a, b: Type = init;`  (var/field declaration group)
  result = newNodeP(nkIdentDefs, p)
  let exportNames = p.section == seInterface and p.visibility != visPrivate
  while true:
    if p.tok.xkind != pxSymbol:
      parError(p, "identifier expected, got " & $p.tok)
    p.syms.declareName(p.tok.ident)
    result.add(exSymbol(newIdentNode(p.tok.ident, p.tok.info), exportNames))
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
    else:
      break
  if p.tok.xkind == pxColon:
    getTokP(p)
    skipCom(p)
    result.add(parseTypeDesc(p, emptyNode(p.tok.info)))
    skipCom(p)
  else:
    result.add(emptyNode(p.tok.info))
  if p.tok.xkind == pxEquals:
    # Delphi var/field default: `var X: T = init;`
    getTokP(p)
    skipCom(p)
    result.add(parseExpr(p))
  else:
    result.add(emptyNode(p.tok.info))

proc parseVarSection*(p: var TParser): Node =
  result = newNodeP(nkVarSection, p)
  getTokP(p)                    # skip var/threadvar
  skipCom(p)
  while true:
    skipCom(p)                  # comments between definitions
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # conditionals + directives between var declarations
      if declDirective(p):
        continue
      break
    if p.tok.xkind != pxSymbol:
      break
    let defs = parseIdentColonEquals(p, false)
    skipCom(p)
    result.add(defs)
    p.opt(pxSemiColon)
    skipCom(p)
    # remember the var types: for set literals, class casts and the
    # typed for-loop bounds
    let tyNode = defs[defs.len - 2]
    if tyNode.kind == nkSetTy:
      for i in 0 ..< defs.len - 2:
        if defs[i].kind == nkIdent:
          p.varTypes[defs[i].strVal.toLowerAscii] = "set"
    elif tyNode.kind == nkIdent and
        tyNode.strVal.toLowerAscii in ["boolean", "bool"]:
      for i in 0 ..< defs.len - 2:
        if defs[i].kind == nkIdent:
          p.varTypes[defs[i].strVal.toLowerAscii] = "bool"
    elif tyNode.kind == nkArrayTy and tyNode.len > 0 and
        tyNode[0].kind == nkRange and tyNode[0].len == 2 and
        tyNode[0][0].kind in {nkIntLit, nkInt64Lit}:
      # Pascal arrays keep their declared low bound; nimony's runtime
      # index check assumes 0-based storage, so index accesses must be
      # offset (see selfQualifyInPlace)
      let low = tyNode[0][0].intVal
      for i in 0 ..< defs.len - 2:
        if defs[i].kind == nkIdent:
          p.arrayLows[defs[i].strVal.toLowerAscii] = int(low)
    elif tyNode.kind == nkIdent:
      let tyKey = tyNode.strVal.toLowerAscii
      if p.arrayTypeLows.hasKey(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.arrayLows[defs[i].strVal.toLowerAscii] =
              p.arrayTypeLows.getOrDefault(tyKey)
      if p.methodPtrTypes.hasKey(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.methodPtrVars[defs[i].strVal.toLowerAscii] =
              p.methodPtrTypes.getOrDefault(tyKey)
      if p.syms.isClass(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.varTypes[defs[i].strVal.toLowerAscii] = "class:" & tyKey
      elif p.recordTypes.hasKey(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            # record vars carry their spelling for `with` lowering
            p.varTypes[defs[i].strVal.toLowerAscii] = "record:" & tyNode.strVal
      else:
        let mapped = rtlSpelling(tyKey)
        if mapped.len > 0:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.varTypes[defs[i].strVal.toLowerAscii] = mapped

proc parseConstSection*(p: var TParser): Node =
  result = newNodeP(nkConstSection, p)
  getTokP(p)                    # skip const/resourcestring
  skipCom(p)
  while true:
    skipCom(p)                  # comments between definitions
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # conditionals + directives between const definitions
      if declDirective(p):
        continue
      break
    if p.tok.xkind != pxSymbol:
      break
    let info = p.tok.info
    let name = p.tok.ident
    getTokP(p)
    skipCom(p)
    p.syms.declareName(name)
    let def = newNode(nkIdentDefs, info)
    def.add(newIdentNode(name, info))
    if p.tok.xkind == pxColon:
      getTokP(p)
      skipCom(p)
      def.add(parseTypeDesc(p, emptyNode(p.tok.info)))
      skipCom(p)
    else:
      def.add(emptyNode(info))
    p.eat(pxEquals)
    skipCom(p)
    def.add(parseExpr(p))
    result.add(def)
    p.opt(pxSemiColon)
    skipCom(p)

proc parseInterfaceType(p: var TParser, definition: Node): Node =
  ## `IFoo = interface [(IBase)] ... end` - a Delphi interface lowered
  ## to an abstract ref class whose methods are nimony `method`s with
  ## discard bodies (nimony's dynamic dispatch plays the vtable).
  ## v1: methods only, single inheritance, no properties.
  let info = definition.info
  getTokP(p)                    # skip `interface`
  skipCom(p)
  var parent = ""
  var parentTy: Node = emptyNode(info)
  if p.tok.xkind == pxParLe:
    getTokP(p)
    skipCom(p)
    parentTy = parseTypeDesc(p, emptyNode(p.tok.info))
    if parentTy.kind == nkIdent:
      parent = parentTy.strVal
    p.eat(pxParRi)
    skipCom(p)
  let defName = definition.strVal
  var res = newNode(nkRefTy, info)
  var record = newNode(nkObjectTy, info)
  res.add(record)
  let ofInh = newNode(nkOfInherit, info)
  if parent.len > 0:
    ofInh.add(parentTy)
  else:
    ofInh.add(newIdentNode("RootRef", info))
  record.add(ofInh)
  record.add(newNode(nkRecList, info))
  p.syms.registerClass(defName, parent, true)
  var ci = p.syms.classes.getOrDefault(defName.toLowerAscii)
  ci.isInterface = true
  p.syms.classes[defName.toLowerAscii] = ci
  # body: bodiless method declarations (COM plumbing skipped below)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    if p.tok.xkind == pxComment:
      skipCom(p)
      continue
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # directives between interface members ({$EXTERNALSYM ...},
      # conditionals) + the GUID bracket member
      if declDirective(p):
        continue
      break
    if p.tok.xkind == pxBracketLe:
      # COM interface GUID member: `['{...}']` - consumed
      getTokP(p)
      while p.tok.xkind != pxBracketRi and p.tok.xkind != pxEof:
        getTokP(p)
      p.eat(pxBracketRi)
      p.opt(pxSemiColon)
      skipCom(p)
      continue
    if p.tok.xkind == pxProperty:
      # COM interface properties are accessors plumbing; v1 skips
      # them (method calls through the interface still lower)
      discard parseProperty(p)
      p.opt(pxSemiColon)
      skipCom(p)
      continue
    if p.tok.xkind == pxSymbol:
      # visibility section words: skip
      getTokP(p)
      skipCom(p)
      p.opt(pxSemiColon)
      skipCom(p)
      continue
    if p.tok.xkind in {pxProcedure, pxFunction}:
      let kind = p.tok.xkind
      getTokP(p)
      skipCom(p)
      if p.tok.xkind != pxSymbol:
        parError(p, "routine name expected in interface body")
      let name = p.tok.ident
      let nameInfo = p.tok.info
      getTokP(p)
      skipCom(p)
      let fp = parseParamList(p)
      skipCom(p)
      if p.tok.xkind == pxColon:
        getTokP(p)
        skipCom(p)
        fp[0] = parseTypeDesc(p, emptyNode(p.tok.info))
        skipCom(p)
      if kind == pxFunction:
        p.syms.returnsValue[name.toLowerAscii] = true
      p.syms.addRoutine(defName, name)
      p.syms.addMethod(defName, name)   # makes implementations virtual
      let dinfo = nameInfo
      var def = newNode(nkMethodDef, dinfo)
      def.add(exSymbol(newIdentNode(name, dinfo),
                       p.visibility != visPrivate))
      def.add(emptyNode(dinfo))
      var nfp = newNode(nkFormalParams, dinfo)
      nfp.add(fp[0])                     # return type slot
      var sd = newNode(nkIdentDefs, dinfo)
      sd.add(newIdentNode("self", dinfo))
      sd.add(newIdentNode(defName, dinfo))
      sd.add(emptyNode(dinfo))
      nfp.add(sd)
      for j in 1 ..< fp.len: nfp.add(fp[j])
      def.add(nfp)
      def.add(emptyNode(dinfo))
      def.add(emptyNode(dinfo))
      var body = newNode(nkStmtList, dinfo)
      if fp[0].kind != nkEmpty:
        # function interface method: nimony's result-init proof
        # demands an initialized result in the dispatch root
        var asgn = newNode(nkAsgn, dinfo)
        asgn.add(newIdentNode("result", dinfo))
        # `default(T)`: a plain call whose first son is the type
        var dcall = newNode(nkCall, dinfo)
        dcall.add(newIdentNode("default", dinfo))
        dcall.add(fp[0])
        asgn.add(dcall)
        body.add(asgn)
      else:
        var ds = newNode(nkDiscardStmt, dinfo)
        ds.add(emptyNode(dinfo))
        body.add(ds)
      def.add(body)
      let key = defName.toLowerAscii
      var sigs = p.intfSigs.getOrDefault(key)
      sigs.add(def)
      p.intfSigs[key] = sigs
      p.opt(pxSemiColon)
      skipCom(p)
      continue
    parError(p, "unsupported interface member: " & $p.tok &
        " (v1 supports methods only)")
  p.eat(pxEnd)
  result = res

proc parseTypeDesc*(p: var TParser, definition: Node): Node =
  let oldcontext = p.context
  p.context = conTypeDesc
  if p.tok.xkind == pxPacked: getTokP(p)  # {.packed.} handled by renderer
  case p.tok.xkind
  of pxCommand:
    result = parseDirective(p)
  of pxProcedure, pxFunction:
    result = parseRoutineType(p)
  of pxRecord:
    # anonymous record -> object type
    result = newNodeP(nkObjectTy, p)
    result.isRecordType = true
    getTokP(p)
    skipCom(p)
    if definition.kind == nkIdent:
      p.recordTypes[definition.strVal.toLowerAscii] = true
    result.add(emptyNode(p.tok.info))     # no inheritance
    let body = newNode(nkRecList, p.tok.info)
    while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
      case p.tok.xkind
      of pxSymbol:
        let defs = parseIdentColonEquals(p, false)
        # field types for 1-based string indexing (`rec.field[i]`)
        let mty = p.mappedTypeName(defs[defs.len - 2])
        if definition.kind == nkIdent and mty.len > 0:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.fieldTypes[definition.strVal.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] = mty
        # array-of-class/record fields: element type for with-index
        if definition.kind == nkIdent:
          var fel = p.syms.classSpelling(defs[1].strVal)
          if fel.len == 0 and defs[1].kind == nkIdent:
            fel = p.arrayAliases.getOrDefault(
                defs[1].strVal.toLowerAscii, "")
          if fel.len == 0 and defs[1].kind in {nkArrayTy, nkSeqTy}:
            if defs[1][defs[1].len - 1].kind == nkIdent:
              fel = defs[1][defs[1].len - 1].strVal
          if fel.len > 0:
            for i in 0 ..< defs.len - 2:
              if defs[i].kind == nkIdent:
                p.classFieldTypes[definition.strVal.toLowerAscii & "." &
                                  defs[i].strVal.toLowerAscii] = fel
        body.add(defs)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxCase:
        let flat = parseRecordCase(p)
        for k in 0 ..< flat.len:
          body.add(flat[k])
      of pxComment:
        skipCom(p)
      of pxPublic:
        p.visibility = visPublic
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxPrivate:
        p.visibility = visPrivate
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxProtected:
        p.visibility = visProtected
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxPublished:
        p.visibility = visPublished
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxClass:
        # `class operator` (v1); class vars in records rejected
        let pk = p.peekTok().xkind
        if pk in {pxProcedure, pxFunction, pxOperator}:
          let rd = parseRoutine(p, true)
          if rd.kind == nkProcDef and rd.len > 0 and
              rd[rd.len - 1].kind == nkEmpty:
            let c = newNode(nkCommentStmt, rd.info)
            c.strVal = "# class method: " & rd[0].strVal
            body.add(c)
          else:
            body.add(rd)
          p.opt(pxSemiColon)
          skipCom(p)
        else:
          parError(p, "unsupported record member: class " & $p.peekTok() &
              " (v1 supports class operator only)")
      else:
        parError(p, "field or `case` expected in record body, got " & $p.tok)
    p.eat(pxEnd)
    skipCom(p)
    result.add(body)
  of pxObject:
    # `object` type inside a type section body (already parsed by
    # parseRecordOrObject when part of a named def)
    result = newNodeP(nkObjectTy, p)
    getTokP(p)
    skipCom(p)
    result.add(emptyNode(p.tok.info))
    result.add(emptyNode(p.tok.info))
  of pxClass:
    result = parseRecordOrObject(p, nkRefTy, definition)
  of pxInterface:
    result = parseInterfaceType(p, definition)
  of pxParLe:
    result = parseEnum(p)
  of pxArray:
    result = newNodeP(nkArrayTy, p)
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxBracketLe:
      # static array: array[lo..hi] of T
      getTokP(p)
      let idx = rangeExpr(p)
      if definition.kind == nkIdent and idx.kind == nkRange and
          idx.len == 2 and idx[0].kind in {nkIntLit, nkInt64Lit}:
        # alias types carry their low bound for var declarations
        p.arrayTypeLows[definition.strVal.toLowerAscii] = int(idx[0].intVal)
      p.eat(pxBracketRi)
      p.syms = p.syms  # no-op; keep table
      result.add(idx)
    elif p.inParamList:
      if p.peekTok.xkind == pxConst:
        # `array of const` -> TArrayOfConst (shim)
        result = newIdentNode("TArrayOfConst", p.tok.info)
        getTokP(p)          # consume `of`
        p.eat(pxConst)
        p.context = oldcontext
        return
      result.kind = nkOpenArrayTy
    else:
      # named dynamic array type: `array of T` -> seq[T]
      result.kind = nkSeqTy
    p.eat(pxOf)
    skipCom(p)
    result.add(parseTypeDesc(p, emptyNode(p.tok.info)))
  of pxSet:
    result = newNodeP(nkSetTy, p)
    getTokP(p)
    p.eat(pxOf)
    skipCom(p)
    result.add(parseTypeDesc(p, emptyNode(p.tok.info)))
  of pxHat:
    getTokP(p)
    if p.peekTok.xkind == pxCommand:
      result = parseDirective(p)
    elif pfRefs in p.flags:
      result = newNodeP(nkRefTy, p)
    else:
      result = newNodeP(nkPtrTy, p)
    skipCom(p)
    result.add(parseTypeDesc(p, emptyNode(p.tok.info)))
  of pxType:
    getTokP(p)
    result = parseTypeDesc(p, emptyNode(p.tok.info))
  else:
    if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "reference" and
        p.peekTok().xkind == pxTo:
      # `reference to procedure(...)`: a plain nimony proc type; the
      # closure state lives in nimony's anonymous-proc machinery
      getTokP(p)
      p.eat(pxTo)
      skipCom(p)
      result = parseRoutineType(p)
      p.context = oldcontext
      return
    if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "specialize":
      # FPC style: `specialize TPair<int, string>`
      getTokP(p)
      skipCom(p)
    if p.tok.xkind == pxSymbol and p.peekTok().xkind == pxLt:
      result = finishGenericInstantiation(p,
          newIdentNode(p.tok.ident, p.tok.info), false)
    else:
      let a = parseExpr(p)
      if p.tok.xkind == pxDotDot:
        result = newNodeP(nkRangeTy, p)
        let r = newNode(nkRange, a.info)
        r.add(a)
        getTokP(p)
        r.add(parseExpr(p))
        result.add(r)
      else:
        result = a
  p.context = oldcontext

proc addPragmaToIdent*(ident: Node, pragma: Node): Node =
  ## attach a pragma to a type definition's name node
  if ident.kind == nkPragmaExpr:
    ident[1].add(pragma)
    result = ident
  else:
    let pragmasNode = newNode(nkPragma, ident.info)
    pragmasNode.add(pragma)
    let e = newNode(nkPragmaExpr, ident.info)
    e.add(ident)
    e.add(pragmasNode)
    result = e

proc parseRecordBody(p: var TParser, result: Node, definition: Node) =
  let oldSelfClass = p.selfClass
  let oldVisibility = p.visibility
  if definition.kind == nkIdent:
    p.selfClass = definition.strVal
  skipCom(p)
  p.visibility = visPublic
  var body = newNode(nkRecList, p.tok.info)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    case p.tok.xkind
    of pxCurlyDirLe, pxStarDirLe:
      # conditionals + directives between class members
      if not declDirective(p):
        break
    of pxSymbol:
      let defs = parseIdentColonEquals(p, false)
      # register fields for self-qualification
      if p.selfClass.len > 0 and defs[1].kind != nkProcTy:
        let mty = p.mappedTypeName(defs[defs.len - 2])
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.syms.addField(p.selfClass, defs[i].strVal)
            if defs[1].kind == nkSetTy:
              p.varTypes[defs[i].strVal.toLowerAscii] = "set"
            if mty.len > 0:
              p.fieldTypes[p.selfClass.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] = mty
            var fcls = p.syms.classSpelling(defs[1].strVal)
            if fcls.len == 0 and defs[1].kind == nkIdent:
              # array-alias field: `FBuckets: TBucketArray` -> element
              fcls = p.arrayAliases.getOrDefault(defs[1].strVal.toLowerAscii, "")
            if fcls.len == 0 and defs[1].kind in {nkArrayTy, nkSeqTy}:
              # array-of-class/record field: `with Buckets[i] do`
              # resolves to the ELEMENT type (the index case unwraps
              # the array)
              let arr = defs[1]
              if arr.len > 0 and arr[arr.len - 1].kind == nkIdent:
                fcls = p.syms.classSpelling(arr[arr.len - 1].strVal)
                if fcls.len == 0 and
                    p.recordTypes.hasKey(arr[arr.len - 1].strVal.toLowerAscii):
                  fcls = arr[arr.len - 1].strVal
            if fcls.len > 0:
              p.classFieldTypes[p.selfClass.toLowerAscii & "." &
                                defs[i].strVal.toLowerAscii] = fcls
        if defs[1].kind == nkIdent and
            p.methodPtrTypes.hasKey(defs[1].strVal.toLowerAscii):
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.methodPtrVars[defs[i].strVal.toLowerAscii] =
                p.methodPtrTypes.getOrDefault(defs[1].strVal.toLowerAscii)
      skipCom(p)
      body.add(defs)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxCase:
      let flat2 = parseRecordCase(p)
      for k2 in 0 ..< flat2.len:
        body.add(flat2[k2])
      p.opt(pxSemiColon)
      skipCom(p)
    of pxPrivate:
      p.visibility = visPrivate
      getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxProtected:
      p.visibility = visProtected
      getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxPublic:
      p.visibility = visPublic
      getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxPublished:
      p.visibility = visPublished
      getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxComment:
      skipCom(p)
    of pxFunction, pxProcedure, pxConstructor, pxDestructor:
      # bodiless method declarations inside the class body
      let a = parseRoutine(p, true)
      if a.kind in {nkProcDef, nkFuncDef} and a.len > 0 and
          a[a.len - 1].kind == nkEmpty and
          p.syms.isGenericClass(definition.strVal):
        # a generic class's bodiless declaration must not become a
        # module-level forward: the type params would be undeclared
        # there; the implementation alone defines the member
        let c = newNode(nkCommentStmt, a.info)
        c.strVal = "# class member: " & a[0].strVal
        body.add(c)
      else:
        body.add(a)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxProperty:
      discard parseProperty(p)
      p.opt(pxSemiColon)
      skipCom(p)
    of pxClass:
      # `class procedure/function` (parseRoutine consumes the prefix),
      # `class var` (module-level storage), others rejected
      let pk = p.peekTok().xkind
      if pk == pxVar:
        getTokP(p)                  # consume `class`
        getTokP(p)                  # consume `var`
        skipCom(p)
        if p.tok.xkind != pxSymbol:
          parError(p, "class var name expected, got " & $p.tok)
        let defs = parseIdentColonEquals(p, false)
        for i in 0 ..< defs.len - 2:
          if defs[i].kind != nkIdent:
            continue
          let nm = defs[i].strVal
          p.syms.addClassVar(p.selfClass, nm)
          let vh = newNode(nkIdentDefs, defs[i].info)
          let vn = newIdentNode(
              p.syms.classVarName(p.selfClass, nm), defs[i].info)
          vn.exported = p.visibility != visPrivate
          vh.add(vn)
          vh.add(defs[defs.len - 2])
          vh.add(defs[defs.len - 1])
          p.classVarHoist.add(vh)
        p.opt(pxSemiColon)
        skipCom(p)
      elif pk in {pxProcedure, pxFunction, pxOperator}:
        let rd = parseRoutine(p, true)
        if rd.kind == nkProcDef and rd.len > 0 and
            rd[rd.len - 1].kind == nkEmpty:
          # class methods lower to module-level procs; the bodiless
          # class-body declaration needs no forward (its name would
          # even drift through the RTL spelling map: Double -> float64)
          let c = newNode(nkCommentStmt, rd.info)
          c.strVal = "# class method: " & rd[0].strVal
          body.add(c)
        else:
          body.add(rd)
        p.opt(pxSemiColon)
        skipCom(p)
      else:
        parError(p, "unsupported class member: class " & $p.peekTok() &
            " (v1 supports class procedure/function/var)")
    else:
      parError(p, "class member expected, got " & $p.tok)
      break
  result.add(body)
  p.eat(pxEnd)
  # optional trailing class pragma/command (e.g. `acyclic`)
  if p.tok.xkind == pxSymbol and definition.kind == nkIdent:
    let word = p.tok.ident.toLowerAscii
    if word == "acyclic":
      getTokP(p)
      discard
  elif p.tok.xkind == pxCommand and definition.kind == nkIdent:
    discard parseDirective(p)
  p.opt(pxSemiColon)
  skipCom(p)
  p.visibility = oldVisibility
  p.selfClass = oldSelfClass

proc parseRecordOrObject*(p: var TParser, kind: NodeKind,
                          definition: Node): Node =
  ## parses `class`/`object` bodies; `definition` is the def name node
  var record: Node
  result = newNode(kind, definition.info)
  if kind == nkRefTy:
    record = newNode(nkObjectTy, definition.info)
    result.add(record)
  else:
    record = result
  getTokP(p)                    # skip `class`/`object`
  skipCom(p)
  if p.tok.xkind == pxSemiColon:
    # forward declaration: `Name = class;`
    getTokP(p)
    result = newNode(nkCommentStmt, definition.info)
    result.strVal = "# forward: " & definition.strVal
    return
  let defName = definition.strVal
  var parent = ""
  if p.tok.xkind == pxParLe:
    getTokP(p)
    skipCom(p)
    let parentTy = parseTypeDesc(p, emptyNode(p.tok.info))
    if parentTy.kind == nkIdent:
      parent = parentTy.strVal
    # Delphi implements list: (Parent, IIntf1, IInt2)
    var interfaces: seq[string] = @[]
    while p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
      let itfTy = parseTypeDesc(p, emptyNode(p.tok.info))
      if itfTy.kind == nkIdent:
        interfaces.add(itfTy.strVal)
    let ofInh = newNode(nkOfInherit, parentTy.info)
    if interfaces.len > 0:
      # v1: exactly one interface; TInterfacedObject counts as empty
      # plumbing and dissolves into the interface's generated class
      if interfaces.len > 1:
        parError(p, "multiple interfaces on one class not supported (v1)")
      if parent.toLowerAscii == "tinterfacedobject" or parent.len == 0:
        parent = interfaces[0]
        ofInh.add(newIdentNode(parent, parentTy.info))
      else:
        parError(p, "a class with a real parent cannot implement an " &
            "interface yet (v1)")
    else:
      ofInh.add(parentTy)
    record.add(ofInh)
    p.eat(pxParRi)
    skipCom(p)
    if p.tok.xkind == pxSemiColon:
      # one-liner subclass: `EBadLine = class(Exception);` - emit the
      # type with an empty body so ctors/raises resolve
      getTokP(p)
      p.syms.registerClass(defName, parent, true)
      var emptyBody = newNode(nkRecList, definition.info)
      record.add(emptyBody)
      return
  elif kind == nkRefTy:
    # class without ancestor: inherit from RootRef
    let ofInh = newNode(nkOfInherit, definition.info)
    ofInh.add(newIdentNode("RootRef", definition.info))
    record.add(ofInh)
  else:
    record.add(emptyNode(definition.info))
  p.syms.registerClass(defName, parent, kind == nkRefTy)
  if p.curTypeParams.len > 0:
    var ci = p.syms.classes.getOrDefault(defName.toLowerAscii)
    ci.typeParams = p.curTypeParams
    p.syms.classes[defName.toLowerAscii] = ci
  parseRecordBody(p, record, definition)

# ---------------------------------------------------------------------------
# properties (accessors generated in genPropertyAccessors)

proc parseProperty*(p: var TParser): Node =
  ## `property Name: Typ read FData write setData;` — returns a comment
  ## node; the real accessors are generated in a post-pass.
  result = newNodeP(nkCommentStmt, p)
  getTokP(p)                    # skip `property`
  skipCom(p)
  if p.tok.xkind != pxSymbol:
    parError(p, "property name expected, got " & $p.tok)
  let propName = p.tok.ident
  getTokP(p)
  p.syms.declareName(propName)
  let decl = TPropDecl(cls: p.selfClass, name: propName,
                       isPublic: p.visibility != visPrivate)
  if p.tok.xkind == pxBracketLe:
    # array property: [index: Integer]
    getTokP(p)
    let params = newNode(nkFormalParams, p.tok.info)
    params.add(emptyNode(p.tok.info))
    while p.tok.xkind != pxBracketRi and p.tok.xkind != pxEof:
      var isVar = false
      if p.tok.xkind == pxConst or p.tok.xkind == pxVar:
        getTokP(p)
      if p.tok.xkind != pxSymbol:
        parError(p, "index name expected")
      let idxName = p.tok.ident
      getTokP(p)
      p.syms.declareName(idxName)
      p.eat(pxColon)
      let idxTy = parseTypeDesc(p, emptyNode(p.tok.info))
      let d = newNode(nkIdentDefs, idxTy.info)
      d.add(newIdentNode(idxName, idxTy.info))
      d.add(idxTy)
      d.add(emptyNode(idxTy.info))
      params.add(d)
      p.opt(pxSemiColon)
      skipCom(p)
    p.eat(pxBracketRi)
    decl.params = params
  p.eat(pxColon)
  skipCom(p)
  decl.typ = parseTypeDesc(p, emptyNode(p.tok.info))
  if decl.params == nil and p.selfClass.len > 0:
    # array-typed property (`property Buckets: TBucketArray ...`):
    # register the element type so `with Buckets[i] do` resolves
    var elty = ""
    if decl.typ.kind in {nkArrayTy, nkSeqTy} and decl.typ.len > 0 and
        decl.typ[decl.typ.len - 1].kind == nkIdent:
      elty = decl.typ[decl.typ.len - 1].strVal
    elif decl.typ.kind == nkIdent:
      elty = p.arrayAliases.getOrDefault(decl.typ.strVal.toLowerAscii, "")
    if elty.len > 0:
      let elKey = elty.toLowerAscii
      if p.syms.isClass(elKey) or p.recordTypes.hasKey(elKey):
        p.classFieldTypes[p.selfClass.toLowerAscii & "." &
                          propName.toLowerAscii] = elty
  if decl.typ.kind == nkIdent and
      p.methodPtrTypes.hasKey(decl.typ.strVal.toLowerAscii):
    p.methodPtrVars[propName.toLowerAscii] =
      p.methodPtrTypes.getOrDefault(decl.typ.strVal.toLowerAscii)
  skipCom(p)
  while p.tok.xkind != pxEof and p.tok.xkind != pxSemiColon:
    if p.tok.xkind == pxSymbol:
      let word = p.tok.ident.toLowerAscii
      if word == "read":
        getTokP(p)
        if p.tok.xkind == pxSymbol:
          decl.readId = p.tok.ident
          getTokP(p)
      elif word == "write":
        getTokP(p)
        if p.tok.xkind == pxSymbol:
          decl.writeId = p.tok.ident
          getTokP(p)
      elif word == "default":
        getTokP(p)
        if p.tok.xkind != pxSemiColon:
          # `default;` after the accessor list vs `default <value>`
          discard parseExpr(p)
        else:
          decl.isDefault = true
      elif word == "nodefault":
        getTokP(p)
      else:
        parError(p, "unexpected token in property: " & p.tok.ident)
    else:
      parError(p, "unexpected token in property: " & $p.tok)
  p.opt(pxSemiColon)
  skipCom(p)
  if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "default":
    decl.isDefault = true
    getTokP(p)
    p.opt(pxSemiColon)
    skipCom(p)
  result.strVal = "property " & propName
  p.props.add(decl)
  if decl.isDefault and decl.readId.len > 0:
    p.syms.setArrayProp(decl.cls, decl.name, decl.readId, decl.writeId,
                        "", "")

proc newDotP(base: Node; name: string): Node =
  result = newNode(nkDotExpr, base.info)
  result.add(base)
  result.add(newIdentNode(name, base.info))

proc isNilNode(n: Node): bool =
  n.kind == nkNilLit or
  (n.kind == nkIdent and n.strVal.toLowerAscii == "nil")

proc methodPtrLeafName(n: Node): string =
  ## lowercase method-ptr field name for `<base>.ev` or bare `ev`
  if n.kind == nkDotExpr and n[1].kind == nkIdent:
    result = n[1].strVal.toLowerAscii
  elif n.kind == nkIdent:
    result = n.strVal.toLowerAscii
  else:
    result = ""

proc methodPtrRecord(p: var TParser; nameNode, procTy: Node): Node =
  ## `T = procedure(...) of object` ->
  ## `T = object evProc: proc(self: RootRef; ...); evObj: RootRef`
  let params = procTy[0]
  p.methodPtrTypes[nameNode.strVal.toLowerAscii] = params
  let info = nameNode.info
  var recList = newNode(nkRecList, info)
  var fp = newNode(nkFormalParams, info)
  fp.add(emptyNode(info))            # return slot
  var sd = newNode(nkIdentDefs, info)
  sd.add(newIdentNode("self", info))
  sd.add(newIdentNode("RootRef", info))
  sd.add(emptyNode(info))
  fp.add(sd)
  for i in 1 ..< params.len:
    fp.add(params[i])
  var pt = newNode(nkProcTy, info)
  pt.add(fp)
  pt.add(emptyNode(info))
  var fd = newNode(nkIdentDefs, info)
  fd.add(newIdentNode("evProc", info))
  fd.add(pt)
  fd.add(emptyNode(info))
  recList.add(fd)
  var od = newNode(nkIdentDefs, info)
  od.add(newIdentNode("evObj", info))
  od.add(newIdentNode("RootRef", info))
  od.add(emptyNode(info))
  recList.add(od)
  result = newNode(nkObjectTy, info)
  result.isRecordType = true
  result.add(emptyNode(info))
  result.add(recList)

proc methodPtrThunk(p: var TParser; cls, handler: string; params: Node;
                    info: TLineInfo): string =
  ## synthesize `proc pasThunkN(self: RootRef; <params>) =
  ##   cast[Cls](self).Handler(<params>)` at module level
  inc p.thunkCounter
  result = "pasThunk" & $p.thunkCounter
  var def = newNode(nkProcDef, info)
  def.add(newIdentNode(result, info))
  def.add(emptyNode(info))           # generic params
  var fp = newNode(nkFormalParams, info)
  fp.add(emptyNode(info))            # return slot
  var sd = newNode(nkIdentDefs, info)
  sd.add(newIdentNode("self", info))
  sd.add(newIdentNode("RootRef", info))
  sd.add(emptyNode(info))
  fp.add(sd)
  for i in 1 ..< params.len:
    fp.add(params[i])
  def.add(fp)
  var pragmas = newNode(nkPragma, info)
  pragmas.add(newIdentNode("raises", info))
  def.add(pragmas)
  def.add(emptyNode(info))           # exceptions
  var castN = newNode(nkCast, info)
  castN.add(newIdentNode(cls, info))
  castN.add(newIdentNode("self", info))
  var call = newNode(nkCall, info)
  call.add(newDotP(castN, handler))
  for i in 1 ..< params.len:
    if params[i].kind == nkIdentDefs:
      call.add(newIdentNode(params[i][0].strVal, info))
  var body = newNode(nkStmtList, info)
  body.add(call)
  def.add(body)
  p.module.add(def)

proc methodPtrFieldBase(p: var TParser; n: Node): Node =
  ## a property access used as a method-pointer base maps to its
  ## backing field (`Btn.OnClick` -> `Btn.FOnClick`); the property
  ## template only inlines in the .nim path, the NIF path needs the
  ## real field
  result = n
  if n.kind != nkDotExpr or n.len != 2 or n[1].kind != nkIdent:
    return
  let leaf = n[1].strVal.toLowerAscii
  for pr in p.props:
    if pr.params == nil and pr.name.toLowerAscii == leaf:
      let fld = if pr.writeId.len > 0: pr.writeId else: pr.readId
      if fld.len > 0:
        result = newDotP(n[0], fld)
      return

proc rewriteMethodPtrAsgn(p: var TParser; a, b: Node; info: TLineInfo): Node =
  ## `ev := Handler` / `x.ev := X.Handler` / `x.ev := nil` ->
  ## two assignments on the evProc/evObj fields
  result = emptyNode(info)
  let evName = methodPtrLeafName(a)
  if evName.len == 0 or not p.methodPtrVars.hasKey(evName):
    return
  let params = p.methodPtrVars.getOrDefault(evName)
  let a = p.methodPtrFieldBase(a)
  var procAsgn = emptyNode(info)
  var objAsgn = emptyNode(info)
  if isNilNode(b):
    procAsgn = newNode(nkAsgn, info)
    procAsgn.add(newDotP(a, "evProc"))
    var nilN = newNode(nkNilLit, info)
    procAsgn.add(nilN)
    objAsgn = newNode(nkAsgn, info)
    objAsgn.add(newDotP(a, "evObj"))
    objAsgn.add(newNode(nkNilLit, info))
  elif b.kind == nkDotExpr and b[0].kind == nkIdent and b[1].kind == nkIdent:
    # `X.Handler`: bind X's instance, cast in the thunk
    let xTy = p.varTypes.getOrDefault(b[0].strVal.toLowerAscii)
    var cls = ""
    if xTy.startsWith("class:"):
      cls = xTy["class:".len .. ^1]
    elif p.paramClassTypes.getOrDefault(b[0].strVal.toLowerAscii).len > 0:
      # X is a routine parameter of class type
      cls = p.paramClassTypes.getOrDefault(b[0].strVal.toLowerAscii)
    elif b[0].strVal.toLowerAscii == "self" and p.selfClass.len > 0:
      cls = p.selfClass
    if cls.len > 0 and p.syms.classSpelling(cls).len > 0:
      let spelling = p.syms.classSpelling(cls)
      if spelling.len > 0:
        let tname = p.methodPtrThunk(spelling, b[1].strVal, params, info)
        procAsgn = newNode(nkAsgn, info)
        procAsgn.add(newDotP(a, "evProc"))
        procAsgn.add(newIdentNode(tname, info))
        objAsgn = newNode(nkAsgn, info)
        objAsgn.add(newDotP(a, "evObj"))
        objAsgn.add(b[0])
  elif b.kind == nkIdent:
    # bare `Handler` inside a method: bind `self`
    if p.selfClass.len > 0:
      let spelling = p.syms.classSpelling(p.selfClass)
      if spelling.len > 0:
        let tname = p.methodPtrThunk(spelling, b.strVal, params, info)
        procAsgn = newNode(nkAsgn, info)
        procAsgn.add(newDotP(a, "evProc"))
        procAsgn.add(newIdentNode(tname, info))
        objAsgn = newNode(nkAsgn, info)
        objAsgn.add(newDotP(a, "evObj"))
        objAsgn.add(newIdentNode("self", info))
  if procAsgn.kind != nkEmpty:
    result = newNode(nkStmtList, info)
    result.add(procAsgn)
    result.add(objAsgn)

proc rewriteMethodPtrCall(p: var TParser; n: Node): Node =
  ## `ev(args)` / `x.ev(args)` -> `ev.evProc(ev.evObj, args)`
  if n.kind != nkCall or n.len == 0: return n
  let evName = methodPtrLeafName(n[0])
  if evName.len == 0 or not p.methodPtrVars.hasKey(evName):
    return n
  let base = p.methodPtrFieldBase(n[0])
  var call = newNode(nkCall, n.info)
  call.add(newDotP(base, "evProc"))
  call.add(newDotP(base, "evObj"))
  for i in 1 ..< n.len:
    call.add(n[i])
  return call

proc rewriteMethodPtrNilCmp(p: var TParser; n: Node): Node =
  ## `ev = nil` / `ev <> nil` -> compare the evProc field
  if n.kind != nkInfix or n.len != 3: return n
  let op = n[0].strVal
  if op != "==" and op != "!=": return n
  var mpSide = emptyNode(n.info)
  if isNilNode(n[2]):
    mpSide = n[1]
  elif isNilNode(n[1]):
    mpSide = n[2]
  if mpSide.kind == nkEmpty: return n
  let evName = methodPtrLeafName(mpSide)
  if evName.len == 0 or not p.methodPtrVars.hasKey(evName):
    return n
  let base = p.methodPtrFieldBase(mpSide)
  result = newNode(nkInfix, n.info)
  result.add(n[0])
  result.add(newDotP(base, "evProc"))
  result.add(newNode(nkNilLit, n.info))

proc parseTypeDef*(p: var TParser): Node =
  ## one `Name = type` definition
  result = newNodeP(nkTypeDef, p)
  if p.tok.xkind != pxSymbol:
    parError(p, "type name expected, got " & $p.tok)
  var name = p.tok.ident
  let nameInfo = p.tok.info
  if name.toLowerAscii == "generic" and p.peekTok().xkind == pxSymbol:
    # FPC objfpc style: `generic TFoo<T> = class ...`
    getTokP(p)
    skipCom(p)
    name = p.tok.ident
  getTokP(p)
  skipCom(p)
  p.syms.declareName(name)
  let nameNode = newIdentNode(name, nameInfo)
  if p.section == seInterface and p.visibility != visPrivate:
    nameNode.exported = true
  result.add(nameNode)
  if p.tok.xkind == pxLt:
    # generic type: `TFoo<K, V> = ...` (Delphi style)
    result.add(parseGenericParams(p))
  else:
    result.add(emptyNode(nameInfo))      # generic params (unused)
  if p.tok.xkind == pxEquals:
    getTokP(p)
    skipCom(p)
    case p.tok.xkind
    of pxClass:
      let ty = parseRecordOrObject(p, nkRefTy, nameNode)
      if ty.kind == nkCommentStmt:
        # forward declaration: emit only the comment
        result = ty
        return
      result.add(ty)
    of pxObject:
      result.add(parseRecordOrObject(p, nkObjectTy, nameNode))
    else:
      result.add(parseTypeDesc(p, nameNode))
    # `procedure of object` -> method-pointer record (evProc/evObj)
    if result.len == 3 and result[2].kind == nkProcTy and
        result[2].len > 1 and result[2][1].kind == nkPragma:
      result[2] = p.methodPtrRecord(result[0], result[2])
  else:
    result.add(emptyNode(nameInfo))
  if p.tok.xkind == pxSemiColon:
    getTokP(p)
    skipCom(p)
  # a `specialize` alias is a first-class class type
  if result.len == 3 and result[2].kind == nkIndexExpr:
    p.syms.registerSpecializedAlias(result[2][0].strVal, name)
  p.curTypeParams = @[]

proc parseTypeSection*(p: var TParser): Node =
  result = newNodeP(nkTypeSection, p)
  getTokP(p)                    # skip `type`
  skipCom(p)
  while true:
    skipCom(p)                  # comments between definitions
    # directives between type definitions ({$EXTERNALSYM ...},
    # conditionals) must not close the section
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      if declDirective(p):
        continue
      break
    if p.tok.xkind != pxSymbol:
      break
    let def = parseTypeDef(p)
    skipCom(p)
    # `TArr = array of TRec` alias: track the element for with-index
    # resolution (`with propOfTArr do`)
    if def.len == 3 and def[0].kind == nkIdent and
        def[2].kind in {nkArrayTy, nkSeqTy} and def[2].len > 0 and
        def[2][def[2].len - 1].kind == nkIdent:
      let el = def[2][def[2].len - 1].strVal.toLowerAscii
      if p.syms.isClass(el) or p.recordTypes.hasKey(el):
        p.arrayAliases[def[0].strVal.toLowerAscii] =
          def[2][def[2].len - 1].strVal
    result.add(def)

# ---------------------------------------------------------------------------
# routines

proc routineKindNode(p: TParser, kind: TTokKind, isMethod: bool): NodeKind =
  if isMethod: result = nkMethodDef
  elif kind == pxFunction: result = nkFuncDef
  else: result = nkProcDef

proc parseRoutineSpecifiers*(p: var TParser, noBody: var bool,
                             isVirtual: var bool; isOverride: var bool;
                             sawReintroduce: var bool): Node =
  ## parses `virtual; override; overload; forward; static; inline;` etc.
  isOverride = false
  sawReintroduce = false
  result = newNodeP(nkPragma, p)
  while true:
    if p.tok.xkind != pxSymbol and p.tok.xkind != pxInline:
      # calling convention directives come as commands (e.g. {$X+}) - skip
      break
    let word = if p.tok.xkind == pxInline: "inline"
               else: p.tok.ident.toLowerAscii
    case word
    of "virtual":
      isVirtual = true
      getTokP(p)
    of "override":
      isVirtual = true
      isOverride = true
      getTokP(p)
    of "overload":
      getTokP(p)
    of "static":
      getTokP(p)
    of "inline":
      # forward as {.inline.} (both renderers emit the pragma son)
      result.add(newIdentNode("inline", p.tok.info))
      getTokP(p)
    of "cdecl", "stdcall":
      # nimony-fulfillable calling conventions - forward as pragmas
      result.add(newIdentNode(word, p.tok.info))
      getTokP(p)
    of "register", "pascal", "safecall":
      # no nimony equivalent - warn (default) or error (--strict)
      parWarning(p, "cc:" & word & ":" & renderInfo(p.tok.info),
          "calling convention '" & word & "' is not supported by the " &
          "nimony chain and is ignored")
      getTokP(p)
    of "forward":
      noBody = true
      getTokP(p)
    of "deprecated":
      # forward as {.deprecated.}; the optional message is
      # documentation only and is dropped (documented divergence)
      result.add(newIdentNode("deprecated", p.tok.info))
      getTokP(p)
      if p.tok.xkind == pxStrLit:
        getTokP(p)
    of "dynamic":
      # Delphi's message-based dispatch lowers to the same virtual
      # model: a dynamic method IS virtual in v1
      isVirtual = true
      getTokP(p)
    of "abstract":
      # no body follows (like forward); the dispatch goes to
      # overriding descendants
      noBody = true
      getTokP(p)
    of "reintroduce":
      # acknowledges that this declaration HIDES an ancestor method;
      # suppresses the hides-virtual warning (Delphi parity)
      sawReintroduce = true
      getTokP(p)
    of "platform", "experimental":
      # genuine no-ops: documentation annotations with no
      # nimony-side semantic
      getTokP(p)
    of "external":
      # external declarations: skip the string/qualifier
      getTokP(p)
      while p.tok.xkind in {pxStrLit, pxSymbol, pxDot}:
        getTokP(p)
      noBody = true
    else:
      break
    p.opt(pxSemiColon)
    skipCom(p)

proc skipAsmBlock(p: var TParser) =
  ## Delphi `asm ... end;` - the body is free-form assembler; v1 has
  ## no backend for it, so the block is skipped at the token level
  getTokP(p)                    # `asm`
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    getTokP(p)
  p.eat(pxEnd)
  p.opt(pxSemiColon)
  skipCom(p)

proc parseRoutineBody(p: var TParser, result: Node) =
  ## local decls + begin/end of a routine with a body
  var stmts = newNodeP(nkStmtList, p)
  p.nestedProcs = @[]
  let savedLabels = p.curLabels
  p.curLabels = @[]
  p.opt(pxSemiColon)
  while true:
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # conditionals between local declarations (Delphi guards a
      # whole `var` block or single locals this way)
      if declDirective(p):
        continue
      parError(p, "begin expected in routine body, got " & $p.tok)
    case p.tok.xkind
    of pxVar, pxThreadvar:
      stmts.add(parseVarSection(p))
      p.opt(pxSemiColon)
      skipCom(p)
    of pxConst, pxResourcestring:
      stmts.add(parseConstSection(p))
      p.opt(pxSemiColon)
      skipCom(p)
    of pxType:
      stmts.add(parseTypeSection(p))
      p.opt(pxSemiColon)
      skipCom(p)
    of pxLabel:
      parseLabelSection(p)
    of pxComment:
      skipCom(p)
    of pxBegin:
      break
    of pxAsm:
      # `function F(...): T; asm ... end;` - no begin-body; the asm
      # block IS the body, skipped and left empty (nimony gets the
      # discard-safe empty list)
      skipAsmBlock(p)
      p.lowerGotos(stmts)
      p.curLabels = savedLabels
      result.add(stmts)
      return
    of pxProcedure, pxFunction, pxConstructor, pxDestructor:
      let savedLabels = p.curLabels
      p.curLabels = @[]
      let nested = parseRoutine(p, false)
      p.curLabels = savedLabels
      if nested.kind in {nkProcDef, nkFuncDef, nkMethodDef} and
          nested[0].kind == nkIdent and nested.defClass.len > 0:
        # only method-nested routines get an explicit `self` param
        # (and thus need `self` at call sites); standalone nested
        # procs close over locals natively
        p.nestedProcs.add(nested[0].strVal)
      stmts.add(nested)
      p.opt(pxSemiColon)
      skipCom(p)
    else:
      parError(p, "begin expected in routine body, got " & $p.tok)
  let body = parseStmt(p)
  if body.kind == nkStmtList:
    if body.len == 0:
      # an empty (stmts) crashes nimony's NIF path - emit a discard
      let d = newNode(nkDiscardStmt, body.info)
      d.add(emptyNode(body.info))
      stmts.add(d)
    else:
      for s in body.sons: stmts.add(s)
  else:
    stmts.add(body)
  p.lowerGotos(stmts)
  p.curLabels = savedLabels
  result.add(stmts)

proc delphiOperatorSymbol(name: string): string =
  ## Delphi `class operator` method name -> nimony operator spelling;
  ## "" for operators v1 does not support
  case name.toLowerAscii
  of "add": result = "+"
  of "subtract": result = "-"
  of "multiply": result = "*"
  of "divide": result = "/"
  of "intdivide": result = "div"
  of "modulus": result = "mod"
  of "and": result = "and"
  of "or": result = "or"
  of "xor": result = "xor"
  of "leftshift": result = "shl"
  of "rightshift": result = "shr"
  of "equal": result = "=="
  of "notequal": result = "!="
  of "greaterthan": result = ">"
  of "greaterthanorequal": result = ">="
  of "lessthan": result = "<"
  of "lessthanorequal": result = "<="
  of "negative": result = "-"
  of "positive": result = "+"
  of "logicalnot", "bitwisenot": result = "not"
  else: result = ""

proc parseRoutine*(p: var TParser; noBody: bool): Node =
  ## procedure/function/constructor/destructor; also `class procedure`
  ## / `class function` (v1: static, lowered to module-level procs)
  var noBody = noBody
  var isVirtual: bool = false
  var isClassProc = false
  var kind = p.tok.xkind
  if kind == pxClass:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind in {pxProcedure, pxFunction, pxOperator}:
      isClassProc = true
      kind = p.tok.xkind
    else:
      parError(p, "unsupported `class` member: class " & $p.tok &
          " (v1 supports class procedure/function/operator/var)")
  let oldOuterName = p.outerProcName
  let oldOuterIsMethod = p.outerIsMethod
  let oldClass = p.classOfProc
  let oldSelfClass = p.selfClass
  let oldVisibility = p.visibility
  let oldResultVt = p.varTypes.getOrDefault("result")
  result = newNodeP(nkProcDef, p)
  getTokP(p)
  skipCom(p)
  if p.tok.xkind != pxSymbol:
    parError(p, "routine name expected, got " & $p.tok)
  var name = p.tok.ident
  let nameInfo = p.tok.info
  getTokP(p)
  skipCom(p)
  var isMethod = false
  var isDotted = false
  if p.tok.xkind == pxLt:
    # dotted generic impl: `TFoo<K, V>.DoIt` - the params come from
    # the class registry, the bracket list is only syntax
    while p.tok.xkind != pxGt and p.tok.xkind != pxEof:
      getTokP(p)
      skipCom(p)
    p.eat(pxGt)
    skipCom(p)
  if p.tok.xkind == pxDot:
    # qualified: `MyClass.doIt`
    let cls = name
    p.removeNextTok()
    skipCom(p)
    if p.tok.xkind != pxSymbol:
      parError(p, "method name expected, got " & $p.tok)
    name = p.tok.ident
    p.selfClass = cls
    p.classOfProc = cls
    isMethod = true
    isDotted = true
    getTokP(p)
    skipCom(p)
  elif p.classOfProc.len > 0 and p.section != seInterface:
    # nested routine inside a method body: it sees `self` implicitly
    isMethod = true
  elif p.selfClass.len > 0 and (p.section == seInterface or noBody):
    # bodiless member declaration inside a class body (interface
    # sections and program-local classes)
    isMethod = true
    p.classOfProc = p.selfClass
  # name + export marker; class methods lower to module-level names,
  # class operators to the nimony operator symbol (resolved by the
  # infix/prefix expression machinery)
  let opSym = if kind == pxOperator: delphiOperatorSymbol(name)
              else: ""
  if kind == pxOperator and opSym.len == 0 and
      name.toLowerAscii notin ["implicit", "explicit", "inc", "dec"]:
    parError(p, "unsupported class operator: " & name &
        " (v1 supports arithmetic, bitwise, comparison and unary " &
        "operators)")
  let defName = if kind == pxOperator and isClassProc and
      name.toLowerAscii in ["implicit", "explicit", "inc", "dec"]:
      "op" & name & "_" & p.classOfProc
    elif kind == pxOperator and isClassProc:
      opSym
    elif isClassProc and isMethod:
      p.syms.classProcName(p.classOfProc, name)
    else:
      name
  let nameNode = newIdentNode(defName, nameInfo)
  var isExported = p.section == seInterface and p.visibility != visPrivate
  if p.section == seImplementation and
      (p.syms.exportedNames.getOrDefault(name.toLowerAscii, false) or
       p.syms.exportedNames.getOrDefault(defName.toLowerAscii, false)):
    # the implementation of an interface-declared routine stays exported;
    # an unmarked redeclaration would shadow the export. Class methods
    # are keyed by their mangled module-level name.
    isExported = true
  if isExported:
    p.syms.exportedNames[defName.toLowerAscii] = true
  result.add(exSymbol(nameNode, isExported))
  # generic params: the enclosing generic class's type params
  var typeVars = emptyNode(nameInfo)
  if p.classOfProc.len > 0:
    let ci = p.syms.classes.getOrDefault(p.classOfProc.toLowerAscii)
    if ci.typeParams.len > 0:
      typeVars = newNode(nkBracket, nameInfo)
      for t in ci.typeParams:
        typeVars.add(newIdentNode(t, nameInfo))
  result.add(typeVars)
  # params
  let params = p.parseParamList()
  p.opt(pxSemiColon)
  skipCom(p)
  # return type (function)
  if p.tok.xkind == pxColon:
    getTokP(p)
    skipCom(p)
    let ret = parseTypeDesc(p, emptyNode(p.tok.info))
    params[0] = ret
    skipCom(p)
    p.opt(pxSemiColon)          # `function Speak: string; virtual;`
    skipCom(p)
  # `result` carries the routine's return type for conversion
  # insertion in the body (`result := intExpr` on a float/record
  # return)
  if params[0].kind == nkIdent:
    let retKey = params[0].strVal.toLowerAscii
    if p.recordTypes.hasKey(retKey):
      p.varTypes["result"] = "record:" & params[0].strVal
    else:
      let rtl = rtlSpelling(retKey)
      if rtl.len > 0: p.varTypes["result"] = rtl
  # Delphi conversion/unary class operators (Implicit/Explicit/Inc/
  # Dec) have no nimony operator symbol: lower to uniquely named
  # procs keyed by the signature, and register them for call-site
  # insertion (assignments, casts, Inc/Dec statements)
  if kind == pxOperator and isClassProc and
      name.toLowerAscii in ["implicit", "explicit", "inc", "dec"] and
      params.len >= 2:
    let cls = p.classOfProc
    let oname = name.toLowerAscii
    var mangled = ""
    if oname in ["inc", "dec"]:
      mangled = "op" & name & "_" & cls
      p.syms.addConvOp(cls, oname, "", "", mangled)
    elif params[1].kind == nkIdentDefs and params[1].len >= 2 and
        params[1][params[1].len - 2].kind == nkIdent and
        params[0].kind == nkIdent:
      let fromTy = params[1][params[1].len - 2].strVal
      let toTy = params[0].strVal
      mangled = "op" & name & "_" & fromTy & "_" & toTy
      p.syms.addConvOp(cls, oname, fromTy, toTy, mangled)
    if mangled.len > 0:
      nameNode.strVal = mangled
  # constructors: return the class type (before self-param insertion)
  if kind == pxConstructor and isMethod:
    params[0] = genSelfType(p)
    result.isCtor = true
  # self parameter for methods; class methods take none
  if isMethod and not isClassProc:
    # value objects get `var self`; class instances a plain ref
    let ci = p.syms.classes.getOrDefault(p.classOfProc.toLowerAscii)
    let selfParam = genSelfParam(p, not ci.isRef)
    var np = newNode(nkFormalParams, params.info)
    np.add(params[0])
    np.add(selfParam)
    for i in 1 ..< params.len:
      np.add(params[i])
    result.add(np)
  else:
    result.add(params)
  if isMethod:
    result.defClass = p.classOfProc
  # specifiers & pragmas
  var isOverride = false
  var sawReintroduce = false
  let pragmas = parseRoutineSpecifiers(p, noBody, isVirtual, isOverride,
                                       sawReintroduce)
  # Delphi method hiding: a descendant `virtual` (no override) over an
  # ancestor's same-name method introduces a NEW slot - the v1 model
  # lowers the hidden method as a static per-class routine (call sites
  # resolve by the receiver's declared type), so a base-typed
  # reference keeps dispatching to the ancestor's method, as Delphi
  # does. Without `reintroduce`, Delphi warns - so do we.
  if isVirtual and isMethod and not isOverride:
    let ci = p.syms.classes.getOrDefault(p.classOfProc.toLowerAscii)
    if ci.spelling.len > 0 and
        p.syms.isMethodOf(ci.parent, name):
      isVirtual = false
      if not sawReintroduce:
        parWarning(p, "hide:" & p.classOfProc & "." & name &
            ":" & renderInfo(nameInfo),
            "method '" & name & "' hides virtual method of ancestor " &
            "type - add reintroduce to acknowledge")
  result.add(pragmas)
  result.add(emptyNode(nameInfo))  # exceptions (unused)
  # register the parameter count (the paren-less call rule consults
  # it in expression position); parseParamList gives one IdentDefs
  # group per Pascal parameter (names, type, optional default)
  block:
    var argc = 0
    for gi in 1 ..< params.len:
      if params[gi].kind == nkIdentDefs: inc argc
    p.syms.routineArgs[defName.toLowerAscii] = argc
  # register the routine name
  if kind in {pxFunction, pxConstructor}:
    if isClassProc and isMethod:
      # call sites emit the mangled module-level name
      p.syms.returnsValue[defName.toLowerAscii] = true
    else:
      p.syms.returnsValue[name.toLowerAscii] = true
    if isMethod and not isClassProc:
      p.syms.returnsValue[p.classOfProc.toLowerAscii & "." &
                          name.toLowerAscii] = true
  elif kind == pxProcedure:
    # a void procedure with the same name must clear the flag (the
    # latest declaration wins; shim units may register value-returning
    # procs of common names like `add` at the uses clause)
    if isClassProc and isMethod:
      p.syms.returnsValue[defName.toLowerAscii] = false
    else:
      p.syms.returnsValue[name.toLowerAscii] = false
    if isMethod and not isClassProc:
      p.syms.returnsValue[p.classOfProc.toLowerAscii & "." &
                          name.toLowerAscii] = false
  if isClassProc and isMethod and kind != pxOperator:
    p.syms.addClassProc(p.classOfProc, name)
  elif isMethod and (isDotted or p.section == seInterface or noBody):
    if kind == pxConstructor or kind == pxDestructor:
      p.syms.addCtor(p.classOfProc, name)
    else:
      p.syms.addRoutine(p.classOfProc, name)
    if not isVirtual and p.section != seInterface:
      # implementation of a method declared virtual in THIS class's
      # body (self-only: a hidden/reintroduce'd method stays static)
      if p.syms.isMethodDeclared(p.classOfProc, name):
        isVirtual = true
    if isVirtual:
      p.syms.addMethod(p.classOfProc, name)
  elif isMethod and p.section != seInterface:
    # implementation of a method declared virtual in the interface
    # the class body's own decision wins over the ancestor walk: a
    # hidden (reintroduce'd) method stays a static routine
    if p.syms.isMethodDeclared(p.classOfProc, name):
      isVirtual = true
  if not isMethod:
    p.syms.declareName(name)
  # body
  var savedOuterParams = p.outerParams
  if p.section == seInterface or noBody:
    result.add(emptyNode(nameInfo))
  else:
    p.outerProcName = name
    p.outerIsMethod = isVirtual and isMethod and not isClassProc
    # param types for 1-based string indexing inside the body
    let savedParamTypes = p.paramTypes
    p.paramTypes = initTable[string, string]()
    for i in 1 ..< params.len:
      let d = params[i]
      if d.kind == nkIdentDefs:
        let mty = p.mappedTypeName(d[d.len - 2])
        if mty.len > 0:
          for j in 0 ..< d.len - 2:
            if d[j].kind == nkIdent:
              p.paramTypes[d[j].strVal.toLowerAscii] = mty
        let pty0 = d[d.len - 2]
        let pty = if pty0.kind == nkVarTy and pty0.len > 0: pty0[0] else: pty0
        if pty.kind == nkIdent:
          let pcls = p.syms.classSpelling(pty.strVal)
          if pcls.len > 0:
            for j in 0 ..< d.len - 2:
              if d[j].kind == nkIdent:
                p.paramClassTypes[d[j].strVal.toLowerAscii] = pcls
          if p.methodPtrTypes.hasKey(pty.strVal.toLowerAscii):
            for j in 0 ..< d.len - 2:
              if d[j].kind == nkIdent:
                p.methodPtrVars[d[j].strVal.toLowerAscii] =
                  p.methodPtrTypes.getOrDefault(pty.strVal.toLowerAscii)
    # remember the param names for bare `inherited;` forwarding
    p.outerParams = @[]
    for i in 1 ..< params.len:
      let d = params[i]
      if d.kind == nkIdentDefs:
        for j in 0 ..< d.len - 2:
          if d[j].kind == nkIdent:
            p.outerParams.add(d[j].strVal)
    parseRoutineBody(p, result)
    if kind == pxConstructor and isMethod:
      # constructors return self so `X := T.create(...)` works
      let pre = newNode(nkAsgn, nameInfo)
      pre.add(newIdentNode("result", nameInfo))
      pre.add(newIdentNode("self", nameInfo))
      let bodyNode = result[result.len - 1]
      var newSons: seq[Node] = @[pre]
      for i in 0 ..< bodyNode.sons.len:
        newSons.add(bodyNode.sons[i])
      bodyNode.sons = newSons
    p.varTypes["result"] = oldResultVt
    p.outerProcName = oldOuterName
    p.outerIsMethod = oldOuterIsMethod
    p.outerParams = savedOuterParams
    p.paramTypes = savedParamTypes
    p.classOfProc = oldClass
  # virtual/override -> method definition
  if isVirtual and isMethod:
    result.kind = nkMethodDef
  p.selfClass = oldSelfClass
  p.classOfProc = oldClass
  p.visibility = oldVisibility

# ---------------------------------------------------------------------------
# statements

proc parseInherited*(p: var TParser): Node =
  ## `inherited;` / `inherited name(args)` -> parent cast + call
  let info = p.tok.info
  p.eat(pxInherited)
  let parent = p.syms.ancestorSpelling(p.classOfProc)
  if parent.len == 0:
    # the class HAS a declared parent but it is not in our registry
    # (a shim/external class like TList): v1 lowers the inherited
    # call to a no-op with a warning - the registry lacks the target
    let ci = p.syms.lookupClass(p.classOfProc)
    if ci.spelling.len > 0 and ci.parent.len > 0:
      parWarning(p, "shiminherited:" & p.classOfProc & "." & ci.parent &
          ":" & renderInfo(info),
          "inherited against unknown parent class '" & ci.parent &
          "' is lowered to nothing (v1 shim gap)")
      if p.tok.xkind == pxSemiColon:
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
        return newNode(nkEmpty, info)
      if p.tok.xkind == pxSymbol:
        getTokP(p)
        if p.tok.xkind == pxParLe:
          var depth = 1
          getTokP(p)
          while depth > 0 and p.tok.xkind != pxEof:
            if p.tok.xkind == pxParLe: depth.inc
            elif p.tok.xkind == pxParRi: depth.dec
            getTokP(p)
        elif p.tok.xkind == pxBracketLe:
          # `inherited Data[i]` / `inherited Data[i] := v`: consume the
          # index; an assignment form also consumes `:=` + the RHS
          var depth = 1
          getTokP(p)
          while depth > 0 and p.tok.xkind != pxEof:
            if p.tok.xkind == pxBracketLe: depth.inc
            elif p.tok.xkind == pxBracketRi: depth.dec
            getTokP(p)
          if p.tok.xkind == pxAsgn:
            getTokP(p)
            skipCom(p)
            discard parseExpr(p)
        p.opt(pxSemiColon)
        skipCom(p)
      return newNode(nkEmpty, info)
    # v1: a parentless class inherits TObject; its Create/Destroy
    # are observable no-ops in our model, so bare `inherited`,
    # `inherited Create` and `inherited Destroy` lower to nothing.
    # A parent call with arguments has no sensible target - keep
    # the error for that case.
    if p.tok.xkind in {pxSemiColon, pxEnd}:
      # bare `inherited;` forwarding - nothing to do at the root
      if p.tok.xkind == pxSemiColon:
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
      result = newNode(nkEmpty, info)
      return
    if p.tok.xkind == pxSymbol and
        p.tok.ident.toLowerAscii in ["create", "destroy", "free"]:
      getTokP(p)
      skipCom(p)
      if p.tok.xkind == pxParLe:
        # argument list on a root call: still a no-op, skip it
        var depth = 1
        getTokP(p)
        while depth > 0 and p.tok.xkind != pxEof:
          if p.tok.xkind == pxParLe: depth.inc
          elif p.tok.xkind == pxParRi: depth.dec
          getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
      result = newNode(nkEmpty, info)
      return
    parError(p, "no parent class for `inherited`")
  let selfNode = newIdentNode("self", info)
  # cast[], not the T(x) call form: the call form trips nimsem's
  # nil-proof when the parent lives in another module
  let parentCast = newNode(nkCast, info)
  parentCast.add(newIdentNode(parent, info))
  parentCast.add(selfNode)
  if p.tok.xkind == pxSemiColon:
    # call the same routine on the parent, forwarding our params
    if p.outerProcName.len == 0:
      parError(p, "`inherited` outside of a routine")
    getTokP(p)
    p.opt(pxSemiColon)
    skipCom(p)
    let call = newNode(nkCall, info)
    call.add(newIdentNode(p.outerProcName, info))
    call.add(parentCast)
    for name in p.outerParams:
      call.add(newIdentNode(name, info))
    call.noQualCallee = true
    if p.outerIsMethod:
      let pc = newNode(nkCommand, info)
      pc.add(newIdentNode("procCall", info))
      pc.add(call)
      result = pc
    else:
      result = call
    if p.syms.returnsValue.getOrDefault(p.outerProcName.toLowerAscii, false):
      let d = newNode(nkDiscardStmt, info)
      d.add(result)
      result = d
  else:
    # `inherited name(args)` or `inherited name`
    var a = parseStmt(p)
    var call: Node
    # a bare call statement comes back discard-wrapped; unwrap it
    if a.kind == nkDiscardStmt and a.len > 0 and a[0].kind == nkCall:
      a = a[0]
    # the prelude exception ctor is named pasExcCreate in systempas
    # (a user `create` would shadow it)
    let parentIsPrelude =
      p.syms.classSpelling(parent) == "PasException" and
      a.kind == nkCall and a.len > 0 and a[0].kind == nkIdent and
      a[0].strVal.toLowerAscii == "create"
    if a.kind == nkCall:
      call = newNode(nkCall, a.info)
      if parentIsPrelude:
        call.add(newIdentNode("pasExcCreate", a.info))
      else:
        call.add(a[0])
      call.add(parentCast)
      for i in 1 ..< a.len:
        call.add(a[i])
    else:
      call = newNode(nkCall, a.info)
      if parentIsPrelude:
        call.add(newIdentNode("pasExcCreate", a.info))
      else:
        call.add(a)
      call.add(parentCast)
    call.noQualCallee = true
    if p.outerIsMethod:
      let pc = newNode(nkCommand, info)
      pc.add(newIdentNode("procCall", info))
      pc.add(call)
      result = pc
    else:
      # a value-returning inherited call must be discarded explicitly
      # (nimony rejects the bare value-dropping call in ctor bodies)
      let callee = if call.len > 0 and call[0].kind == nkIdent: call[0].strVal
                   else: ""
      if p.syms.returnsValue.getOrDefault(callee.toLowerAscii, false):
        let d = newNode(nkDiscardStmt, info)
        d.add(call)
        result = d
      else:
        result = call

proc parseCase*(p: var TParser): Node =
  result = newNodeP(nkCaseStmt, p)
  getTokP(p)                    # skip `case`
  skipCom(p)
  result.add(parseExpr(p))
  p.eat(pxOf)
  skipCom(p)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
    var b: Node
    if p.tok.xkind == pxElse:
      b = newNodeP(nkElse, p)
      getTokP(p)
    else:
      b = newNodeP(nkOfBranch, p)
      while p.tok.xkind != pxEof and p.tok.xkind != pxColon:
        b.add(rangeExpr(p))
        p.opt(pxComma)
        skipCom(p)
      p.eat(pxColon)
    skipCom(p)
    b.add(parseStmt(p))
    result.add(b)
    if b.kind == nkElse: break
  p.eat(pxEnd)

proc parseTry*(p: var TParser): Node =
  result = newNodeP(nkTryStmt, p)
  getTokP(p)                    # skip try
  skipCom(p)
  let body = newNodeP(nkStmtList, p)
  while not (p.tok.xkind in {pxFinally, pxExcept, pxEof, pxEnd}):
    # a compound-statement body (for/begin..end) leaves its trailing
    # `;` unconsumed - eat it between statements
    let s = parseStmt(p)
    if s.kind != nkEmpty: body.add(s)
    p.opt(pxSemiColon)
    skipCom(p)
  result.add(body)
  if p.tok.xkind == pxExcept:
    getTokP(p)
    skipCom(p)
    var sawOn = false
    let info = parLineInfo(p)
    while p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "on":
      sawOn = true
      let b = newNodeP(nkExceptBranch, p)
      getTokP(p)
      # `on E: SomeEx do`
      if p.tok.xkind != pxSymbol:
        parError(p, "exception variable name expected")
      let varName = p.tok.ident
      getTokP(p)
      p.syms.declareName(varName)
      p.eat(pxColon)
      let excTy = qualifiedIdent(p)
      skipCom(p)
      p.eat(pxDo)
      let handler = parseStmt(p)
      # map the Delphi exception class to an ErrorCode
      var code: string = ""
      if excTy.kind == nkIdent:
        code = excSpelling(excTy.strVal.toLowerAscii)
      elif excTy.kind == nkDotExpr:
        code = excSpelling(excTy[1].strVal.toLowerAscii)
      if code.len == 0:
        code = "Failure"
      # except ErrorCode as <hidden>: case <hidden> of code:
      #   var varName: ExcTy = pasCurrentExc; handler
      # the Pascal exception variable binds the INSTANCE (M4-2), the
      # ErrorCode rides a hidden binding
      inc p.thunkCounter
      let hidName = "pasECode" & $p.thunkCounter
      let varDecl = newNode(nkVarSection, info)
      let vd = newNode(nkIdentDefs, info)
      vd.add(newIdentNode(varName, info))
      vd.add(excTy)
      # the ErrorCode filter cannot prove the instance class: downcast
      let instCast = newNode(nkCast, info)
      instCast.add(excTy)
      instCast.add(newIdentNode("pasCurrentExc", info))
      vd.add(instCast)
      varDecl.add(vd)
      var body = handler
      if body.kind != nkStmtList:
        let sl = newNode(nkStmtList, info)
        sl.add(body)
        body = sl
      # prepend the instance binding (rebuild: nimony seq has no insert)
      let body2 = newNode(nkStmtList, info)
      body2.add(varDecl)
      for s in body.sons: body2.add(s)
      body = body2
      b.add(newIdentNode("ErrorCode", info))
      b.add(newIdentNode(hidName, info))
      let caseNode = newNode(nkCaseStmt, info)
      caseNode.add(newIdentNode(hidName, info))
      let ofBranch = newNode(nkOfBranch, info)
      ofBranch.add(newIdentNode(code, info))
      ofBranch.add(body)
      caseNode.add(ofBranch)
      let elseB = newNode(nkElse, info)
      let skip = newNode(nkStmtList, info)
      let discardN = newNode(nkDiscardStmt, info)
      discardN.add(emptyNode(info))
      skip.add(discardN)
      elseB.add(skip)
      caseNode.add(elseB)
      b.add(caseNode)
      result.add(b)
      p.opt(pxSemiColon)
      skipCom(p)
    if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "else":
      # bare `else` handler for the whole except section
      getTokP(p)
      let b = newNodeP(nkExceptBranch, p)
      b.add(emptyNode(info))
      b.add(emptyNode(info))
      if sawOn:
        # handled via the re-raise in each `on` branch; plain body here
        let body2 = parseStmt(p)
        b.add(body2)
        # replace: bare else only applies when no `on` branches exist
        b.kind = nkCommentStmt
        b.strVal = "# except-else after on-branches not supported"
      else:
        let body2 = parseStmt(p)
        b[1] = body2
      result.add(b)
      p.opt(pxSemiColon)
      skipCom(p)
    if not sawOn and result.len == 1:
      # `except <stmts> end` without on/else: plain handler
      let b = newNodeP(nkExceptBranch, p)
      b.add(emptyNode(info))
      b.add(emptyNode(info))
      let body2 = parseStmt(p)
      b[1] = body2
      result.add(b)
  if p.tok.xkind == pxFinally:
    getTokP(p)
    let fin = newNodeP(nkFinally, p)
    skipCom(p)
    # the finally body is a statement list up to `end` (a single
    # parseStmt stopped after the first statement)
    while not (p.tok.xkind in {pxEnd, pxEof}):
      let s = parseStmt(p)
      if s.kind == nkStmtList:
        for k in 0 ..< s.len: fin.add(s[k])
      else:
        fin.add(s)
    result.add(fin)
  p.eat(pxEnd)

var forLoopVarName: string = ""

proc parseFor*(p: var TParser): Node =
  result = newNodeP(nkForStmt, p)
  getTokP(p)                    # skip `for`
  skipCom(p)
  if p.tok.xkind != pxSymbol:
    parError(p, "loop variable expected, got " & $p.tok)
  let loopVarName = p.tok.ident
  # the for binds `_`: the shim iterator drives the *declared* Pascal
  # variable through its var parameter, so the body must resolve the
  # loop variable to that declaration, not to a fresh binding
  result.add(newIdentNode("_", p.tok.info))
  forLoopVarName = loopVarName
  getTokP(p)
  skipCom(p)
  if p.tok.xkind == pxAsgn:
    # `for i := a to/downto b do body` ->
    # `for _ in pforTo(i, a, b): body` / `for _ in pforDownto(i, a, b)`
    # The shim iterators take the loop variable by `var` and drive it,
    # keeping Pascal semantics: the declared variable IS the loop
    # variable (captures see the live value, break leaves it at the
    # broken-out value, after normal termination it holds b+1 / b-1).
    # The `var T` parameter also pins the iterator's T to the declared
    # type, so literal bounds need no casts.
    getTokP(p)
    skipCom(p)
    let a = parseExpr(p)
    var down = false
    if p.tok.xkind == pxTo:
      getTokP(p)
    elif p.tok.xkind == pxDownto:
      down = true
      getTokP(p)
    else:
      parError(p, "to/downto expected in for loop, got " & $p.tok)
    skipCom(p)
    let b = parseExpr(p)
    let iter = newNode(nkCall, b.info)
    iter.add(newIdentNode(if down: "pforDownto" else: "pforTo", b.info))
    iter.add(newIdentNode(forLoopVarName, result[0].info))
    iter.add(a)
    iter.add(b)
    result.add(iter)
  elif p.tok.xkind == pxIn:
    # for x in items do
    getTokP(p)
    skipCom(p)
    result.add(parseExpr(p))
  else:
    parError(p, ":= or `in` expected in for loop")
  p.eat(pxDo)
  skipCom(p)
  result.add(parseStmt(p))

proc parseRepeat*(p: var TParser): Node =
  # repeat ... until cond  ->  while true: ... if cond: break
  result = newNodeP(nkWhileStmt, p)
  getTokP(p)
  skipCom(p)
  result.add(newIdentNode("true", p.tok.info))
  let body = newNodeP(nkStmtList, p)
  while p.tok.xkind != pxEof and p.tok.xkind != pxUntil:
    body.add(parseStmt(p))
  p.eat(pxUntil)
  skipCom(p)
  let a = newNodeP(nkIfStmt, p)
  let b = newNodeP(nkElifBranch, p)
  let c = newNodeP(nkBreakStmt, p)
  c.add(emptyNode(p.tok.info))
  b.add(parseExpr(p))
  skipCom(p)
  b.add(c)
  a.add(b)
  body.add(a)
  result.add(body)

proc fixExit(p: var TParser, n: Node): bool =
  # legacy helper; `Exit` is now handled directly in parseStmt
  result = false

proc asStrOperand(n: Node): Node =
  ## `$n` for non-string operands; string literals pass through;
  ## pasW calls already produce Pascal-formatted strings
  if n.kind == nkStrLit:
    return n
  if n.kind == nkCall and n.len > 0 and n[0].kind == nkIdent and
      n[0].strVal == "pasW":
    return n
  let dollar = newNode(nkCall, n.info)
  dollar.add(newIdentNode("$", n.info))
  dollar.add(n)
  return dollar

proc mapStringBuiltins*(p: var TParser, n: Node): Node =
  ## expression-level rewrites of the 1-based string family
  ## (also invoked from mapBuiltinCall for statement-position calls)
  if n.kind != nkCall or n.len == 0: return n
  if n[0].kind != nkIdent: return n
  case n[0].strVal.toLowerAscii
  of "assigned":
    # Assigned(ev) -> ev.evProc != nil for method pointers
    if n.len == 2:
      let evName = methodPtrLeafName(n[1])
      if evName.len > 0 and p.methodPtrVars.hasKey(evName):
        var ne = newNode(nkInfix, n.info)
        ne.add(newIdentNode("!=", n.info))
        ne.add(newDotP(n[1], "evProc"))
        ne.add(newNode(nkNilLit, n.info))
        return ne
    return n
  of "pos":
    # Pos(sub, s) is 1-based (0 when absent); find is 0-based (-1 absent):
    # find(s, sub) + 1 maps exactly
    if n.len == 3:
      let sub = n[1]
      let str1 = n[2]
      var call = newNode(nkCall, n.info)
      call.add(newIdentNode("find", n.info))
      call.add(str1)
      call.add(sub)
      let plus = newNode(nkInfix, n.info)
      plus.add(newIdentNode("+", n.info))
      plus.add(call)
      plus.add(newIntNode(nkIntLit, 1, n.info))
      return plus
    return n
  of "ansipos":
    # AnsiPos(sub, s) - identical mapping to Pos
    if n.len == 3:
      let sub = n[1]
      let str1 = n[2]
      var call = newNode(nkCall, n.info)
      call.add(newIdentNode("find", n.info))
      call.add(str1)
      call.add(sub)
      let plus = newNode(nkInfix, n.info)
      plus.add(newIdentNode("+", n.info))
      plus.add(call)
      plus.add(newIntNode(nkIntLit, 1, n.info))
      return plus
    return n
  of "copy":
    # Copy(s, a[, b]) -> substr(s, a-1[, a-1 + (b-1)])
    n[0].strVal = "substr"
    if n.len >= 3:
      n[2] = p.decIndex(n[2])
      if n.len == 4:
        let minus = newNode(nkInfix, n.info)
        minus.add(newIdentNode("-", n.info))
        minus.add(n[3])
        minus.add(newIntNode(nkIntLit, 1, n.info))
        let plus = newNode(nkInfix, n.info)
        plus.add(newIdentNode("+", n.info))
        plus.add(n[2])
        plus.add(minus)
        n[3] = plus
    return n
  of "delete":
    # Delete(s, idx, cnt): no string delete in nimony - 1-based shim
    n[0].strVal = "strDelete"
    return n
  of "insert":
    # Insert(src, s, idx): 1-based shim
    n[0].strVal = "strInsert"
    return n
  else:
    return n

proc lowerFormatArrayOfConst*(p: var TParser, n: Node): Node =
  ## Delphi array-of-const: Format(fmt, [a, b, c]) lowers the bracket
  ## literal to toVrec calls so the shim's TVarRec openArray accepts it
  if n.kind == nkCall and n.len >= 1 and n[0].kind == nkIdent and
      n[0].strVal.toLowerAscii == "format" and n.len == 3 and
      n[2].kind == nkBracket:
    var lst = newNode(nkBracket, n[2].info)
    for e in n[2].sons:
      var c = newNode(nkCall, e.info)
      c.add(newIdentNode("toVrec", e.info))
      c.add(e)
      lst.add(c)
    n.sons = @[n[0], n[1], lst]
  result = n

proc writelnArg(p: var TParser, n: Node): Node =
  ## convert writeln operands that need Delphi/FPC rendering
  if n.kind == nkCharLit:
    let s = newNode(nkStrLit, n.info)
    s.strVal = ""
    s.strVal.add(n.strVal[0])
    return s
  if n.kind == nkIdent and
      p.varTypes.getOrDefault(n.strVal.toLowerAscii, "") == "bool":
    let c = newNode(nkCall, n.info)
    c.add(newIdentNode("delphiBool", n.info))
    c.add(n)
    return c
  if n.kind == nkCall and n.len > 0 and n[0].kind == nkIdent and
      p.syms.returnsBool.getOrDefault(n[0].strVal.toLowerAscii, false):
    let c = newNode(nkCall, n.info)
    c.add(newIdentNode("delphiBool", n.info))
    c.add(n)
    return c
  if n.kind == nkInfix and n.len == 3 and n[0].kind == nkIdent and
      n[0].strVal == "in":
    # `elem in set` yields a Boolean
    let c = newNode(nkCall, n.info)
    c.add(newIdentNode("delphiBool", n.info))
    c.add(n)
    return c
  if n.kind == nkIdent and n.strVal in ["true", "false"]:
    # a bare Boolean literal renders TRUE/FALSE
    let c = newNode(nkCall, n.info)
    c.add(newIdentNode("delphiBool", n.info))
    c.add(n)
    return c
  if n.kind == nkPrefix and n.len == 2 and n[0].kind == nkIdent and
      n[0].strVal == "not":
    # `not expr` is always Boolean
    let c = newNode(nkCall, n.info)
    c.add(newIdentNode("delphiBool", n.info))
    c.add(n)
    return c
  if n.kind == nkInfix and n.len == 3 and n[0].kind == nkIdent:
    let op = n[0].strVal
    if op == "not":
      # unary not is always Boolean
      let c = newNode(nkCall, n.info)
      c.add(newIdentNode("delphiBool", n.info))
      c.add(n)
      return c
    if op in ["and", "or", "xor"]:
      # wrap only when both operands are Boolean-shaped; integer
      # and/or/xor must keep its numeric result
      var allBool = true
      for i in 1 ..< n.len:
        let o = n[i]
        let boolish = (o.kind == nkIdent and
                       (o.strVal in ["true", "false"] or
                        p.varTypes.getOrDefault(o.strVal.toLowerAscii) == "bool"))
        if not boolish: allBool = false
      if allBool:
        let c = newNode(nkCall, n.info)
        c.add(newIdentNode("delphiBool", n.info))
        c.add(n)
        return c
  return n

proc rhsExprType(p: var TParser, n: Node): string =
  ## best-effort static type of an assignment RHS, used to insert
  ## Delphi Implicit-conversion calls; "" when unknown
  case n.kind
  of nkIntLit, nkCharLit:
    result = "int32"
  of nkIdent:
    result = p.varTypes.getOrDefault(n.strVal.toLowerAscii)
  of nkDotExpr:
    if n.len == 2 and n[0].kind == nkIdent and n[1].kind == nkIdent:
      let rv = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
      if rv.startsWith("record:"):
        result = p.fieldTypes.getOrDefault(rv[7 .. ^1].toLowerAscii &
                                           "." &
                                           n[1].strVal.toLowerAscii)
      else:
        result = p.fieldTypes.getOrDefault(n[0].strVal.toLowerAscii &
                                           "." &
                                           n[1].strVal.toLowerAscii)
    else: result = ""
  of nkInfix:
    if n.len == 3:
      let lt = rhsExprType(p, n[1])
      let rt = rhsExprType(p, n[2])
      if lt.startsWith("record:"): result = lt
      elif rt.startsWith("record:"): result = rt
      elif lt in ["float32", "float64"] or rt in ["float32", "float64"]:
        result = "float64"
      elif lt.len > 0 and rt.len > 0: result = "int32"
      else: result = ""
    else: result = ""
  else: result = ""

proc mapBuiltinCall*(p: var TParser, n: Node): Node =
  ## rewrite builtins that need argument changes:
  ## write(x) -> write(stdout, x); writeln(...) -> echo(...);
  ## Pos(sub, s) -> find(s, sub); Copy(s, a, b) -> substr(s, a, b);
  ## Exit / Exit(x) -> return / return x
  let m = mapStringBuiltins(p, n)
  if m != n: return m
  if n.kind != nkCall or n.len == 0: return n
  if n[0].kind != nkIdent: return n
  let name = n[0].strVal.toLowerAscii
  case name
  of "inc", "dec":
    # Delphi class operator Inc/Dec: a record operand with the
    # operator registered lowers to `x = opInc_cls(x)`
    if n.len == 2 and n[1].kind == nkIdent:
      let vt = p.varTypes.getOrDefault(n[1].strVal.toLowerAscii)
      if vt.startsWith("record:"):
        let opName = p.syms.getConvOp(vt[7 .. ^1], name, "", "")
        if opName.len > 0:
          let asgn = newNode(nkAsgn, n.info)
          asgn.add(n[1])
          let c = newNode(nkCall, n.info)
          c.add(newIdentNode(opName, n.info))
          c.add(n[1])
          asgn.add(c)
          return asgn
    # nimony's inc/dec require the offset to match the var's type;
    # Pascal offsets are plain int literals -> lower to an
    # assignment `x = x +/- cast` so the types line up
    if n.len >= 3:
      let target = n[1]
      if target.kind == nkIdent:
        let vt = p.varTypes.getOrDefault(target.strVal.toLowerAscii)
        if vt in ["int32", "int16", "int8", "uint8", "uint16", "uint32"]:
          let off = n[2]
          let castNode = newNode(nkCall, n.info)
          castNode.add(newIdentNode(vt, n.info))
          castNode.add(off)
          let op = if name == "inc": "+" else: "-"
          let inf = newNode(nkInfix, n.info)
          inf.add(newIdentNode(op, n.info))
          inf.add(target)
          inf.add(castNode)
          let asgn = newNode(nkAsgn, n.info)
          asgn.add(target)
          asgn.add(inf)
          return asgn
    return n
  of "write":
    n[0].strVal = "write"
    let stdoutNode = newIdentNode("stdout", n[0].info)
    var newSons: seq[Node] = @[n[0], stdoutNode]
    for i in 1 ..< n.len:
      newSons.add(n[i])
    n.sons = newSons
    if n.len > 3:
      # nimony's write takes one value; fold into a single string,
      # $-converting every operand that is not a string literal
      var folded: Node = asStrOperand(n[2])
      for i in 3 ..< n.len:
        let cat = newNode(nkInfix, n.info)
        cat.add(newIdentNode("&", n.info))
        cat.add(folded)
        cat.add(asStrOperand(n[i]))
        folded = cat
      n.sons = @[n[0], stdoutNode, folded]
    return n
  of "writeln":
    # Delphi/FPC render booleans TRUE/FALSE and chars bare; imported
    # `$` overloads do not resolve across nimony modules, so convert
    # the operand kinds we can detect instead of wrapping in `$`
    var i = 1
    while i < n.len:
      n[i] = writelnArg(p, n[i])
      inc i
    n[0].strVal = "echo"
    return n
  of "format":
    return lowerFormatArrayOfConst(p, n)
  of "exit":
    # Exit; / Exit(value);
    let ret = newNode(nkReturnStmt, n.info)
    if n.len == 2:
      ret.add(n[1])
    else:
      ret.add(emptyNode(n.info))
    return ret
  else:
    return n

proc isRoutineDefKind(k: NodeKind): bool {.inline.} =
  k in {nkProcDef, nkFuncDef, nkMethodDef}

proc parseLabelSection(p: var TParser) =
  ## `label L1, L2, 10;` — declared goto targets of the enclosing routine
  getTokP(p)                   # skip `label`
  skipCom(p)
  while true:
    if p.tok.xkind == pxSymbol:
      p.curLabels.add(p.tok.ident.toLowerAscii)
      getTokP(p)
    elif p.tok.xkind in {pxIntLit, pxInt64Lit}:
      p.curLabels.add($p.tok.iNumber)
      getTokP(p)
    else:
      parError(p, "label name expected, got " & $p.tok)
    skipCom(p)
    if p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
      continue
    break
  p.opt(pxSemiColon)
  skipCom(p)

proc parseLabeledStmt(p: var TParser): Node =
  ## `Name: statement` — a label definition; dissolved by lowerGotos
  let info = p.tok.info
  let name = if p.tok.xkind == pxSymbol: p.tok.ident
             else: $p.tok.iNumber
  getTokP(p)                   # skip the label name
  p.eat(pxColon)
  skipCom(p)
  let stmt = parseStmt(p)
  result = newNode(nkLabeledStmt, info)
  result.add(newIdentNode(name, info))
  result.add(stmt)

proc gotoMatches(n: Node, lab: string): bool {.inline.} =
  n.kind == nkGotoStmt and n.len > 0 and n[0].kind == nkIdent and
      n[0].strVal.toLowerAscii == lab

proc isLoopKind(k: NodeKind): bool {.inline.} =
  k in {nkWhileStmt, nkForStmt}

proc collectGotos(n: Node, lab: string, found: var seq[Node]) =
  ## every `goto lab` in the subtree, skipping nested routines
  if isRoutineDefKind(n.kind): return
  if gotoMatches(n, lab): found.add(n)
  for s in n.sons: collectGotos(s, lab, found)

proc collectAnyGoto(n: Node, found: var seq[Node]) =
  if isRoutineDefKind(n.kind): return
  if n.kind == nkGotoStmt: found.add(n)
  for s in n.sons: collectAnyGoto(s, found)

proc subtreeHas(n: Node, target: Node): bool =
  if n == target: return true
  for s in n.sons:
    if subtreeHas(s, target): return true
  return false

proc topLevelIndex(list: Node, g: Node): int =
  ## index of the direct son of `list` whose subtree contains `g`
  result = -1
  for i in 0 ..< list.len:
    if subtreeHas(list[i], g): return i

proc backwardBelowLoop(n: Node, lab: string, inLoop: bool): bool =
  ## true if a backward goto sits below a loop (continue would bind to
  ## the wrong loop)
  if isRoutineDefKind(n.kind): return false
  if gotoMatches(n, lab) and inLoop: return true
  let deeper = inLoop or isLoopKind(n.kind)
  for s in n.sons:
    if backwardBelowLoop(s, lab, deeper): return true
  return false

proc rewriteGotosTo(n: Node, lab, blockName: string, toBreak: bool) =
  ## replace `goto lab` with `break pasGotoX` / `continue` in-place
  if isRoutineDefKind(n.kind): return
  for i in 0 ..< n.len:
    if gotoMatches(n[i], lab):
      if toBreak:
        let b = newNode(nkBreakStmt, n[i].info)
        b.add(newIdentNode(blockName, n[i].info))
        n[i] = b
      else:
        n[i] = newNode(nkContinueStmt, n[i].info)
    else:
      rewriteGotosTo(n[i], lab, blockName, toBreak)

proc processLabel(p: var TParser, list: Node, idx: int) =
  ## restructure one label (list[idx]) and its gotos
  let labNode = list[idx]
  let lab = labNode[0].strVal.toLowerAscii
  var gotos: seq[Node] = @[]
  collectGotos(list, lab, gotos)
  if gotos.len == 0:
    list[idx] = labNode[1]     # declared but unused: dissolve the marker
    return
  var fwd = -1                 # earliest forward top-level son index
  var bwd = -1                 # latest backward top-level son index
  for g in gotos:
    let top = topLevelIndex(list, g)
    if top < idx:
      if fwd < 0 or top < fwd: fwd = top
    elif top >= idx:
      if top > bwd: bwd = top
  let info = labNode.info
  if fwd >= 0 and bwd >= 0:
    parError(p, "label '" & labNode[0].strVal &
        "' is targeted both forward and backward; not supported (v1)")
  # hidden block name is routine-local, so the label spelling suffices
  let blockName = "pasGoto" & labNode[0].strVal
  var rebuilt: seq[Node] = @[]
  if fwd >= 0:
    # forward: block pasGotoX: <region>; goto -> break pasGotoX
    var inner = newNode(nkStmtList, info)
    for j in fwd ..< idx: inner.add(list[j])
    rewriteGotosTo(inner, lab, blockName, true)
    var blk = newNode(nkBlockStmt, info)
    blk.add(newIdentNode(blockName, info))
    blk.add(inner)
    for j in 0 ..< fwd: rebuilt.add(list[j])
    rebuilt.add(blk)
    rebuilt.add(labNode[1])
  else:
    # backward: while true: <region>; goto -> continue
    if backwardBelowLoop(list, lab, false):
      parError(p, "backward goto to '" & labNode[0].strVal &
          "' crosses a loop boundary; not supported (v1)")
    var inner = newNode(nkStmtList, info)
    for j in idx .. bwd: inner.add(list[j])
    rewriteGotosTo(inner, lab, blockName, false)
    inner.add(newNode(nkBreakStmt, info))   # fall-through exits the loop
    var wh = newNode(nkWhileStmt, info)
    wh.add(newIdentNode("true", info))
    wh.add(inner)
    for j in 0 ..< idx: rebuilt.add(list[j])
    rebuilt.add(wh)
  for j in (if fwd >= 0: idx + 1 else: bwd + 1) ..< list.len:
    rebuilt.add(list[j])
  list.sons = rebuilt

proc findLabelDFS(p: var TParser, n: Node, owner: var Node,
                  idx: var int): bool =
  ## the first label definition; owner/idx = its enclosing statement list
  if isRoutineDefKind(n.kind): return false
  for i in 0 ..< n.len:
    let c = n[i]
    if c.kind == nkLabeledStmt:
      if n.kind != nkStmtList:
        parError(p, "a label must be defined inside a begin/end block (v1)")
      owner = n
      idx = i
      return true
    if findLabelDFS(p, c, owner, idx): return true
  return false

proc lowerGotos(p: var TParser, root: Node) =
  ## restructure all goto/label pairs under `root` into nimony named
  ## blocks (forward) and while/continue loops (backward); nested
  ## routines lower themselves and are skipped here
  while true:
    var owner = emptyNode(root.info)
    var idx = -1
    if not findLabelDFS(p, root, owner, idx): break
    processLabel(p, owner, idx)
  var leftover: seq[Node] = @[]
  collectAnyGoto(root, leftover)
  if leftover.len > 0:
    parError(p, "goto without a matching label in the same statement " &
        "block, or jumping into a nested block, is not supported (v1)")

proc withExprClass(p: var TParser, e: Node): string =
  ## class spelling of a with-expression ("" when unresolvable);
  ## v1 supports class-typed vars/params, `self`, ctor calls and
  ## member chains of class-typed fields
  if e.kind == nkIdent:
    if e.strVal.toLowerAscii == "self" and p.selfClass.len > 0:
      return p.selfClass
    # a hidden with-temp: its class is the scope's class
    for i in 0 ..< p.withDepth:
      if p.withTemps[i] == e.strVal:
        return p.withClasses[i]
    let vt = p.varTypes.getOrDefault(e.strVal.toLowerAscii)
    if vt.startsWith("record:"):
      return vt[7 .. ^1]
    if vt.startsWith("class:"):
      let sp = p.syms.classSpelling(vt[6..^1])
      if sp.len > 0:
        let ci = p.syms.classes.getOrDefault(sp.toLowerAscii)
        if not ci.isRef:
          return ""   # value objects copy: v1 gap (needs ptr lowering)
      return sp
    let pc = p.paramClassTypes.getOrDefault(e.strVal.toLowerAscii)
    if pc.len > 0: return pc
    # inside another with-scope the expression is evaluated there
    # (`with S, FOrigin do` — Delphi semantics)
    let k = e.strVal.toLowerAscii
    var i = p.withDepth - 1
    while i >= 0:
      var c = p.withClasses[i].toLowerAscii
      var guard = 0
      while c.len > 0 and guard < 100:
        let ci = p.syms.classes.getOrDefault(c)
        if ci.spelling.len == 0:
          # a record scope: its fields register in classFieldTypes
          let ft = p.classFieldTypes.getOrDefault(c & "." & k)
          if ft.len > 0: return ft
          break
        if ci.fieldSet.hasKey(k):
          let ft = p.classFieldTypes.getOrDefault(c & "." & k)
          if ft.len > 0: return ft
        c = ci.parent
        inc guard
      dec i
    # a bare field of the enclosing class
    if p.selfClass.len > 0:
      let ft = p.classFieldTypes.getOrDefault(
          p.selfClass.toLowerAscii & "." & k)
      if ft.len > 0: return ft
    if p.selfClass.len > 0:
      let ft = p.classFieldTypes.getOrDefault(
          p.selfClass.toLowerAscii & "." & k)
      if ft.len > 0: return ft
  elif e.kind == nkCall and e.len >= 1 and e[0].kind == nkDotExpr and
      e[0][0].kind == nkIdent and e[0][1].kind == nkIdent and
      e[0][1].strVal.toLowerAscii == "create":
    # `TSome.Create(...)` evaluates to a TSome instance
    return p.syms.classSpelling(e[0][0].strVal)
  elif e.kind == nkDotExpr and e.len == 2:
    let baseCls = p.withExprClass(e[0])
    if baseCls.len > 0 and e[1].kind == nkIdent:
      return p.classFieldTypes.getOrDefault(
          baseCls.toLowerAscii & "." & e[1].strVal.toLowerAscii)
  elif e.kind in {nkIndexExpr, nkBracket} and e.len == 2:
    # `Buckets[i]` / `Slice.Fields[i]`: the element class of an
    # array-typed base (v1: only class/record-element arrays resolve)
    return p.withExprClass(e[0])
  return ""

proc unitModuleSpelling(p: TParser, name: string): string =
  ## the module spelling a uses-clause unit name maps to, for
  ## unit-qualified calls like `DateUtils.Now` (mirrors the uses
  ## clause's shim mapping)
  result = name
  case name.toLowerAscii
  of "strutils": result = "passtrutils"
  of "math": result = "pasmath"
  of "dateutils": result = "pasdateutils"
  of "classes": result = "pasclasses"
  of "sysutils", "si_strings", "system": result = "systempas"
  else:
    result = p.unitFiles.getOrDefault(name.toLowerAscii, name)

proc withBaseText(e: Node): string =
  ## source-text rendering of a simple with-base (idents, dots,
  ## indexes, literals) for record-with qualification
  result = ""
  case e.kind
  of nkIdent: result = e.strVal
  of nkDotExpr:
    if e.len == 2 and e[0].kind in {nkIdent, nkDotExpr, nkBracket} and
        e[1].kind == nkIdent:
      result = withBaseText(e[0]) & "." & e[1].strVal
  of nkIndexExpr, nkBracket:
    if e.len >= 2:
      result = withBaseText(e[0]) & "[" & withBaseText(e[e.len - 1]) & "]"
  of nkIntLit: result = $e.intVal
  else: result = ""


proc withQualify(p: var TParser, n: Node): Node =
  ## qualify a bare identifier against the active with-scopes
  ## (innermost first, Delphi shadowing semantics)
  if n.kind != nkIdent or p.withDepth == 0: return n
  let k = n.strVal.toLowerAscii
  if k in ["self", "result", "true", "false", "nil"]: return n
  var i = p.withDepth - 1
  while i >= 0:
    var c = p.withClasses[i].toLowerAscii
    var guard = 0
    while c.len > 0 and guard < 100:
      # records qualify through the fieldTypes map
      if p.fieldTypes.hasKey(c & "." & k):
        return newDotP(newIdentNode(p.withTemps[i], n.info), n.strVal)
      let ci = p.syms.classes.getOrDefault(c)
      if ci.spelling.len == 0: break
      if ci.fieldSet.hasKey(k) or ci.routineSet.hasKey(k) or
          ci.ctorSet.hasKey(k):
        return newDotP(newIdentNode(p.withTemps[i], n.info), n.strVal)
      # properties of the class qualify too
      for pr in p.props:
        if pr.params == nil and pr.cls.toLowerAscii == c and
            pr.name.toLowerAscii == k:
          return newDotP(newIdentNode(p.withTemps[i], n.info), n.strVal)
      c = ci.parent
      inc guard
    dec i
  return n

proc parseWith(p: var TParser): Node =
  ## `with E1, E2 do stmt` -> nested statement list holding one hidden
  ## temp per expression; body idents that are members of a with-class
  ## are qualified against the innermost match. (Planned extension:
  ## Oxygene-style `with E as X do` names the temp explicitly.)
  result = newNodeP(nkStmtList, p)
  getTokP(p)                       # skip `with`
  skipCom(p)
  var pushed = 0
  while true:
    let e = parseExpr(p)
    let cls = p.withExprClass(e)
    if cls.len == 0:
      parError(p, "cannot determine the class of a with expression; " &
        "assign it to a variable first (v1 supports class-typed " &
        "variables, self and constructor calls)")
    inc p.withCounter
    let temp = "pasW" & $p.withCounter
    let info = p.tok.info
    # record withs are qualified DIRECTLY against the original
    # expression: records copy by value, so a hidden temp would
    # receive writes Delphi applies to the original
    var isRecord = p.recordTypes.hasKey(cls.toLowerAscii)
    # push the scope (fixed-size stack: nimony seqs have no pop);
    # record withs use the original expression as the qualifier
    var qualifier = temp
    if isRecord:
      # record withs qualify against the original expression text
      qualifier = withBaseText(e)
      if qualifier.len == 0:
        # complex record expression: fall back to the temp (v1:
        # writes through the with go to the temp, not the original)
        qualifier = temp
        isRecord = false
    if not isRecord:
      # class (or fallback) withs bind a hidden temp var
      let vd = newNode(nkVarSection, info)
      let d = newNode(nkIdentDefs, info)
      d.add(newIdentNode(temp, info))
      d.add(newIdentNode(cls, info))
      d.add(e)
      vd.add(d)
      result.add(vd)
    if p.withDepth < p.withTemps.len:
      p.withTemps[p.withDepth] = qualifier
      p.withClasses[p.withDepth] = cls
    else:
      p.withTemps.add(qualifier)
      p.withClasses.add(cls)
    inc p.withDepth
    inc pushed
    skipCom(p)
    if p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
      continue
    break
  p.eat(pxDo)
  skipCom(p)
  let body = parseStmt(p)
  if body.kind == nkStmtList:
    for s in body.sons: result.add(s)
  else:
    result.add(body)
  dec p.withDepth, pushed

proc checkSetLiteral(p: var TParser, n: Node): Node =
  ## if `n` is an array literal and the context wants a set, convert
  ## (the parser tracks var types for this)
  result = n

proc parseStmt*(p: var TParser): Node =
  # a label definition `Name:` / `10:` (declared in a label section)
  if (p.tok.xkind == pxSymbol or p.tok.xkind in {pxIntLit, pxInt64Lit}) and
      p.peekTok.xkind == pxColon:
    let name = if p.tok.xkind == pxSymbol: p.tok.ident.toLowerAscii
               else: $p.tok.iNumber
    if p.tok.xkind in {pxIntLit, pxInt64Lit} or name in p.curLabels:
      return parseLabeledStmt(p)
  case p.tok.xkind
  of pxEof:
    result = emptyNode(p.tok.info)
  of pxComment:
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = p.tok.literal
    getTokP(p)
  of pxCurlyDirLe, pxStarDirLe:
    if isHandledDirective(p):
      result = parseDirective(p)
    else:
      parError(p, p.tok.ident & " not allowed here")
      result = emptyNode(p.tok.info)
  of pxBegin:
    result = newNodeP(nkStmtList, p)
    getTokP(p)
    skipCom(p)
    while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
      # a conditional may span the begin/end boundary (Delphi guards
      # parts of one block per target); dead branches skip at the
      # token level, taken ones flow into the block
      if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        if declDirective(p):
          continue
        break
      let s = parseStmt(p)
      if s.kind != nkEmpty: result.add(s)
      if p.tok.xkind == pxSemiColon:
        getTokP(p)
        skipCom(p)
    p.eat(pxEnd)
    p.opt(pxDot)
  of pxIf:
    result = newNodeP(nkIfStmt, p)
    while true:
      getTokP(p)              # skip `if`/`else if`
      let branch = newNodeP(nkElifBranch, p)
      skipCom(p)
      branch.add(parseExpr(p))
      p.eat(pxThen)
      skipCom(p)
      while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        # `if C then {$IFDEF} ... {$ENDIF} begin` - a conditional
        # may wrap the statement boundary
        if declDirective(p):
          continue
        break
      branch.add(parseStmt(p))
      result.add(branch)
      skipCom(p)
      if p.tok.xkind == pxElse:
        getTokP(p)
        skipCom(p)
        if p.tok.xkind == pxIf:
          continue            # else if -> additional elif branch
        let elseBranch = newNodeP(nkElse, p)
        skipCom(p)
        elseBranch.add(parseStmt(p))
        result.add(elseBranch)
      break
  of pxWhile:
    result = newNodeP(nkWhileStmt, p)
    getTokP(p)
    skipCom(p)
    result.add(parseExpr(p))
    p.eat(pxDo)
    skipCom(p)
    result.add(parseStmt(p))
  of pxRepeat:
    result = parseRepeat(p)
  of pxCase:
    result = parseCase(p)
  of pxTry:
    result = parseTry(p)
  of pxFor:
    result = parseFor(p)
  of pxRaise:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind != pxSemiColon:
      # `raise SomeExc.Create(args)` / `raise excInstance`:
      # stash the instance in the current-exception slot, then raise
      # the mapped ErrorCode (nimony raise only transports ErrorCode)
      let e = parseExpr(p)
      var code = ""
      if e.kind == nkCall and e.len >= 1 and e[0].kind == nkDotExpr and
          e[0][1].kind == nkIdent and
          e[0][1].strVal.toLowerAscii == "create":
        let excName = e[0][0]
        if excName.kind == nkIdent:
          code = excSpelling(excName.strVal.toLowerAscii)
        elif excName.kind == nkDotExpr:
          code = excSpelling(excName[1].strVal.toLowerAscii)
      else:
        # re-raising a captured instance: map its class if it is one
        var base = e
        if base.kind == nkDotExpr and base.len == 2:
          base = base[1]
        if base.kind == nkIdent:
          let vt = p.varTypes.getOrDefault(base.strVal.toLowerAscii)
          if vt.startsWith("class:"):
            code = excSpelling(p.syms.classSpelling(vt[6..^1]))
      if code.len == 0: code = "Failure"
      let stmts = newNode(nkStmtList, p.tok.info)
      let stash = newNode(nkAsgn, p.tok.info)
      stash.add(newIdentNode("pasCurrentExc", p.tok.info))
      # cast[]: the upcast of a not-provably-non-nil call result trips
      # nimsem's nil-proof
      let stashCast = newNode(nkCast, p.tok.info)
      stashCast.add(newIdentNode("PasException", p.tok.info))
      stashCast.add(e)
      stash.add(stashCast)
      stmts.add(stash)
      let rn = newNode(nkRaiseStmt, p.tok.info)
      rn.add(newIdentNode(code, p.tok.info))
      stmts.add(rn)
      result = stmts
    else:
      # bare `raise;` re-raise: nimony's own handler re-raise
      result = newNodeP(nkRaiseStmt, p)
      result.add(emptyNode(p.tok.info))
    p.opt(pxSemiColon)
  of pxInherited:
    result = parseInherited(p)
  of pxWith:
    result = parseWith(p)
  of pxGoto:
    getTokP(p)                 # skip `goto`
    skipCom(p)
    if p.tok.xkind == pxSymbol:
      result = newNode(nkGotoStmt, p.tok.info)
      result.add(newIdentNode(p.tok.ident, p.tok.info))
      getTokP(p)
    elif p.tok.xkind in {pxIntLit, pxInt64Lit}:
      result = newNode(nkGotoStmt, p.tok.info)
      result.add(newIdentNode($p.tok.iNumber, p.tok.info))
      getTokP(p)
    else:
      parError(p, "label name expected after `goto`")
      result = emptyNode(p.tok.info)
    p.opt(pxSemiColon)
  of pxAsm:
    # inline assembler has no v1 backend: skip the block, keep parsing
    skipAsmBlock(p)
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# asm skipped"
  of pxLabel:
    # module level: labels of the program main body
    parseLabelSection(p)
    result = emptyNode(p.tok.info)
    result = emptyNode(p.tok.info)
  of pxExports:
    # exports clauses are irrelevant for a single-module translation
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# exports"
    while p.tok.xkind != pxEof and p.tok.xkind != pxSemiColon: getTokP(p)
    p.opt(pxSemiColon)
  of pxUses:
    result = parseUsesStmt(p)
  of pxInterface:
    getTokP(p)
    p.section = seInterface
    p.visibility = visPublic
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# interface"
  of pxImplementation:
    getTokP(p)
    p.section = seImplementation
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# implementation"
  of pxInitialization:
    getTokP(p)
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# initialization section:"
    let body = parseStmt(p)
    if body.kind == nkStmtList:
      result = body
    else:
      let l = newNodeP(nkStmtList, p)
      l.add(body)
      result = l
  of pxFinalization:
    getTokP(p)
    # module finalization: no nimony equivalent; keep as comment
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# finalization section skipped"
    discard parseStmt(p)
  of pxVar:
    result = parseVarSection(p)
  of pxThreadvar:
    # translate like a var section; the renderer adds {.threadvar.}
    result = parseVarSection(p)
  of pxConst, pxResourcestring:
    result = parseConstSection(p)
  of pxType:
    result = parseTypeSection(p)
  of pxProcedure, pxFunction, pxConstructor, pxDestructor, pxClass:
    # `class procedure/function/operator` implementations land here
    # too; the `class` prefix is consumed inside parseRoutine
    if p.tok.xkind == pxClass and
        p.peekTok().xkind notin {pxProcedure, pxFunction, pxOperator}:
      parError(p, "unsupported statement: class " & $p.peekTok() &
          " (v1 supports class procedure/function/operator)")
    result = parseRoutine(p, false)
  of pxProgram:
    # `program Name;` header - skip; a program is one implementation
    getTokP(p)
    while p.tok.xkind != pxEof and p.tok.xkind != pxSemiColon: getTokP(p)
    p.opt(pxSemiColon)
    p.section = seImplementation
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# program"
  of pxUnit:
    # `unit Name;` header - skip
    getTokP(p)
    while p.tok.xkind != pxEof and p.tok.xkind != pxSemiColon: getTokP(p)
    p.opt(pxSemiColon)
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "# unit"
  of pxProperty:
    result = parseProperty(p)
  else:
    # expression / assignment statement
    let info = parLineInfo(p)
    if p.tok.xkind == pxSymbol and
        p.tok.ident.toLowerAscii in ["break", "continue"]:
      # loop control (not lexer keywords in v1)
      result = if p.tok.ident.toLowerAscii == "break":
                 newNodeP(nkBreakStmt, p)
               else: newNodeP(nkContinueStmt, p)
      result.add(emptyNode(p.tok.info))
      getTokP(p)
      p.opt(pxSemiColon)
      skipCom(p)
      return
    let a = parseExpr(p)
    if p.tok.xkind == pxAsgn:
      getTokP(p)
      skipCom(p)
      let b = parseExpr(p)
      result = newNode(nkAsgn, info)
      result.add(a)
      result.add(b)
      let mpAsgn = p.rewriteMethodPtrAsgn(a, b, info)
      if mpAsgn.kind != nkEmpty:
        result = mpAsgn
      elif a.kind == nkIdent and p.varTypes.getOrDefault(
          a.strVal.toLowerAscii) == "set" and b.kind == nkBracket:
        # set literal assignment
        b.kind = nkCurly
      elif a.kind == nkIdent and b.kind in
          {nkIdent, nkIntLit, nkCharLit, nkInfix, nkPrefix}:
        # Delphi class operator Implicit: assignments whose sides
        # have known incompatible types insert the conversion call;
        # int-typed LHS with pure-literal arithmetic keeps the
        # width cast below
        let lhsVt = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
        let rhsTy = rhsExprType(p, b)
        var opName = ""
        if lhsVt.startsWith("record:"):
          let cls = lhsVt[7 .. ^1]
          if rhsTy.startsWith("record:"):
            let rcls = rhsTy[7 .. ^1]
            if rcls != cls:
              opName = p.syms.getConvOp(cls, "implicit", rcls, cls)
          elif rhsTy in ["int8", "uint8", "int16", "uint16", "int32",
                         "uint32", "int64", "uint64"]:
            opName = p.syms.getConvOp(cls, "implicit", "integer", cls)
          elif rhsTy in ["float32", "float64"]:
            opName = p.syms.getConvOp(cls, "implicit", "float64", cls)
        elif rhsTy.startsWith("record:") and lhsVt in
            ["int8", "uint8", "int16", "uint16", "int32", "uint32",
             "int64", "uint64", "float32", "float64"]:
          let rcls = rhsTy[7 .. ^1]
          opName = p.syms.getConvOp(rcls, "implicit", rcls, lhsVt)
        elif lhsVt in ["float32", "float64"] and rhsTy in
            ["int8", "uint8", "int16", "uint16", "int32", "uint32",
             "int64", "uint64"]:
          # Pascal widens int arithmetic into float targets silently;
          # nimony needs the explicit conversion
          let c = newNode(nkCall, b.info)
          c.add(newIdentNode(lhsVt, b.info))
          c.add(b)
          result[1] = c
        if opName.len > 0:
          let c = newNode(nkCall, b.info)
          c.add(newIdentNode(opName, b.info))
          c.add(b)
          result[1] = c
      if a.kind == nkIdent and b.kind in {nkInfix, nkCall, nkPrefix}:
        # (nkPrefix: a negative literal like `-512` types as int in
        # nimony - an int32 target needs the width cast too)
        # Pascal computes Integer arithmetic in the declared width;
        # nimony types pure-literal arithmetic as int (64), so
        # `x = 21 * 2` on an int32 x needs an explicit cast
        let lhsTy = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
        if lhsTy in ["int8", "uint8", "int16", "uint16", "int32",
                    "uint32", "int64", "uint64"]:
          let castN = newNode(nkCall, b.info)
          castN.add(newIdentNode(lhsTy, b.info))
          castN.add(b)
          result[1] = castN
      p.opt(pxSemiColon)
    elif a.kind == nkIdent and a.strVal.toLowerAscii == "exit":
      # bare `Exit;` statement
      result = newNode(nkReturnStmt, info)
      result.add(emptyNode(info))
      p.opt(pxSemiColon)
    else:
      result = mapBuiltinCall(p, a)
      if result.kind in {nkDotExpr, nkIdent}:
        # statement-level proc call without parentheses
        let call = newNode(nkCall, info)
        call.add(result)
        result = call
      if result.kind == nkCall and result.len >= 1 and
          result[0].kind == nkIdent and
          result[0].strVal in p.nestedProcs:
        # nested routines see `self` implicitly
        result.add(newIdentNode("self", info))
      if result.kind == nkCall and result.len >= 1 and
          p.tok.xkind != pxAsgn:
        # nimony requires discarding unused non-void results; the
        # callee may be a bare ident or a dot-call (shim methods like
        # TStringList.Add return the index)
        var callee = ""
        var recvClass = ""
        if result[0].kind == nkIdent:
          callee = result[0].strVal
        elif result[0].kind == nkDotExpr and result[0].len == 2 and
            result[0][1].kind == nkIdent:
          callee = result[0][1].strVal
          if result[0][0].kind == nkIdent:
            let rvt = p.varTypes.getOrDefault(
                result[0][0].strVal.toLowerAscii)
            if rvt.startsWith("class:"):
              recvClass = rvt[6 .. ^1].toLowerAscii
        var retFlag = false
        if recvClass.len > 0:
          # the receiver's class decides: the per-class key survives a
          # user routine clearing the global name (e.g. TFoo.Add vs
          # TStringList.Add)
          retFlag = p.syms.returnsValue.getOrDefault(
              recvClass & "." & callee.toLowerAscii,
              p.syms.returnsValue.getOrDefault(callee.toLowerAscii,
                  false))
        elif callee.len > 0:
          retFlag = p.syms.returnsValue.getOrDefault(callee.toLowerAscii,
              false)

        if callee.len > 0 and retFlag and
            callee.toLowerAscii != "inttostr":
          let d = newNode(nkDiscardStmt, info)
          d.add(result)
          result = d
      p.opt(pxSemiColon)
  skipCom(p)
  if result.kind == nkEmpty:
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = "#"

# ---------------------------------------------------------------------------
# post-passes

proc parseStmtList(p: var TParser): Node =
  ## statement sequence for directive bodies; stops at EOF or
  ## at {$else}/{$endif} which the caller handles
  result = newNodeP(nkStmtList, p)
  while true:
    case p.tok.xkind
    of pxEof:
      break
    of pxCurlyDirLe, pxStarDirLe:
      if not isHandledDirective(p): break
    else:
      discard
    let s = parseStmt(p)
    if s.kind != nkEmpty: result.add(s)
    p.opt(pxSemiColon)
    skipCom(p)
  if result.len == 1: result = result[0]

proc collectScopeNames(n: Node, names: var seq[string]) =
  ## collect the local names declared in a statement subtree
  case n.kind
  of nkVarSection, nkConstSection:
    for defs in n.sons:
      if defs.kind == nkIdentDefs:
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            names.add(defs[i].strVal.toLowerAscii)
  of nkForStmt:
    if n[0].kind == nkIdent:
      names.add(n[0].strVal.toLowerAscii)
  else:
    discard

proc isMemberName(p: TParser, cls, name: string): bool =
  ## true if `name` is a member (field/routine/property) of the class chain
  let key = cls.toLowerAscii
  let lower = name.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = p.syms.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.fieldSet.hasKey(lower) or ci.routineSet.hasKey(lower):
      return true
    k = ci.parent
    inc guard
  # properties
  for pr in p.props:
    if pr.cls.toLowerAscii == key and pr.name.toLowerAscii == lower:
      return true
  return false

proc isCtorName(p: TParser, cls, name: string): bool =
  p.syms.isCtorOf(cls, name)

proc selfQualifyInPlace(p: var TParser, n: Node,
                        scope: var seq[string]): Node

proc selfQualifyKids(p: var TParser, n: Node,
                     scope: var seq[string]): Node =
  result = n
  for i in 0 ..< n.len:
    n[i] = selfQualifyInPlace(p, n[i], scope)

proc selfQualifyInPlace(p: var TParser, n: Node,
                        scope: var seq[string]): Node =
  ## returns the (possibly wrapped) node with bare members qualified
  case n.kind
  of nkProcDef, nkFuncDef, nkMethodDef:
    var inner: seq[string] = @[]
    if n.len >= 3 and n[2].kind == nkFormalParams:
      for i in 1 ..< n[2].len:
        let d = n[2][i]
        if d.kind == nkIdentDefs:
          for j in 0 ..< d.len - 2:
            if d[j].kind == nkIdent:
              inner.add(d[j].strVal.toLowerAscii)
    if n.len > 0:
      let bodySon = n[n.len - 1]
      if bodySon.kind == nkStmtList:
        for s in bodySon.sons:
          if s.kind in {nkVarSection, nkConstSection, nkTypeSection}:
            collectScopeNames(s, inner)
        for i in 0 ..< bodySon.len:
          bodySon[i] = selfQualifyInPlace(p, bodySon[i], inner)
    return n
  of nkStmtList, nkElse, nkFinally, nkOfBranch, nkElifBranch:
    var inner = scope
    for s in n.sons:
      if s.kind in {nkVarSection, nkConstSection, nkTypeSection}:
        collectScopeNames(s, inner)
    return selfQualifyKids(p, n, inner)
  of nkIdent:
    let lower = n.strVal.toLowerAscii
    if p.qualClass.len > 0 and lower notin scope and
        lower != "self" and lower != "result" and
        not isCtorName(p, p.qualClass, n.strVal) and
        isMemberName(p, p.qualClass, n.strVal):
      let dot = newNode(nkDotExpr, n.info)
      dot.add(newIdentNode("self", n.info))
      dot.add(n)
      return dot
    return n
  of nkCommand:
    # procCall wrapper: qualify the argument's contents but keep the
    # inherited callee unqualified
    n[1] = selfQualifyInPlace(p, n[1], scope)
    return n
  of nkCall:
    var qualCallee = not n.noQualCallee
    if n.len >= 1 and qualCallee:
      n[0] = selfQualifyInPlace(p, n[0], scope)
    for i in 1 ..< n.len:
      n[i] = selfQualifyInPlace(p, n[i], scope)
    return n
  of nkDotExpr:
    # qualify the receiver, never the selector
    n[0] = selfQualifyInPlace(p, n[0], scope)
    return n
  of nkIndexExpr:
    # array-index offsets happen in adjustArrayIndices (one pass)
    return selfQualifyKids(p, n, scope)
  of nkTypeDef, nkTypeSection, nkImportStmt, nkProcTy, nkRefTy, nkPtrTy,
     nkObjectTy, nkEnumTy, nkArrayTy, nkSetTy, nkOpenArrayTy, nkRangeTy,
     nkFormalParams, nkIdentDefs, nkVarSection, nkConstSection, nkCommentStmt:
    return n
  else:
    return selfQualifyKids(p, n, scope)

proc rewriteClassProcCalls(p: var TParser, n: Node): Node =
  ## `TMath.Double(21)` -> `pasCm_TMath_Double(21)`;
  ## `obj.Bump(...)` -> the class method without the receiver;
  ## `TMath.FCount` -> the hoisted module-level var
  if n.kind == nkCall and n.len >= 1 and n[0].kind == nkDotExpr and
      n[0][0].kind == nkIdent and n[0][1].kind == nkIdent:
    let cls = p.syms.classSpelling(n[0][0].strVal)
    if cls.len > 0 and p.syms.isClassProcOf(cls, n[0][1].strVal):
      let newCall = newNode(nkCall, n.info)
      newCall.add(newIdentNode(
          p.syms.classProcName(cls, n[0][1].strVal), n.info))
      for i in 1 ..< n.len:
        newCall.add(n[i])
      result = newCall
      for i in 0 ..< result.len:
        if result.sons[i].len > 0:
          result.sons[i] = rewriteClassProcCalls(p, result.sons[i])
      return
    # instance receiver: `obj.Bump(...)` on a class-method name
    let vt = p.varTypes.getOrDefault(n[0][0].strVal.toLowerAscii)
    if vt.startsWith("class:") and
        p.syms.isClassProcOf(vt[6 ..^ 1], n[0][1].strVal):
      let newCall = newNode(nkCall, n.info)
      newCall.add(newIdentNode(
          p.syms.classProcName(vt[6 ..^ 1], n[0][1].strVal), n.info))
      for i in 1 ..< n.len:
        newCall.add(n[i])
      result = newCall
      for i in 0 ..< result.len:
        if result.sons[i].len > 0:
          result.sons[i] = rewriteClassProcCalls(p, result.sons[i])
      return
  if n.kind == nkDotExpr and n.len == 2 and n[0].kind == nkIdent and
      n[1].kind == nkIdent:
    let cls = p.syms.classSpelling(n[0].strVal)
    if cls.len > 0:
      if p.syms.isClassVarOf(cls, n[1].strVal):
        return newIdentNode(
            p.syms.classVarName(cls, n[1].strVal), n.info)
      if p.syms.isClassProcOf(cls, n[1].strVal):
        # zero-arg class method used as a value
        let newCall = newNode(nkCall, n.info)
        newCall.add(newIdentNode(
            p.syms.classProcName(cls, n[1].strVal), n.info))
        return newCall
  result = n
  for i in 0 ..< n.len:
    if n.sons[i].len > 0:
      n.sons[i] = rewriteClassProcCalls(p, n.sons[i])

proc classQualifyInPlace(p: var TParser, n: Node, cls: string,
                         scope: var seq[string]): Node

proc classQualifyKids(p: var TParser, n: Node, cls: string,
                      scope: var seq[string]): Node =
  result = n
  for i in 0 ..< n.len:
    n[i] = classQualifyInPlace(p, n[i], cls, scope)

proc classQualifyInPlace(p: var TParser, n: Node, cls: string,
                         scope: var seq[string]): Node =
  ## rewrite bare class-var / class-method names inside the class's
  ## own routines to their hoisted module-level spellings
  case n.kind
  of nkProcDef, nkFuncDef, nkMethodDef:
    return n          # nested routines carry their own context
  of nkIdent:
    let lower = n.strVal.toLowerAscii
    if lower notin scope and p.syms.isClassVarOf(cls, n.strVal):
      return newIdentNode(p.syms.classVarName(cls, n.strVal), n.info)
    return n
  of nkCommand:
    # procCall wrapper: keep the inherited callee unqualified
    n[1] = classQualifyInPlace(p, n[1], cls, scope)
    return n
  of nkCall:
    if n.len >= 1 and n[0].kind == nkIdent and
        n[0].strVal.toLowerAscii notin scope and
        p.syms.isClassProcOf(cls, n[0].strVal):
      let newCall = newNode(nkCall, n.info)
      newCall.add(newIdentNode(
          p.syms.classProcName(cls, n[0].strVal), n.info))
      for i in 1 ..< n.len:
        newCall.add(classQualifyInPlace(p, n[i], cls, scope))
      return newCall
    return classQualifyKids(p, n, cls, scope)
  of nkDotExpr:
    # qualify the receiver, never the selector
    n[0] = classQualifyInPlace(p, n[0], cls, scope)
    return n
  of nkStmtList, nkElse, nkFinally, nkOfBranch, nkElifBranch:
    var inner = scope
    for s in n.sons:
      if s.kind in {nkVarSection, nkConstSection, nkTypeSection}:
        collectScopeNames(s, inner)
    return classQualifyKids(p, n, cls, inner)
  else:
    return classQualifyKids(p, n, cls, scope)

proc hasClassStatics(p: TParser, cls: string): bool =
  ## true if `cls` or an ancestor declares class vars or class methods
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = p.syms.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.classVarSet.len > 0 or ci.classProcSet.len > 0:
      return true
    k = ci.parent
    inc guard
  return false

proc classQualifyAll(p: var TParser, module: Node) =
  ## bare class-var / class-method references inside a class's routines
  for def in module.sons:
    if def.kind in {nkProcDef, nkFuncDef, nkMethodDef} and
        def.defClass.len > 0:
      let cls = def.defClass
      if not hasClassStatics(p, cls):
        continue
      var scope: seq[string] = @[]
      if def.len >= 3 and def[2].kind == nkFormalParams:
        for j in 1 ..< def[2].len:
          let d = def[2][j]
          if d.kind == nkIdentDefs:
            for k in 0 ..< d.len - 2:
              if d[k].kind == nkIdent:
                scope.add(d[k].strVal.toLowerAscii)
      if def.len > 0 and def[def.len - 1].kind == nkStmtList:
        let body = def[def.len - 1]
        for s in body.sons:
          if s.kind in {nkVarSection, nkConstSection, nkTypeSection}:
            collectScopeNames(s, scope)
        for j in 0 ..< body.len:
          body[j] = classQualifyInPlace(p, body[j], cls, scope)

proc initValueResult(p: var TParser, module: Node) =
  ## nimony's result-init proof rejects procs whose body only assigns
  ## fields of a value-object result; give record-returning procs an
  ## explicit `result = default(T)` preamble
  for def in module.sons:
    if def.kind in {nkProcDef, nkFuncDef, nkMethodDef} and
        def.len >= 3 and def[2].kind == nkFormalParams:
      let ret = def[2][0]
      if ret.kind == nkIdent and
          p.recordTypes.hasKey(ret.strVal.toLowerAscii) and
          def.len > 0 and def[def.len - 1].kind == nkStmtList and
          def[def.len - 1].len > 0:
        let body = def[def.len - 1]
        var asgn = newNode(nkAsgn, ret.info)
        asgn.add(newIdentNode("result", ret.info))
        var dcall = newNode(nkCall, ret.info)
        dcall.add(newIdentNode("default", ret.info))
        dcall.add(ret)
        asgn.add(dcall)
        var rebuilt: seq[Node] = @[asgn]
        for s in body.sons:
          rebuilt.add(s)
        body.sons = rebuilt

proc selfQualifyAll*(p: var TParser, module: Node) =
  for i in 0 ..< module.len:
    let def = module[i]
    if def.kind in {nkProcDef, nkFuncDef, nkMethodDef}:
      var scope: seq[string] = @[]
      if def.len >= 3 and def[2].kind == nkFormalParams:
        for j in 1 ..< def[2].len:
          let d = def[2][j]
          if d.kind == nkIdentDefs:
            for k in 0 ..< d.len - 2:
              if d[k].kind == nkIdent:
                scope.add(d[k].strVal.toLowerAscii)
      let savedClass = p.qualClass
      p.qualClass = def.defClass
      if def.len > 0 and def[def.len - 1].kind == nkStmtList:
        let body = def[def.len - 1]
        for s in body.sons:
          if s.kind in {nkVarSection, nkConstSection, nkTypeSection}:
            collectScopeNames(s, scope)
        for j in 0 ..< body.len:
          body[j] = selfQualifyInPlace(p, body[j], scope)
      p.qualClass = savedClass

proc adjustArrayIndicesInPlace(p: var TParser, n: Node): Node =
  ## Pascal arrays keep their declared low bound (`array[1..5]`); nimony's
  ## runtime index check assumes 0-based storage, so every index access
  ## gets offset by the recorded low bound exactly once
  if n.kind == nkIndexExpr and n.len == 2:
    n[0] = adjustArrayIndicesInPlace(p, n[0])
    n[1] = adjustArrayIndicesInPlace(p, n[1])
    if n[0].kind == nkIdent:
      let low = p.arrayLows.getOrDefault(n[0].strVal.toLowerAscii, 0)
      if low != 0:
        let minus = newNode(nkInfix, n[1].info)
        minus.add(newIdentNode("-", n[1].info))
        minus.add(n[1])
        minus.add(newIntNode(nkIntLit, int64(low), n[1].info))
        n[1] = minus
    return n
  for i in 0 ..< n.len:
    n[i] = adjustArrayIndicesInPlace(p, n[i])
  return n

proc adjustArrayIndices*(p: var TParser, module: Node) =
  for i in 0 ..< module.len:
    discard adjustArrayIndicesInPlace(p, module[i])

proc rewriteClassAsgns*(p: var TParser, n: Node) =
  ## upcast assignments between differently typed class variables:
  ## `my = sec` -> `my = MyClass(sec)`
  if n.kind == nkAsgn and n[0].kind == nkIdent and n[1].kind == nkIdent:
    let lhsT = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
    let rhsT = p.varTypes.getOrDefault(n[1].strVal.toLowerAscii)
    if lhsT.startsWith("class:") and rhsT.startsWith("class:") and
        lhsT != rhsT:
      let castCall = newNode(nkCall, n.info)
      castCall.add(newIdentNode(lhsT[6..^1], n.info))
      castCall.add(n[1])
      n[1] = castCall
      return
  for s in n.sons:
    rewriteClassAsgns(p, s)

proc rewriteCtorCalls*(p: var TParser, n: Node): Node =
  ## `Second.create(v)` -> `create(Second(), v)`; an inherited
  ## constructor call gets an explicit cast back to the subclass
  if n.kind == nkCall and n.len >= 1 and n[0].kind == nkDotExpr and
      n[0][0].kind in {nkIdent, nkIndexExpr} and n[0][1].kind == nkIdent:
    let cls = if n[0][0].kind == nkIdent: n[0][0].strVal
              else: n[0][0][0].strVal
    let name = n[0][1].strVal
    if p.syms.isCtorOf(cls, name):
      # the prelude exception ctor is named pasExcCreate in systempas
      # (a user `create` would shadow it)
      let callee = if p.syms.findCtorClass(cls, name) == "PasException":
          "pasExcCreate"
        else:
          name
      let newCall = newNode(nkCall, n.info)
      newCall.add(newIdentNode(callee, n.info))
      let ctor = newNode(nkCall, n.info)
      ctor.add(n[0][0])
      newCall.add(ctor)
      for i in 1 ..< n.len:
        newCall.add(n[i])
      # an inherited constructor returns the declaring class; cast back
      let decl = p.syms.findCtorClass(cls, name)
      if decl.len > 0 and decl.toLowerAscii != cls.toLowerAscii:
        let castCall = newNode(nkCall, n.info)
        castCall.add(newIdentNode(cls, n.info))
        castCall.add(newCall)
        result = castCall
      else:
        result = newCall
      return
  if n.kind == nkDotExpr and n.len == 2 and n[0].kind == nkIdent and
      n[1].kind == nkIdent and p.syms.isCtorOf(n[0].strVal, n[1].strVal):
    # zero-arg constructor used as a value: `c := TClass.Create`
    let newCall = newNode(nkCall, n.info)
    newCall.add(newIdentNode(n[1].strVal, n.info))
    let ctor = newNode(nkCall, n.info)
    ctor.add(newIdentNode(n[0].strVal, n.info))
    newCall.add(ctor)
    result = newCall
    return
  result = n
  for i in 0 ..< n.len:
    if n.sons[i].len > 0:
      n.sons[i] = rewriteCtorCalls(p, n.sons[i])

proc rewriteArrayProps*(p: var TParser, n: Node) =
  ## `obj.myArr[idx]` -> `myArr(obj, idx)`; `obj.myArr[idx] := v` ->
  ## `setMyArr(obj, idx, v)`; `obj[idx]` (default property) likewise
  if n.kind == nkAsgn and n[0].kind == nkIndexExpr and n[0].len >= 2:
    let lhs = n[0]
    if lhs[0].kind == nkDotExpr and lhs[0][1].kind == nkIdent:
      let propName = lhs[0][1].strVal.toLowerAscii
      for pr in p.props:
        if pr.params != nil and pr.name.toLowerAscii == propName and
            pr.writeId.len > 0:
          let call = newNode(nkCall, n.info)
          call.add(newIdentNode(pr.writeId, n.info))
          call.add(lhs[0][0])
          for i in 1 ..< lhs.len:
            call.add(lhs[i])
          call.add(n[1])
          n.kind = nkCall
          n.sons = @[]
          for s in call.sons: n.sons.add(s)
          return
    elif lhs[0].kind == nkIdent:
      let vt = p.varTypes.getOrDefault(lhs[0].strVal.toLowerAscii)
      if vt.startsWith("class:"):
        let arrProp = p.syms.getArrayProp(vt[6..^1])
        if arrProp.arrName.len > 0 and arrProp.arrSetter.len > 0:
          let call = newNode(nkCall, n.info)
          call.add(newIdentNode(arrProp.arrSetter, n.info))
          call.add(lhs[0])
          for i in 1 ..< lhs.len:
            call.add(lhs[i])
          call.add(n[1])
          n.kind = nkCall
          n.sons = @[]
          for s in call.sons: n.sons.add(s)
          return
  if n.kind == nkIndexExpr and n.len >= 2 and n[0].kind == nkDotExpr and
      n[0][1].kind == nkIdent:
    let propName = n[0][1].strVal.toLowerAscii
    for pr in p.props:
      if pr.params != nil and pr.name.toLowerAscii == propName and
          pr.readId.len > 0:
        let call = newNode(nkCall, n.info)
        call.add(newIdentNode(pr.readId, n.info))
        call.add(n[0][0])
        for i in 1 ..< n.len:
          call.add(n[i])
        n.kind = nkCall
        n.sons = @[]
        for s in call.sons: n.sons.add(s)
        return
  for s in n.sons:
    rewriteArrayProps(p, s)

proc genPropertyAccessors*(p: var TParser, module: Node) =
  ## generate accessor templates for all properties
  for pr in p.props:
    if pr.cls.len == 0: continue
    if pr.params != nil:
      # array properties are resolved directly to their accessors by
      # rewriteArrayProps; no template is generated
      discard
    else:
      # read accessor: template name(self: C): T = ...
      if pr.readId.len > 0:
        let t = newNode(nkTemplateDef, pr.typ.info)
        t.add(exSymbol(newIdentNode(pr.name, pr.typ.info), pr.isPublic))
        t.add(emptyNode(pr.typ.info))
        let params = newNode(nkFormalParams, pr.typ.info)
        params.add(pr.typ)
        let selfDef = newNode(nkIdentDefs, pr.typ.info)
        selfDef.add(newIdentNode("self", pr.typ.info))
        selfDef.add(newIdentNode(pr.cls, pr.typ.info))
        selfDef.add(emptyNode(pr.typ.info))
        params.add(selfDef)
        t.add(params)
        t.add(emptyNode(pr.typ.info))
        t.add(emptyNode(pr.typ.info))
        let body = newNode(nkStmtList, pr.typ.info)
        let readLower = pr.readId.toLowerAscii
        let clsKey = pr.cls.toLowerAscii
        var isRoutine = false
        let ciRead = p.syms.classes.getOrDefault(clsKey)
        isRoutine = ciRead.routineSet.hasKey(readLower)
        let access = newNode(nkDotExpr, pr.typ.info)
        access.add(newIdentNode("self", pr.typ.info))
        access.add(newIdentNode(pr.readId, pr.typ.info))
        if isRoutine:
          let call = newNode(nkCall, pr.typ.info)
          call.add(access)
          body.add(call)
        else:
          body.add(access)
        t.add(body)
        module.add(t)
      # write accessor: template `name =`(self: C; v: T) = ...
      if pr.writeId.len > 0:
        let t = newNode(nkTemplateDef, pr.typ.info)
        let setterName = newIdentNode(pr.name & " =", pr.typ.info)
        t.add(exSymbol(setterName, pr.isPublic))
        t.add(emptyNode(pr.typ.info))
        let params = newNode(nkFormalParams, pr.typ.info)
        params.add(emptyNode(pr.typ.info))
        let selfDef = newNode(nkIdentDefs, pr.typ.info)
        selfDef.add(newIdentNode("self", pr.typ.info))
        selfDef.add(newIdentNode(pr.cls, pr.typ.info))
        selfDef.add(emptyNode(pr.typ.info))
        params.add(selfDef)
        let vDef = newNode(nkIdentDefs, pr.typ.info)
        vDef.add(newIdentNode("v", pr.typ.info))
        vDef.add(pr.typ)
        vDef.add(emptyNode(pr.typ.info))
        params.add(vDef)
        t.add(params)
        t.add(emptyNode(pr.typ.info))
        t.add(emptyNode(pr.typ.info))
        let body = newNode(nkStmtList, pr.typ.info)
        let writeLower = pr.writeId.toLowerAscii
        let clsKey = pr.cls.toLowerAscii
        var isRoutine = false
        let ciWrite = p.syms.classes.getOrDefault(clsKey)
        isRoutine = ciWrite.routineSet.hasKey(writeLower)
        if isRoutine:
          let call = newNode(nkCall, pr.typ.info)
          call.add(newIdentNode(pr.writeId, pr.typ.info))
          call.add(newIdentNode("self", pr.typ.info))
          call.add(newIdentNode("v", pr.typ.info))
          body.add(call)
        else:
          let asgn = newNode(nkAsgn, pr.typ.info)
          let access = newNode(nkDotExpr, pr.typ.info)
          access.add(newIdentNode("self", pr.typ.info))
          access.add(newIdentNode(pr.writeId, pr.typ.info))
          asgn.add(access)
          asgn.add(newIdentNode("v", pr.typ.info))
          body.add(asgn)
        t.add(body)
        module.add(t)

# ---------------------------------------------------------------------------
# unit driver

proc wrapMemberCalls*(p: var TParser, n: Node): Node =
  ## Pascal allows calling a member function without parentheses
  ## (`x := obj.Value`); nimony needs the call. Wrap a member access
  ## whose selector is a routine of the receiver's class into a call.
  if n.kind == nkDotExpr and n.len == 2 and n[0].kind == nkIdent and
      n[1].kind == nkIdent:
    let vt = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
    if vt.startsWith("class:") and p.syms.isRoutineOf(vt[6..^1], n[1].strVal):
      let call = newNode(nkCall, n.info)
      call.add(n)
      return call
    return n
  for i in 0 ..< n.len:
    if n.kind == nkCall and i == 0:
      continue  # the callee of a call is already the call target
    if n.sons[i].len > 0:
      n.sons[i] = wrapMemberCalls(p, n.sons[i])
  return n

proc parseUnit*(p: var TParser): Node =
  ## parse a whole unit/program; returns the module statement list with
  ## all post-passes applied
  # the prelude runtime modules are always importable: register their
  # exported spellings for case-insensitive resolution
  absorbNimModule(p, "systempas")
  absorbNimModule(p, "pasdatetime")
  while p.tok.xkind != pxEof:
    if p.tok.xkind == pxEnd:
      # unit/program terminator: a bare `end.` with no begin-block
      getTokP(p)
      p.opt(pxDot)
      break
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # directives between unit-level declarations: conditionals
      # branch at parse time, orphan {$endif}/{$else} of a branch
      # whose tokens already flowed are consumed, the rest skipped
      if declDirective(p):
        continue
    let s = parseStmt(p)
    # a conditional directive's branch statements arrive as an
    # nkStmtList - splice them so unit-level declarations register
    if s.kind == nkStmtList and s.strVal == "#condsplice":
      for k in 0 ..< s.len:
        p.module.add(s[k])
    elif s.kind != nkEmpty and s.strVal != "#":
      p.module.add(s)
    elif s.kind != nkCommentStmt:
      p.module.add(s)
    p.opt(pxSemiColon)
    skipCom(p)
  # a module-level begin block is the program main body
  for i in 0 ..< p.module.len:
    if p.module[i].kind == nkStmtList:
      p.lowerGotos(p.module[i])
  # Delphi's inherited TObject.Create: synthesize a default no-arg
  # constructor for ref classes that declare none (deterministic order)
  var ctorless: seq[string] = @[]
  for key, ci in p.syms.classes:
    # only when neither the class nor any ancestor declares a
    # constructor; otherwise the inherited-ctor path applies
    if ci.isRef and ci.genericOf.len == 0 and
        p.syms.findCtorClass(key, "create").len == 0:
      ctorless.add(key)
  var sortedCtorless: seq[string] = @[]
  for key in ctorless:
    var pos = sortedCtorless.len
    for i in 0 ..< sortedCtorless.len:
      if key < sortedCtorless[i]:
        pos = i
        break
    var rebuilt: seq[string] = @[]
    for i in 0 ..< sortedCtorless.len:
      if i == pos: rebuilt.add(key)
      rebuilt.add(sortedCtorless[i])
    if pos == sortedCtorless.len: rebuilt.add(key)
    sortedCtorless = rebuilt
  var synthCtors: seq[Node] = @[]
  for key in sortedCtorless:
    let ci = p.syms.classes.getOrDefault(key)
    let info = TLineInfo(line: 0, col: 0, file: p.module.info.file)
    var def = newNode(nkProcDef, info)
    def.add(exSymbol(newIdentNode("create", info), false))
    var selfTy: Node = newIdentNode(ci.spelling, info)
    if ci.typeParams.len > 0:
      var tv = newNode(nkBracket, info)
      for t in ci.typeParams:
        tv.add(newIdentNode(t, info))
      def.add(tv)
      selfTy = newNode(nkIndexExpr, info)
      selfTy.add(newIdentNode(ci.spelling, info))
      for t in ci.typeParams:
        selfTy.add(newIdentNode(t, info))
    else:
      def.add(emptyNode(info))
    var fp = newNode(nkFormalParams, info)
    fp.add(selfTy)
    var sd = newNode(nkIdentDefs, info)
    sd.add(newIdentNode("self", info))
    sd.add(selfTy)
    sd.add(emptyNode(info))
    fp.add(sd)
    def.add(fp)
    def.add(emptyNode(info))
    def.add(emptyNode(info))
    var body = newNode(nkStmtList, info)
    var asgn = newNode(nkAsgn, info)
    asgn.add(newIdentNode("result", info))
    asgn.add(newIdentNode("self", info))
    body.add(asgn)
    def.add(body)
    p.syms.addCtor(key, "create")
    synthCtors.add(def)
  # interface dispatch roots: nimony `method`s with discard bodies,
  # deterministically ordered like the synthesized constructors
  var intfKeys: seq[string] = @[]
  for key, ci in p.syms.classes:
    if ci.isInterface:
      intfKeys.add(key)
  var sortedIntf: seq[string] = @[]
  for key in intfKeys:
    var pos = sortedIntf.len
    for i in 0 ..< sortedIntf.len:
      if key < sortedIntf[i]:
        pos = i
        break
    var rebuilt: seq[string] = @[]
    for i in 0 ..< sortedIntf.len:
      if i == pos: rebuilt.add(key)
      rebuilt.add(sortedIntf[i])
    if pos == sortedIntf.len: rebuilt.add(key)
    sortedIntf = rebuilt
  for key in sortedIntf:
    for def in p.intfSigs.getOrDefault(key):
      p.module.add(def)
  genPropertyAccessors(p, p.module)
  initValueResult(p, p.module)
  selfQualifyAll(p, p.module)
  adjustArrayIndices(p, p.module)
  classQualifyAll(p, p.module)
  # class vars: hoist the module-level storage after the last type
  # section so routines (declared later) see it
  if p.classVarHoist.len > 0:
    var vs = newNode(nkVarSection, p.module.info)
    for v in p.classVarHoist:
      vs.add(v)
    var lastType = -1
    for i in 0 ..< p.module.len:
      if p.module[i].kind == nkTypeSection:
        lastType = i
    var rebuilt: seq[Node] = @[]
    if lastType < 0:
      rebuilt.add(vs)
    for i in 0 ..< p.module.len:
      rebuilt.add(p.module[i])
      if i == lastType:
        rebuilt.add(vs)
    p.module.sons = rebuilt
  for i in 0 ..< p.module.len:
    rewriteClassAsgns(p, p.module[i])
    p.module.sons[i] = rewriteCtorCalls(p, p.module[i])
    p.module.sons[i] = rewriteClassProcCalls(p, p.module[i])
    rewriteArrayProps(p, p.module[i])
  # move the generated property accessors before the first plain
  # statement (the main block), so nimony sees them before their use
  var accessors: seq[Node] = @[]
  var rest: seq[Node] = @[]
  for i in 0 ..< p.module.len:
    let n = p.module[i]
    if n.kind == nkTemplateDef:
      accessors.add(n)
    else:
      rest.add(n)
  var final: seq[Node] = @[]
  var inserted = false
  for n in rest:
    if not inserted and n.kind notin {nkImportStmt, nkCommentStmt,
        nkTypeSection, nkVarSection, nkConstSection, nkProcDef, nkFuncDef,
        nkMethodDef, nkTemplateDef, nkWhenExpr}:
      for c in synthCtors: final.add(c)
      for a in accessors: final.add(a)
      inserted = true
    final.add(n)
  if not inserted:
    for c in synthCtors: final.add(c)
    for a in accessors: final.add(a)
  var m = newNode(nkStmtList, p.module.info)
  for n in final: m.add(n)
  discard wrapMemberCalls(p, m)
  # wrap top-level statement blocks in try/except: raising procs may
  # only be called inside try, and an uncaught Delphi exception reports
  # the instance message (M4-2)
  var m2 = newNode(nkStmtList, p.module.info)
  for n in m.sons:
    if n.kind == nkStmtList:
      let wrapper = newNode(nkTryStmt, n.info)
      wrapper.add(n)
      let eb = newNode(nkExceptBranch, n.info)
      eb.add(newIdentNode("ErrorCode", n.info))
      eb.add(newIdentNode("pasUncaught", n.info))
      let hbody = newNode(nkStmtList, n.info)
      let iff = newNode(nkIfStmt, n.info)
      let elifb = newNode(nkElifBranch, n.info)
      let cond = newNode(nkInfix, n.info)
      cond.add(newIdentNode("!=", n.info))
      cond.add(newIdentNode("pasCurrentExc", n.info))
      cond.add(newNode(nkNilLit, n.info))
      elifb.add(cond)
      let rep = newNode(nkCall, n.info)
      rep.add(newIdentNode("echo", n.info))
      let msgcat = newNode(nkInfix, n.info)
      msgcat.add(newIdentNode("&", n.info))
      let pre = newNode(nkStrLit, n.info)
      pre.strVal = "Exception: "
      msgcat.add(pre)
      let dot = newNode(nkDotExpr, n.info)
      dot.add(newIdentNode("pasCurrentExc", n.info))
      dot.add(newIdentNode("Message", n.info))
      msgcat.add(dot)
      rep.add(msgcat)
      elifb.add(rep)
      iff.add(elifb)
      hbody.add(iff)
      eb.add(hbody)
      wrapper.add(eb)
      m2.add(wrapper)
    else:
      m2.add(n)
  result = m2
