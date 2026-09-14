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
    readPath*: seq[string]      ## dotted `read a.b` field path (empty if simple)
    isDefault*: bool
    isPublic*: bool

  UnitSet* = ref object
    files*: Table[string, bool]   ## unit file -> true once fully parsed, and
                                  ## -> false while it is being parsed, so a
                                  ## `uses` CYCLE (two corpus units)
                                  ## is cut instead of recursing forever

  TParser* = object
    lex*: ref TLexer
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
    outerResultTy*: string      ## enclosing routine's result spelling, lowercased
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
    setTypes*: Table[string, bool]  ## lowercase set-alias type names
    arrayAliases*: Table[string, string] ## array alias -> element spelling
    arrayVarElems*: Table[string, string] ## array var -> element spelling
    arrayVarElemTypes*: Table[string, string]
    ## array/open-array var/param -> ELEMENT type spelling (any element
    ## kind, not just class/record): drives the assignment narrowing cast
    ## for `digest[i] = i + 1` on an `array of Byte`.
    pointerAliases*: Table[string, string] ## P = ^T alias -> element
    routineReturns*: Table[string, string] ## routine name -> return spelling
    routineParams*: Table[string, seq[string]] ## routine name -> param spellings
    variantParams*: Table[string, seq[bool]] ## routine -> which params are Variant
    varRawTypes*: Table[string, string] ## var name -> *Pascal* type spelling
                                        ## (Currency is float64 in nimony but
                                        ## varCurrency in a Variant)
    routineParamDefaults*: Table[string, seq[Node]]
    ## interface-declared default param values by `class.name:argc` -
    ## the implementation redeclaration repeats them so nimony merges
    ## the pair instead of seeing two distinct overloads
    curTypeParams*: seq[string]  ## params of the type being declared
    genericArgDepth*: int  ## > 0 while parsing `<...>` generic args
    withTemps*: seq[string]     ## hidden per-with temporaries by depth
    withClasses*: seq[string]   ## class spelling per with depth
    withPtrs*: seq[bool]        ## with temp is an `addr` temp (needs `[]`)
    withIdxNames*: seq[string]  ## ptr-array with: index temp ("" = none)
    withDepth*: int             ## active with-scope count
    withCounter*: int           ## unique temp-name source
    methodPtrTypes*: Table[string, Node] ## method-ptr type name -> its formal params
    procTyTypes*: Table[string, bool] ## plain proc-type aliases (Assigned lowers to != nil)
    procVarTypes*: Table[string, bool] ## vars/fields of plain proc type
    methodPtrVars*: Table[string, Node] ## var/field/param/prop name -> its formal params
    thunkCounter*: int                 ## synthesized event-thunk serial
    ptrTmpCounter*: int                ## ptr-pool addr temp serial
    absorbed*: UnitSet               ## shared unit-absorption cycle guard
    unitFiles*: Table[string, string] ## lowercase unit name -> module file stem
    classOfProc*: string        ## class the current routine belongs to
    qualClass*: string          ## class context of the self-qualify pass
    nestedProcs*: seq[string]   ## nested routine names of the current proc
    arrayTypeLows*: Table[string, int]  ## alias type name -> declared low
    typeAliasTargets*: Table[string, string] ## simple alias -> target spelling
    constTypes*: Table[string, string] ## typed const name -> Pascal type spelling
    constParams*: Table[string, bool] ## current routine's `const` params
    withTempOwners*: Table[string, string] ## hidden with temp -> class spelling
    setElemTexts*: Table[string, string] ## set var -> nimony element type text
    procTyReturns*: Table[string, string]  ## plain proc-type alias -> return spelling
    procTyParams*: Table[string, Node]     ## plain proc-type alias -> formal params
    procVarReturns*: Table[string, string] ## proc-typed var/param -> return spelling

var condWhenStack: seq[int] = @[]
  ## One entry per OPEN conditional group of the parser currently running,
  ## outermost first: 1 = the group was FORWARDED to Nim as a `when` (its
  ## closers belong to parseIfDirAux), 0 = it was evaluated at parse time
  ## by declDirective, which then consumes its own closers.
  ##
  ## Module state rather than a TParser field: a field added to TParser
  ## makes the nimony frontend abort with a nifcore body assertion while
  ## compiling this very file. absorbUnit saves and clears the stack
  ## around a nested unit parse, so nesting stays correct when a `uses`
  ## clause sits inside a conditional.

# ---------------------------------------------------------------------------
# token plumbing

var includeStack: seq[ref TLexer] = @[]
  ## One parked lexer per OPEN `{$I ...}`/`{$INCLUDE ...}` splice,
  ## outermost first. The included file is lexed by its own lexer, so every
  ## token keeps its real file name and line - unlike a textual
  ## pre-expansion, which would shift every position after the include.
  ## The stack is module state for the same reason condWhenStack is (a
  ## TParser field breaks the nimony frontend compiling this file);
  ## openParser clears it and absorbUnit saves it around a nested parse.
  ##
  ## `seq[ref TLexer]`, not `seq[TLexer]`: a sequence of the inherited
  ## object type itself makes nimony's final codegen die inside the
  ## openArray array-converter ("expected expression but got: (baseobj
  ## ... BaseLexer ... dest)"). A sequence of refs sidesteps it.

proc includePop(p: var TParser): bool =
  ## the current lexer ran to EOF and belongs to an include: close it and
  ## resume the including file. False when the EOF is the real one.
  if includeStack.len == 0: return false
  closeLexer(p.lex[])
  p.lex = includeStack.pop()
  result = true

proc includeGetTok(p: var TParser, tok: var TToken) =
  ## getTok across include boundaries: an included file's EOF is not the
  ## end of the token stream, it is the return to the parent.
  while true:
    getTok(p.lex[], tok)
    if tok.xkind != pxEof or not includePop(p):
      break

proc includeNext(p: var TParser): TToken =
  ## read one token across include boundaries. Goes through a local so the
  ## `var` argument never aliases a field of the `var` parser parameter
  ## (nimony rejects that alias outright).
  result = default(TToken)
  includeGetTok(p, result)

proc resolveInclude(p: TParser, name: string): string =
  ## absolute path of an include file, or "" when it cannot be found.
  ## The caller turns "" into a hard parser error (except for the
  ## `{$I+}`/`{$I-}` switches, filtered before the call).
  ##
  ## Search order: the including file's directory, the CLI search paths,
  ## then a `package` subdirectory of either (Delphi projects keep shared
  ## includes beside the units, in `package/`, and those directories are
  ## not themselves on --path).
  if name.len == 0: return ""
  let nm = name.replace('\\', '/')
  var dirs: seq[string] = @[]
  let base = splitFile(p.lex.filename).dir
  if base.len > 0: dirs.add(base)
  dirs.add(".")
  for sp in p.searchPaths:
    if sp notin dirs: dirs.add(sp)
  var cands: seq[string] = @[]
  for d in dirs: cands.add(d / nm)
  for d in dirs: cands.add(d / "package" / nm)
  for c in cands:
    if fileExists(c): return c
    if splitFile(nm).ext.len == 0 and fileExists(c & ".inc"):
      return c & ".inc"
  result = ""

proc getTokP(p: var TParser) =
  if p.hasAhead:
    p.tok = p.aheadTok
    p.hasAhead = false
  else:
    p.tok = includeNext(p)

proc peekTok*(p: var TParser): TToken =
  if not p.hasAhead:
    p.aheadTok = includeNext(p)
    p.hasAhead = true
  result = p.aheadTok

proc pushInclude(p: var TParser, name: string): bool =
  ## splice an include in place: the current lexer is parked on the stack
  ## and the included file's first token becomes p.tok. Reads the first
  ## token directly (not through getTokP) so the helpers stay ordered
  ## without a forward declaration, which this toolchain lacks.
  let f = resolveInclude(p, name)
  if f.len == 0: return false
  var sub: ref TLexer
  new(sub)
  sub[].openLexer(f)
  includeStack.add(p.lex)
  p.lex = sub
  p.hasAhead = false
  p.tok = includeNext(p)
  result = true

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

proc skipDirectives(p: var TParser) =
  ## Step over a conditional directive in EXPRESSION position, leaving the
  ## taken branch's tokens for the caller:
  ##   Value.TypeInfo := PropInfo^.PropType{$IFNDEF FPC} ^ {$ENDIF} ...
  ##   SetFilePointer(h, Lo, {$IFDEF FPC}PLong(@r.Hi){$ELSE}@r.Hi{$ENDIF}, Origin)
  ## A `{$IF}` that an earlier `declDirective` already consumed emits only
  ## the trailing `{$ELSE}`/`{$ENDIF}`, and `declDirective` handles those
  ## too, so one call covers both shapes. `ifend` is the Delphi spelling
  ## for a `{$if}` group's close.
  var guard = 0
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe} and guard < 64:
    inc guard
    let kw = p.tok.ident.toLowerAscii
    if kw in ["ifdef", "ifndef", "if", "else", "endif", "ifend"]:
      discard declDirective(p)
    else:
      break
  skipCom(p)

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
  new(p.lex)
  p.lex[].openLexer(filename)
  # a fresh unit starts on a fresh include stack: a leftover entry from a
  # previous parse in the same process would close the wrong lexer
  includeStack.setLen(0)
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
  closeLexer(p.lex[])

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
proc parseTypeDefList(p: var TParser): Node
proc parseUsesList(p: var TParser): Node

proc typeLeafName(n: Node): string =
  ## the bare spelling of a possibly unit-QUALIFIED type name. The corpus
  ## writes `TaStringList = class(vStrLst.TaTemplateList)` in an
  ## implementation section; the ancestor registry is keyed by the leaf
  ## name, so a dotted parent used to be recorded as no parent at all -
  ## and `inherited IndexOf(x)` then emitted `cast[](self)`.
  if n.kind == nkIdent:
    result = n.strVal
  elif n.kind == nkDotExpr and n.len >= 1:
    result = typeLeafName(n[n.len - 1])
  else:
    result = ""

proc isHandledDirective(p: TParser): bool =
  result = false
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    case p.tok.ident.toLowerAscii
    of "else", "elseif", "endif", "ifend": result = false
    else: result = true

proc definedExpr(p: var TParser): Node =
  result = newNodeP(nkCall, p)
  result.add(newIdentNameNodeP("defined", p))
  if p.tok.xkind == pxSymbol:
    result.add(newIdentNode(p.tok.ident, p.tok.info))
    getTokP(p)
  else:
    parError(p, "identifier expected in directive")

const
  cmStmt = 0   ## a conditional's arm holds statements
  cmType = 1   ## a conditional's arm holds type definitions
  cmUses = 2   ## a conditional's arm holds entries of a uses clause

proc parseCondBody(p: var TParser, mode: int): Node =
  ## the body of one arm of a forwarded `{$if <expr>}` group. `mode` says
  ## which construct the group sits in, because the arm has to be parsed
  ## as the SAME construct: a group opened between two type definitions
  ## holds definitions, not statements.
  if mode == cmType: result = parseTypeDefList(p)
  elif mode == cmUses: result = parseUsesList(p)
  else: result = parseStmtList(p)

proc parseIfDirAux(p: var TParser, result: Node, mode = cmStmt) =
  ## the arms of a `{$if <expr>}` group that is forwarded to Nim as a
  ## `when`. Each arm is parsed as a complete body of the surrounding
  ## construct, so every closer of THIS group is consumed here; the
  ## declaration parsers inside an arm decline them (see declDirective
  ## and condWhenStack).
  result[0].add(parseCondBody(p, mode))
  # `{$elseif <expr>}` adds another guarded arm (Delphi 2007+); the
  # emitters render it as `elif` and walk every branch, so any number is
  # fine.
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe} and p.tok.ident.toLowerAscii == "elseif":
    let em = succ(p.tok.xkind)
    let branch = newNodeP(nkElifBranch, p)
    getTokP(p)                  # skip `{$elseif`
    branch.add(parseExpr(p))
    eatDirEnd(p, em)
    branch.add(parseCondBody(p, mode))
    result.add(branch)
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    let endMarker = succ(p.tok.xkind)
    if p.tok.ident.toLowerAscii == "else":
      let s = newNodeP(nkElse, p)
      eatDirEnd(p, endMarker)
      s.add(parseCondBody(p, mode))
      result.add(s)
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      let endMarker2 = succ(p.tok.xkind)
      # Delphi 2007 closes a `{$if}` with `{$ifend}`; newer compilers
      # accept `{$endif}` as well. Both end the same group here.
      if p.tok.ident.toLowerAscii in ["endif", "ifend"]:
        eatDirEnd(p, endMarker2)
      else:
        parError(p, "{$ifend} or {$endif} expected")

proc eatDirEnd(p: var TParser; endMarker: TTokKind) =
  ## consume the end marker of a compiler directive, tolerating a trailing
  ## LABEL between the directive name and the marker.
  ##
  ## Delphi accepts a symbolic label there and real code uses it heavily:
  ## `{$ENDIF CLR}`, `{$ELSE MSWINDOWS}`, `{$IFEND FPC}`, `{$ELSE
  ## OS2GCC}`. The lexer delivers the whole `{$...}` as an opener token
  ## plus the `}` end marker, so with a label present the marker is not
  ## the very next token and a bare `eat` reports "expected } but got:
  ## CLR" at the label.
  while p.tok.xkind != pxEof and p.tok.xkind != endMarker:
    getTokP(p)
  p.eat(endMarker)

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
      of "endif", "ifend":
        # Delphi closes a `{$IF}` with `{$IFEND}`; `{$ENDIF}` is accepted
        # too (and is what FPC writes). Without `ifend` here the skip runs
        # to EOF and swallows every following routine.
        if depth == 0:
          getTokP(p)
          eatDirEnd(p, em)
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
    eatDirEnd(p, emX)
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

proc parseIfDir(p: var TParser, endMarker: TTokKind, mode = cmStmt): Node =
  ## `{$if <expr>}` is forwarded to Nim verbatim as a `when`: a frontend
  ## cannot answer `declared()`, `sizeof()` or `CompilerVersion`, and
  ## guessing a branch is worse than handing the condition to a real
  ## compiler. `parseExpr` keeps it a Pascal expression, so the emitted
  ## `when` carries a valid condition rather than a source-level hack.
  result = newNodeP(nkWhenExpr, p)
  let branch = newNodeP(nkElifBranch, p)
  getTokP(p)                    # skip `{$if`
  branch.add(parseExpr(p))
  result.add(branch)
  eatDirEnd(p, endMarker)
  condWhenStack.add(1)          # this group's closers belong to us
  parseIfDirAux(p, result, mode)
  if condWhenStack.len > 0:
    condWhenStack.setLen(condWhenStack.len - 1)

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
      eatDirEnd(p, endMarker)
    of "undef":
      getTokP(p)
      let nm = parseCondName(p)
      p.syms.defines[nm] = false
      eatDirEnd(p, endMarker)
    of "i", "include":
      # `{$I file}` / `{$INCLUDE file}`: splice the file in at this point.
      # The lexer already captured the name (quoted or bare) into
      # tok.literal and left the close marker as the next token.
      #
      # Not every `{$I...}` is a file include. Delphi also spells the I/O
      # checking switches `{$I+}` / `{$I-}` with the same letter, and the
      # lexer's bare-name capture yields "+"/"-" for them; compiler-variable
      # includes look like `{$I %DATE%}`. Those are left alone. A REAL
      # include whose file does not exist is a hard error: the old
      # skip-it path silently dropped declarations and failed much later
      # with a confusing "undeclared identifier" on the use site.
      let nm = p.tok.literal
      let dirInfo = p.tok.info
      getTokP(p)                  # the close marker
      let isSwitch = nm.len == 0 or nm in ["+", "-"] or nm.startsWith("%")
      if isSwitch:
        getTokP(p)
      elif not pushInclude(p, nm):
        p.tok.info = dirInfo
        parError(p, "include file not found: " & nm)
    else:
      # skip unknown compiler directive
      eatDirEnd(p, endMarker)
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
    # `hasKey` covers both senses: a unit already parsed, and one whose
    # parse is still on the stack. The second is a `uses` cycle and must
    # be cut here - recursing into it never terminates.
    return
  p.unitFiles[unitName.toLowerAscii] = splitFile(unitFile).name
  p.absorbed.files[unitFile] = false   # wait marker, set true when done
  var up = default(TParser)
  # CLI defines propagate into units; source-level {$define}s stay
  # local to the unit that made them (Delphi-like scoping)
  var inheritedDefines: seq[string] = @[]
  for d, v in p.syms.defines:
    if v and not p.sourceDefines.hasKey(d):
      inheritedDefines.add(d)
  # the nested unit parses on its own conditional stack, so a `uses`
  # inside an open `{$if}` of the importer cannot corrupt either side
  let savedCond = condWhenStack
  condWhenStack.setLen(0)
  # same for includes: a unit absorbed from inside an included file must
  # not inherit (or drain) the importer's splice stack
  let savedInc = includeStack
  includeStack.setLen(0)
  # the CLI --path dirs must reach the nested unit: openParser assigns
  # searchPaths, so passing them here (not pre-filling up.searchPaths) is
  # what lets a used unit's own {$I}/{$INCLUDE} resolve along the path
  openParser(up, unitFile, p.flags, inheritedDefines, p.searchPaths)
  up.absorbed = p.absorbed   # shared ref: cycle guard works across units
  discard parseUnit(up)
  closeParser(up)
  condWhenStack = savedCond
  includeStack = savedInc
  p.absorbed.files[unitFile] = true    # fully parsed
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
      if t.find("= ptr ") >= 0 or t.find("= ^") >= 0:
        var ppe = t.find("= ptr ")
        if ppe >= 0: ppe = ppe + 6
        else:
          ppe = t.find("= ^")
          if ppe >= 0: ppe = ppe + 4
        if ppe >= 0:
          var e = ppe
          while e < t.len and t[e] in {' ', '\t'}: inc e
          var el = ""
          while e < t.len and t[e] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
            el.add(t[e]); inc e
          if el.len > 0:
            s.ptrAliases.add(name.toLowerAscii & ":" & el)
      # exported ref-object type: register as a class so receiver-typed
      # logic (varTypes "class:<t>", per-class method return keys) sees
      # shim types like TStringList; the `of Parent` spelling feeds the
      # ancestor walk (shim chains must not collapse to no parent)
      if t.find("ref object") >= 0:
        var par = ""
        let oq = t.find(" of ")
        if oq >= 0:
          var v = oq + 4
          while v < t.len and t[v] in {' ', '\t'}: inc v
          var pn = ""
          while v < t.len and t[v] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
            pn.add(t[v]); inc v
          par = pn
        s.registerClassShape(name, par, true)
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
          if cty.len > 0:
            # the shim method rides the declaring class's registry so
            # bare member calls inside methods self-qualify
            s.addRoutine(cty, name)
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
  # our own shim units: <pas2nimony>/runtime/ (bin -> pas2nimony);
  # placeholder companions live in runtime/placeholders/
  let rtDir = parentDir(parentDir(getAppFilename()))
  if rtDir.len > 0:
    cands.add(rtDir / "runtime" / modpath & ".nim")
    cands.add(rtDir / "runtime" / "placeholders" / modpath & ".nim")
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
      for k in p.syms.ptrAliases:
        let c = k.find(":")
        if c > 0: p.pointerAliases[k[0 ..< c]] = k[c + 1 ..< k.len]
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
  of "ifdef", "ifndef":
    # Answerable at parse time: the target's symbol set comes from the
    # command line (-d:MSWINDOWS ...) plus every {$define} seen so far,
    # so a frontend can decide this branch honestly.
    let kw = p.tok.ident.toLowerAscii
    getTokP(p)                  # skip the directive name
    let name = parseCondName(p)
    let defined = p.syms.defines.getOrDefault(name, false)
    let taken = if kw == "ifndef": not defined else: defined
    eatDirEnd(p, endMarker)     # closing brace (tolerates a label)
    if taken:
      # the live branch's tokens flow through the enclosing loop; its
      # trailing {$else}/{$endif} are handled by the cases below. The
      # group is recorded as parse-time EVALUATED, so those closers are
      # consumed here rather than left for an enclosing forwarded group.
      condWhenStack.add(0)
      skipCom(p)
      return true
    if skipCondBranch(p, endMarker):
      # the dead branch ran to a {$else}: that branch is LIVE - consume
      # the directive so its tokens flow through the loop
      getTokP(p)
      eatDirEnd(p, endMarker)
      condWhenStack.add(0)
      skipCom(p)
    # else: skipCondBranch consumed the {$endif}
    return true
  of "if":
    # `{$if <expr>}` is NOT answerable by a frontend: `declared()`,
    # `sizeof(Pointer)`, `CompilerVersion` all need semantic knowledge
    # that only a full Pascal compiler has. The construct is forwarded to
    # Nim as a `when` instead: `parseDirective` -> `parseIfDir` builds an
    # nkWhenExpr, emitted as `(when (elif COND BODY) ...)`. It is
    # deliberately NOT handled here - returning false lets the caller
    # fall through to `parseStmt`, where that node has a slot to land in.
    # A caller with only a token-stream slot reports its own error rather
    # than silently taking a branch that cannot be proven.
    return false
  of "else", "elseif":
    if condWhenStack.len > 0 and condWhenStack[^1] != 0:
      # the closer belongs to a group FORWARDED to Nim as a `when`:
      # parseIfDirAux owns it, so leave it for the enclosing statement
      # list to stop on.
      return false
    # the dead alternative of a branch evaluated at parse time: consume
    # the {$else} token, then skipCondBranch runs to the {$endif}
    getTokP(p)
    eatDirEnd(p, endMarker)
    discard skipCondBranch(p, endMarker)
    if condWhenStack.len > 0:
      condWhenStack.setLen(condWhenStack.len - 1)
    skipCom(p)
    return true
  of "endif", "ifend":
    if condWhenStack.len > 0 and condWhenStack[^1] != 0:
      # the closer of a forwarded `when` group: parseIfDirAux consumes it
      return false
    # the closing token of a branch whose taken side flowed here.
    # `ifend` is the Delphi/FPC spelling for a {$if} group, `endif` the
    # newer one; both end the group identically.
    getTokP(p)
    eatDirEnd(p, endMarker)
    if condWhenStack.len > 0:
      condWhenStack.setLen(condWhenStack.len - 1)
    skipCom(p)
    return true
  else:
    discard parseDirective(p)
    return true

proc parseUsesItem(p: var TParser, dest: Node): bool =
  ## one entry of a uses clause appended to `dest`: `Unit`, a dotted
  ## `System.SysUtils`, the `nim.x.y` nimony bridge, or `Unit in 'path'`.
  ## Returns true when an entry was consumed. The caller has already
  ## established that the token can start an entry.
  result = false
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
  # `uses Foo in '..\\Foo.pas';` - a source-file binding. It is
  # informational here: a unit is resolved by looking next to the
  # importer and along --path:<dir> for <Unit>.pas, so the path is
  # parsed and discarded. Without this the `in` token reached the
  # expression parser and the corpus's main program died with
  # "identifier expected in uses clause".
  if p.tok.xkind == pxIn:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind in {pxStrLit}:
      getTokP(p)
    elif p.tok.xkind == pxSymbol:
      # an unquoted path (rare, but Delphi accepts it in some forms)
      getTokP(p)
    else:
      parError(p, "file name expected after `in` in uses clause")
    skipCom(p)
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
      dest.add(newIdentNode(path, p.tok.info))
      absorbNimModule(p, path)
      result = true
  else:
    case unitName.toLowerAscii
    of "strutils":
      # our Delphi-shaped shim unit (M3)
      dest.add(newIdentNode("passtrutils", p.tok.info))
      absorbNimModule(p, "passtrutils")
      result = true
    of "math":
      dest.add(newIdentNode("pasmath", p.tok.info))
      absorbNimModule(p, "pasmath")
      result = true
    of "dateutils":
      # Delphi DateUtils naming layer over the TDateTime core (M3)
      dest.add(newIdentNode("pasdateutils", p.tok.info))
      absorbNimModule(p, "pasdateutils")
      result = true
    of "classes":
      # TStringList shim (M3); the rest of Classes is future work
      dest.add(newIdentNode("pasclasses", p.tok.info))
      absorbNimModule(p, "pasclasses")
      result = true
    of "sysutils", "si_strings", "system", "variants":
      # our runtime shim (systempas) provides the Delphi RTL helpers
      dest.add(newIdentNode("systempas", p.tok.info))
      result = true
    of "windows":
      # the Win32 compat shim carries the API surface the corpus's
      # MSWINDOWS branches reference (M11 Delphi oracle tier); the
      # spelling must match the shim's file name (Linux is
      # case-sensitive)
      dest.add(newIdentNode("Windows", p.tok.info))
      absorbNimModule(p, "Windows")
      result = true
    of "registry":
      # the Registry compat shim (runtime/placeholders/Registry.nim).
      # one corpus unit names it in `uses` without taking a symbol;
      # DBWebbrowser uses TRegistry directly. Explicit, like Windows, so
      # the import spelling matches the file name
      dest.add(newIdentNode("Registry", p.tok.info))
      absorbNimModule(p, "Registry")
      result = true
    else:
      # own unit: absorb its declarations, then import the module
      absorbUnit(p, unitName)
      if not p.unitFiles.hasKey(unitName.toLowerAscii):
        # no Pascal source: a runtime/ or placeholder shim .nim may
        # provide the unit (its exports seed routineArgs for the
        # paren-less 0-arg call machinery)
        absorbNimModule(p, unitName)
      # the import name must match the translated FILE name (Linux is
      # case-sensitive; Pascal unit/file casing may differ)
      let canonical = p.unitFiles.getOrDefault(unitName.toLowerAscii,
          p.syms.canonical(unitName))
      dest.add(newIdentNode(canonical, p.tok.info))
      result = true

proc parseUsesList(p: var TParser): Node =
  ## the entries of a uses clause contributed by ONE arm of a forwarded
  ## `{$if <expr>}` group. The arm becomes an import statement of its own,
  ## which the `when` then guards:
  ##
  ##   when declared(FormatSettings):
  ##     import FormatSettings
  ##
  ## A frontend cannot answer `declared(...)`, so the group is handed to a
  ## real compiler instead of being guessed at parse time.
  result = newNodeP(nkStmtList, p)
  let imp = newNodeP(nkImportStmt, p)
  var any = false
  while true:
    skipCom(p)
    if p.tok.xkind == pxEof: break
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: break  # closer / else
    if p.tok.xkind == pxSemiColon: break
    if p.tok.xkind == pxComma:
      getTokP(p)
      continue
    if p.tok.xkind != pxSymbol: break
    if parseUsesItem(p, imp): any = true
    p.opt(pxComma)
  if any:
    result.add(imp)
  else:
    # an empty arm is an empty block to nimony; emit a real statement
    result.add(newNodeP(nkDiscardStmt, p))

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
      let dir = p.tok.ident.toLowerAscii
      if dir == "if":
        # forwarded, not evaluated: a guarded import
        result.add(parseIfDir(p, succ(p.tok.xkind), cmUses))
        any = true
        if p.tok.xkind in {pxSemiColon, pxEof}: break
        continue
      if declDirective(p): continue
      break
    if p.tok.xkind == pxComma:
      # comma-first continuation: `,\n  NextUnit` (Delphi style)
      getTokP(p)
      continue
    if p.tok.xkind in {pxSemiColon, pxEof}:
      break
    if p.tok.xkind != pxSymbol:
      parError(p, "identifier expected in uses clause")
    if parseUsesItem(p, result): any = true
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

proc condArgsAux(p: var TParser; into: var seq[Node]) =
  ## Parse the arguments contributed by a `{$IFDEF}`/`{$ELSE}`/`{$ENDIF}`
  ## group that sits INSIDE an argument list, appending them to `into`.
  ##
  ## a corpus unit spells a call across a conditional:
  ##   Int64Rec(Result).Lo := SetFilePointer(THandle(Handle),
  ##     Int64Rec(Result).Lo,
  ##   {$IFDEF FPC}
  ##     PLONG(@Int64Rec(Result).Hi), Origin);
  ##   {$ELSE}
  ##     @Int64Rec(Result).Hi, Origin);
  ##   {$ENDIF}
  ## The dead arm embeds a whole call tail - including the closing `)` -
  ## so it must be SKIPPED at the token level, never parsed. The taken
  ## arm's arguments are spliced in place of the directive, which is what
  ## this does; the caller's loop then continues with whatever follows
  ## `{$ENDIF}` (there, the `;`). The `{$IF}`/`{$ELSEIF}` text-form
  ## evaluator used for declarations is deliberately not reused here: an
  ## argument cannot be an expression node without losing the delimiters
  ## that live inside the arms.
  var kw = ""
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    kw = p.tok.ident.toLowerAscii
  let endMarker = succ(p.tok.xkind)
  var taken = false
  if kw == "ifdef" or kw == "ifndef":
    getTokP(p)
    let d = p.syms.defines.getOrDefault(parseCondName(p), false)
    taken = if kw == "ifdef": d else: not d
    eatDirEnd(p, endMarker)
  elif kw == "if":
    # `{$if <expr>}` is not evaluable by a frontend. Here the arms hold
    # argument delimiters (a dead arm embeds the closing `)`), so
    # the group cannot be lowered to a Nim `when` expression either:
    # refusing is the honest outcome and the Pascal source has to change.
    parError(p, "{$if <expr>} cannot be evaluated by a frontend and " &
      "cannot be lowered to a `when` in an argument list")
  else:
    return
  skipCom(p)
  var done = false
  var inTaken = taken
  while not done:
    if p.tok.xkind == pxEof:
      break
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      let d = p.tok.ident.toLowerAscii
      if d == "else":
        eatDirEnd(p, succ(p.tok.xkind))
        skipCom(p)
        if taken:
          discard skipCondBranch(p, endMarker)
          done = true
        else:
          inTaken = true
        continue
      elif d == "endif":
        eatDirEnd(p, succ(p.tok.xkind))
        done = true
        continue
      elif d in ["ifdef", "ifndef", "if", "elseif"] and inTaken:
        # a nested group inside a taken arm: recurse for its arguments
        condArgsAux(p, into)
        continue
      elif inTaken:
        eatDirEnd(p, succ(p.tok.xkind))
        continue
      else:
        discard skipCondBranch(p, succ(p.tok.xkind))
        continue
    if not inTaken:
      getTokP(p)
      continue
    var a = parseExpr(p)
    if p.tok.xkind == pxColon:
      # the same `e:w[:p]` width lowering exprListAux performs
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
    into.add(a)
    if p.tok.xkind in {pxComma, pxSemiColon}:
      getTokP(p)
      skipCom(p)
    else:
      done = true

proc exprListAux(p: var TParser, endTok, sepTok: TTokKind, result: Node) =
  getTokP(p)
  skipCom(p)
  while true:
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # a conditional group inside the list
      var spliced: seq[Node] = @[]
      condArgsAux(p, spliced)
      for sp in spliced:
        result.add(sp)
      if p.tok.xkind == endTok:
        getTokP(p)
        break
      continue
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

proc isOrdinalSpelling(ty: string): bool =
  ## integer/ordinal spellings the RTL map produces (rtlSpelling) - the
  ## source side of the Win32-era `Pointer(<ordinal>)` reinterpretation
  case ty
  of "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32",
     "uint64", "int", "uint", "bool", "char":
    result = true
  else:
    result = false

proc isPointerishSpelling(ty: string): bool =
  ## spellings that hold a raw address or an object reference - the
  ## source side of the Win32-era `Integer(<pointer>)` reinterpretation
  result = ty == "pointer" or ty == "rootref" or ty.startsWith("class:") or
           ty.startsWith("ptr") or ty.startsWith("ref ")

proc operandDomain(p: var TParser; a: Node): string =
  ## the operand's domain for Delphi's typecast rules: "num" (ordinal),
  ## "ptr" (raw pointer), "ref" (object/method reference), "" unknown.
  ## Delphi casts WITHIN one domain are conversions (`int32(1)`,
  ## `float(1)`) and stay plain calls; casts ACROSS domains are
  ## reinterpretations (`Integer(ptr)`, `Pointer(someInt)`,
  ## `TObject(ptr)`) and need `cast[...]`, which nimsem requires anyway -
  ## it accepts neither an ordinal->pointer nor a ref->pointer
  ## conversion, on any target.
  var ty = ""
  case a.kind
  of nkIdent:
    ty = p.stringBaseType(a)
  of nkCast:
    if a.len == 2 and a[0].kind == nkIdent:
      ty = p.syms.canonical(a[0].strVal).toLowerAscii
  of nkIntLit:
    return "num"
  of nkNilLit:
    return "ptr"
  else:
    return ""
  if ty.len == 0: return ""
  if ty == "pointer": return "ptr"
  if isOrdinalSpelling(ty): return "num"
  if isPointerishSpelling(ty): return "ref"
  result = ""

proc recordOfType(p: TParser; ty: string): string =
  ## the record/class spelling a Pascal type spelling denotes, following
  ## one pointer level (`PNode` -> `RNode`); "" for a non-record type
  result = ""
  if ty.len == 0: return
  if ty.startsWith("record:"):
    return ty[7 .. ^1]
  if ty.startsWith("class:"):
    let cs = p.syms.classSpelling(ty[6 .. ^1])
    return (if cs.len > 0: cs else: ty[6 .. ^1])
  let el = p.pointerAliases.getOrDefault(ty.toLowerAscii, "")
  if el.len > 0:
    let cs = p.syms.classSpelling(el)
    return (if cs.len > 0: cs else: el)

proc baseOwnerSpelling(p: TParser; name: string): string =
  ## the record/class spelling a bare base name denotes, following one
  ## pointer level: `n: PNode` where `PNode = ^RNode` resolves to
  ## `RNode`. Params record a pointer alias as `ptr:PNode` in varTypes,
  ## var-section locals as the raw alias in varRawTypes; a plain
  ## record/class base reads straight from varTypes. "" when unknown.
  let k = name.toLowerAscii
  # a with-qualifier: for a RECORD with the qualifier is the original
  # expression TEXT ('Path[h]') stored as an ident, so resolve it to the
  # scope's class before anything else
  for i in 0 ..< p.withDepth:
    if p.withTemps[i].toLowerAscii == k and p.withClasses[i].len > 0:
      return p.withClasses[i]
  let vt = p.varTypes.getOrDefault(k, "")
  if vt.startsWith("record:"):
    return vt[7 .. ^1]
  if vt.startsWith("class:"):
    return p.syms.classSpelling(vt[6 .. ^1])
  var alias = ""
  if vt.startsWith("ptr:"):
    alias = vt[4 .. ^1]
  else:
    alias = p.varRawTypes.getOrDefault(k, "")
    if alias.len == 0:
      alias = p.paramTypes.getOrDefault(k, "")
  if alias.len == 0 and p.withDepth > 0:
    # a bare FIELD of the active with-scope ('with Path[h] do'
    # 'Nd.Items[x]'): its type is registered against the scope class
    var i = p.withDepth - 1
    while i >= 0 and alias.len == 0:
      alias = p.fieldTypes.getOrDefault(
          p.withClasses[i].toLowerAscii & "." & k, "")
      dec i
  result = p.recordOfType(alias)

proc chainRecordSpelling(p: TParser; e: Node): string =
  ## record/class spelling of an expression used as the OWNER of a field
  ## access, following one pointer level. Handles a bare name, a
  ## qualified chain ('Path[h].Nd') and an indexed element ('Path[h]').
  ## The with-qualifier runs while the primary is parsed, so by the time
  ## the index builder sees 'Path[h].Nd.Items' the owner is already a
  ## NESTED dot expression, not a bare name.
  result = ""
  case e.kind
  of nkIdent:
    result = p.baseOwnerSpelling(e.strVal)
  of nkDotExpr:
    if e.len == 2 and e[1].kind == nkIdent:
      let own = p.chainRecordSpelling(e[0])
      if own.len > 0:
        result = p.recordOfType(p.fieldTypes.getOrDefault(
            own.toLowerAscii & "." & e[1].strVal.toLowerAscii, ""))
  of nkIndexExpr, nkBracket:
    if e.len >= 2:
      if e[0].kind == nkIdent:
        result = p.arrayVarElems.getOrDefault(
            e[0].strVal.toLowerAscii, "")
      if result.len == 0 and e[0].kind == nkDotExpr and e[0].len == 2 and
          e[0][1].kind == nkIdent:
        let own = p.chainRecordSpelling(e[0][0])
        if own.len > 0:
          result = p.classFieldTypes.getOrDefault(
              own.toLowerAscii & "." & e[0][1].strVal.toLowerAscii, "")
  of nkDeref:
    if e.len == 1:
      result = p.chainRecordSpelling(e[0])
  else:
    discard

proc ptrToArraySpelling(p: TParser; e: Node): string =
  ## Pascal type spelling of `e` when it is known, "" otherwise; used to
  ## recognise a pointer-to-array base under `[]`.
  result = ""
  if e.kind == nkIdent:
    let k = e.strVal.toLowerAscii
    result = p.varRawTypes.getOrDefault(k, "")
    if result.len == 0:
      result = p.paramTypes.getOrDefault(k, "")
    if result.len == 0:
      result = p.varTypes.getOrDefault(k, "")
  elif e.kind == nkDotExpr and e.len == 2 and e[1].kind == nkIdent:
    # `r.Items`: field types are keyed by the OWNER's spelling, so the
    # owner (a bare name, a qualified chain or an index) is resolved
    # first. The owner may be a pointer alias, which chainRecordSpelling
    # follows one level.
    let cls = p.chainRecordSpelling(e[0])
    if cls.len > 0:
      result = p.fieldTypes.getOrDefault(
          cls.toLowerAscii & "." & e[1].strVal.toLowerAscii, "")

proc bracketExprList(p: var TParser, first: Node): Node =
  ## `a[i]`, `a[i, j]`, ... A MULTI-index access is built as nested
  ## one-index nodes (`a[i][j]`): nimony has no multi-index `[]`, and
  ## `a[i, j]` there resolves the head once and then fails to match the
  ## second argument. The generic-instantiation `[...]` spelling comes
  ## from the `<...>` path, so it is unaffected.
  ##
  ## A POINTER-to-array base needs an explicit deref: Pascal (like
  ## Nim 1) auto-derefs `n.Items[x]`, but nimony rejects indexing a
  ## `ptr array` ("ptr array[..] does not match constraint T"), so emit
  ## `n.Items[][x]`. `pointerAliases` knows the `P = ^T` aliases and
  ## the base's recorded spelling identifies one.
  var head = first
  if head.kind != nkDeref:
    var sp = p.ptrToArraySpelling(head).toLowerAscii
    if sp.startsWith("ptr:"): sp = sp[4 .. ^1]
    if sp.len > 0 and p.pointerAliases.hasKey(sp):
      let d = newNode(nkDeref, head.info)
      d.add(head)
      head = d
  var idxs: seq[Node] = @[]
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
    idxs.add(a)
  # Delphi strings are 1-based; nimony's `string` is 0-based
  if idxs.len == 1 and stringBaseType(p, head) == "string":
    idxs[0] = decIndex(p, idxs[0])
  result = head
  for x in idxs:
    let ie = newNode(nkIndexExpr, head.info)
    ie.add(result)
    ie.add(x)
    result = ie

proc identOrLiteral(p: var TParser): Node =
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    # a conditional in expression position: `x = {$IFDEF}#10{$ELSE}#13{$ENDIF}`
    if not declDirective(p): break
    skipCom(p)
  case p.tok.xkind
  of pxOperator:
    # a keyword-escaped parameter name used in an expression
    # (`case Operator of` on `const Operator: TVarOp`)
    result = newIdentNode(p.tok.ident, p.tok.info)
    getTokP(p)
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
      if p.tok.xkind == pxComma or p.tok.xkind == pxSemiColon:
        # a typed record constant separates its fields with `;`
        # (`(Key: ''; Link: nil)`), while array/set constructors use
        # `,`. Accept both: neither form is legal with the other
        # separator, so this cannot swallow a real delimiter.
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
  # consume the dead alternative / closing marker of the same
  # conditional: `x = {$IFDEF}#10{$ELSE}#13{$ENDIF}` - the live value
  # was the literal; the {$else} group and {$endif} end here
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    if declDirective(p): skipCom(p)
    else: break

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
    let atInfo = p.tok.info
    getTokP(p)
    if p.tok.xkind == pxSymbol and
        p.syms.routineArgs.hasKey(p.tok.ident.toLowerAscii):
      # `@F` where F is a routine: Pascal takes the routine's ADDRESS, but
      # in Nim the routine name already IS the proc value and
      # `addr(F)` is rejected ("invalid expression for `addr` operation").
      # Lower to the bare name so method-pointer / callback arguments
      # type-check (synacode's `@MD5Transform`).
      result = primary(p)
      return
    result = newNode(nkAddr, atInfo)
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
    # Comments must go unconditionally; the costlier directive descent is
    # guarded, because this runs for EVERY postfix operand.
    skipCom(p)
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: skipDirectives(p)
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
      # `Variant(x)` is a *cast*: measured (test/variant/vcast.pas) to agree
      # with the implicit conversion for every source type in both oracles
      if result.len == 2 and result[0].kind == nkIdent and
          result[0].strVal.toLowerAscii in ["variant", "olevariant"]:
        result = variantCoerce(p, result[1])
      # a 1-char Pascal literal passed to a callee is a string in
      # almost every signature; char-arg procs keep the char. The
      # callee's declared param types win over the default.
      if result.len > 1 and not (a.kind == nkIdent and
          a.strVal.toLowerAscii in ["stringofchar", "ord", "chr"]):
        var argTypes: seq[string] = @[]
        if a.kind == nkIdent and a.strVal.toLowerAscii == "setlength" and
            result.len == 3 and
            not (result[2].kind in {nkIntLit, nkInt64Lit}):
          # nimony's setLen takes `int` (64-bit); a Cardinal/Longint
          # length argument needs the widening cast
          let wc = newNode(nkCast, result[2].info)
          wc.add(newIdentNode("int64", result[2].info))
          wc.add(result[2])
          result[2] = wc
        if a.kind == nkIdent and
            a.strVal.toLowerAscii in ["vararrayof", "vararraycreate"] and
            result.len > 1 and result[1].kind == nkBracket:
          # VarArrayOf([...]) - an open `array of Variant`, so each element
          # must be built; VarArrayCreate's bounds are `array of Integer`
          # (int32 in the shim) and a literal array is typed int64 by nimony
          let br = result[1]
          let isOf = a.strVal.toLowerAscii == "vararrayof"
          for bi in 0 ..< br.len:
            br[bi] = if isOf: variantArrayElem(p, br[bi])
                     else: variantInt32Arg(p, br[bi])
        block:
          # Variant-typed parameters (Delphi's implicit conversion)
          var vps: seq[bool] = @[]
          if a.kind == nkIdent:
            vps = p.variantParams.getOrDefault(a.strVal.toLowerAscii)
            if vps.len == 0 and p.selfClass.len > 0:
              vps = p.variantParams.getOrDefault(
                  p.selfClass.toLowerAscii & "." & a.strVal.toLowerAscii)
            if vps.len == 0 and p.classOfProc.len > 0:
              vps = p.variantParams.getOrDefault(
                  p.classOfProc.toLowerAscii & "." & a.strVal.toLowerAscii)
          elif a.kind == nkDotExpr and a[1].kind == nkIdent:
            vps = p.variantParams.getOrDefault(a[1].strVal.toLowerAscii)
            if vps.len == 0 and a[0].kind == nkIdent:
              vps = p.variantParams.getOrDefault(
                a[0].strVal.toLowerAscii & "." & a[1].strVal.toLowerAscii)
          for ai in 1 ..< result.len:
            if ai - 1 < vps.len and vps[ai - 1]:
              result[ai] = variantCoerce(p, result[ai])
        if a.kind == nkIdent:
          argTypes = p.routineParams.getOrDefault(a.strVal.toLowerAscii)
          if argTypes.len == 0 and p.selfClass.len > 0:
            # a bare member call inside a method: the declared param
            # types are keyed by the declaring class
            argTypes = p.routineParams.getOrDefault(
                p.selfClass.toLowerAscii & "." & a.strVal.toLowerAscii)
          if argTypes.len == 0 and p.classOfProc.len > 0:
            argTypes = p.routineParams.getOrDefault(
                p.classOfProc.toLowerAscii & "." & a.strVal.toLowerAscii)
        elif a.kind == nkDotExpr and a[1].kind == nkIdent:
          argTypes = p.routineParams.getOrDefault(a[1].strVal.toLowerAscii)
          if argTypes.len == 0 and a[0].kind == nkIdent:
            argTypes = p.routineParams.getOrDefault(
              a[0].strVal.toLowerAscii & "." & a[1].strVal.toLowerAscii)
        for ai in 1 ..< result.len:
          if ai - 1 < argTypes.len:
            let pta = argTypes[ai - 1].toLowerAscii
            if pta == "set" or pta.startsWith("set:") or
                p.setTypes.hasKey(pta):
              # a Pascal set literal passed to a set-typed parameter must
              # be `{...}`: nimony's `[...]` is an array and does not
              # coerce. The literal can be an operand of a set
              # union/difference (`['='] + NonAsciiChar`).
              if result[ai].kind == nkBracket:
                result[ai].kind = nkCurly
              elif result[ai].kind == nkInfix and result[ai].len == 3:
                for oi in 1 ..< result[ai].len:
                  if result[ai][oi].kind == nkBracket:
                    result[ai][oi].kind = nkCurly
          if result[ai].kind == nkCharLit:
            if ai - 1 < argTypes.len and
                argTypes[ai - 1].toLowerAscii in ["char", "ansichar", "widechar"]:
              continue
            let sd = newNode(nkStrLit, result[ai].info)
            sd.strVal = result[ai].strVal
            result[ai] = sd
          elif result[ai].kind == nkCall and result[ai].len == 2 and
              result[ai][0].kind == nkIdent and
              result[ai][0].strVal.toLowerAscii == "ord" and
              ai - 1 < argTypes.len:
            # nimony's ord() types wide (int64 for a bool); a narrow
            # declared param needs the width cast at the call boundary
            let at = argTypes[ai - 1].toLowerAscii
            var narrow = ""
            case at
            of "int8", "shortint": narrow = "int8"
            of "uint8", "byte": narrow = "uint8"
            of "int16", "smallint": narrow = "int16"
            of "uint16", "word": narrow = "uint16"
            of "int32", "integer", "longint": narrow = "int32"
            of "uint32", "cardinal", "longword": narrow = "uint32"
            else: discard
            if narrow.len > 0:
              let wc = newNode(nkCall, result[ai].info)
              wc.add(newIdentNode(narrow, result[ai].info))
              wc.add(result[ai])
              result[ai] = wc
          elif result[ai].kind == nkCall and result[ai].len == 2 and
              result[ai][0].kind == nkIdent and
              result[ai][0].strVal.toLowerAscii == "sizeof" and
              ai - 1 < argTypes.len:
            # Pascal SizeOf is an Integer (int32) that converts implicitly
            # at the call boundary; nimony's sizeof types int64, so a
            # fixed-width declared param needs the conversion here
            let st = argTypes[ai - 1].toLowerAscii
            var width = ""
            case st
            of "int8", "shortint": width = "int8"
            of "uint8", "byte": width = "uint8"
            of "int16", "smallint": width = "int16"
            of "uint16", "word": width = "uint16"
            of "int32", "integer", "longint": width = "int32"
            of "uint32", "cardinal", "longword", "dword": width = "uint32"
            of "int64": width = "int64"
            of "uint64": width = "uint64"
            else: discard
            if width.len > 0:
              let wc = newNode(nkCall, result[ai].info)
              wc.add(newIdentNode(width, result[ai].info))
              wc.add(result[ai])
              result[ai] = wc
      if result.len > 1 and a.kind == nkIdent:
        # the dialect shim's own Variant-taking helpers (VarType, VarToStr,
        # VarAsType, VarIs*, VarArray*): their signatures live in the Nim
        # runtime, so the parser carries the positions
        for pos in variantShimArgPositions(a.strVal.toLowerAscii):
          if pos < result.len:
            result[pos] = variantCoerce(p, result[pos])
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
      if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        # A conditional may select the member NAME itself, which is how
        # a struct field that Delphi and POSIX name differently is
        # spelled (a corpus unit):
        #   Multicast6.ipv6mr_multiaddr.{$IFDEF POSIX}s6_addr{$ELSE}u6_addr8{$ENDIF}[n]
        # The branch is chosen at parse time exactly as for
        # declarations, and only the taken branch is parsed - the dead
        # arm's identifier must not be emitted.
        var kw = p.tok.ident.toLowerAscii
        let em0 = succ(p.tok.xkind)
        var taken = false
        if kw == "ifdef" or kw == "ifndef":
          getTokP(p)
          let d = p.syms.defines.getOrDefault(parseCondName(p), false)
          taken = if kw == "ifdef": d else: not d
          eatDirEnd(p, em0)
        elif kw == "if":
          # a `{$if <expr>}` cannot select an identifier: the choice is
          # semantic (declared()/sizeof()) and a frontend cannot make it.
          parError(p, "{$if <expr>} cannot select an identifier; a " &
            "frontend cannot evaluate the condition")
        skipCom(p)
        var chosen = false
        while not chosen:
          if p.tok.xkind == pxEof:
            parError(p, "identifier expected after '.'")
            break
          if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
            let d = p.tok.ident.toLowerAscii
            if d == "else":
              eatDirEnd(p, succ(p.tok.xkind))
              skipCom(p)
              taken = not taken
              continue
            elif d == "endif":
              eatDirEnd(p, succ(p.tok.xkind))
              break
            elif taken and d in ["ifdef", "ifndef"]:
              # a nested group guarding the name
              let emN = succ(p.tok.xkind)
              getTokP(p)
              let dn = p.syms.defines.getOrDefault(parseCondName(p), false)
              let tn = if d == "ifdef": dn else: not dn
              eatDirEnd(p, emN)
              if not tn:
                discard skipCondBranch(p, succ(p.tok.xkind))
              continue
            else:
              eatDirEnd(p, succ(p.tok.xkind))
              continue
          if taken:
            if p.tok.xkind != pxSymbol:
              parError(p, "identifier expected after '.'")
              break
            result.add(newIdentNode(p.tok.ident, p.tok.info))
            getTokP(p)
            chosen = true
          else:
            getTokP(p)
      elif p.tok.xkind == pxSymbol:
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
  # A comment (or a conditional directive) may sit between an operand
  # and its operator, and the operator may even be on the far side of a
  # line break:
  #   if (Length(right.FValue.FWord.FInput) > 0)
  #     //or IsFunction(right)
  #     or IsFunctionWithParam(right, nextOp)
  #   then
  # a corpus unit wraps exactly like that. Without skipCom the `or`
  # is never seen as an operator, the parse stops after the comparison
  # and the caller reports "expected then but got: or".
  skipCom(p)
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: skipDirectives(p)
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
      if isVariantExpr(p, v) or isVariantExpr(p, v2):
        # Variant arithmetic/comparison: Pascal semantics live in the shim's
        # named procs (they raise on a type mismatch, exactly like Delphi)
        let vop = variantOpName(opNode.strVal)
        if vop.len > 0:
          let vc = newNode(nkCall, node.info)
          vc.add(newIdentNode(vop, node.info))
          vc.add(variantCoerce(p, v))
          vc.add(variantCoerce(p, v2))
          v = vc
          op = nextop
          opPred = getPrecedence(nextop)
          continue
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
      # a bit-op's ord() operand: nimony's ord types int64, a narrow
      # declared width needs the cast at the operator boundary
      if op in {pxAnd, pxOr, pxXor, pxShl, pxShr}:
        let lt = rhsExprType(p, v)
        if lt in ["int8", "uint8", "int16", "uint16", "int32", "uint32"]:
          if v2.kind == nkCall and v2.len == 2 and
              v2[0].kind == nkIdent and
              v2[0].strVal.toLowerAscii == "ord":
            let wc = newNode(nkCall, v2.info)
            wc.add(newIdentNode(lt, v2.info))
            wc.add(v2)
            v2 = wc
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
    var b = parseExpr(p)
    # an empty `[]` assigned to a set-typed target is the empty SET
    # (nimony's `[]` is an auto array literal that will not coerce);
    # the renderer keeps plain empty `[]` for openArray call args
    if b.kind == nkBracket and b.len == 0:
      var lhs = a
      if lhs.kind == nkDotExpr and lhs.len == 2:
        lhs = lhs[1]
      if lhs.kind == nkIdent:
        let vt = p.varTypes.getOrDefault(lhs.strVal.toLowerAscii)
        if vt == "set" or vt.startsWith("set:"):
          result = newNode(nkAsgn, info)
          result.add(a)
          let curly = newNode(nkCurly, b.info)
          result.add(curly)
          return
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
  while p.tok.xkind != pxEnd and p.tok.xkind != pxParRi and
      p.tok.xkind != pxEof:
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
        if p.tok.xkind == pxCase:
          # a nested variant case inside the branch body
          let nested = parseRecordCase(p)
          for s in nested.sons: body.add(s)
          p.opt(pxSemiColon)
          skipCom(p)
          continue
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
      var isOut = false
      var isConst = false
      if p.tok.xkind == pxVar:
        isVar = true
        getTokP(p)
      elif p.tok.xkind == pxOut:
        # Delphi `out` params: nimony's `out` also sees the caller's
        # variable but needs no init proof at the call site
        isVar = true
        isOut = true
        getTokP(p)
      elif p.tok.xkind == pxConst:
        # `const` param: a plain param for codegen, but read-only for the
        # writability check that chooses a `with` temp (address vs copy)
        isConst = true
        getTokP(p)
      skipCom(p)
      # names
      var names: seq[string] = @[]
      while true:
        if p.tok.xkind != pxSymbol and p.tok.xkind != pxOperator:
          parError(p, "identifier expected in params, got " & $p.tok)
        # keyword-escaped param name: `const Operator: TVarOp` - the
        # nimony spelling escapes the keyword
        var pname = p.tok.ident
        if p.tok.xkind == pxOperator:
          pname = "pas" & pname[0].toUpperAscii & pname[1 .. ^1]
        names.add(pname)
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
          elif p.pointerAliases.hasKey(pty):
            p.varTypes[n.toLowerAscii] = "ptr:" & lastType.strVal
          elif p.syms.isClass(pty):
            # class-typed params/locals: receiver-class logic (the
            # statement discard rule, per-class return keys) needs it
            p.varTypes[n.toLowerAscii] = "class:" & lastType.strVal
          else:
            p.varRawTypes[n.toLowerAscii] = pty
            let rtl = rtlSpelling(pty)
            if rtl.len > 0:
              p.varTypes[n.toLowerAscii] = rtl
            let rr = p.procTyReturns.getOrDefault(pty)
            if rr.len > 0: p.procVarReturns[n.toLowerAscii] = rr
        let d = newNode(nkIdentDefs, p.tok.info)
        d.isImmutable = isConst
        d.add(newIdentNode(n, p.tok.info))
        if isVar:
          let vt = newNode(nkVarTy, p.tok.info)
          vt.isOutParam = isOut
          if lastType.kind != nkEmpty: vt.add(lastType)
          else:
            # Delphi `var X;` (untyped buffer param): nimony rejects
            # untyped params outside macros, so a concrete stand-in
            # keeps calls chaining (v1 divergence, see docs)
            vt.add(newIdentNode("pointer", p.tok.info))
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
  # return type
  if p.tok.xkind == pxColon:
    getTokP(p)
    skipCom(p)
    let ret = parseTypeDesc(p, emptyNode(p.tok.info))
    # the return type lives in the params' slot 0 (the procTy's own
    # son 0 is the params); overwriting it broke 2-son invariants
    result[0][0] = ret
  # `of object` closure marker - Delphi allows it after the return
  # type as well: `function: WideString of object` (method pointer)
  if p.tok.xkind == pxOf:
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxObject:
      isClosure = true
      getTokP(p)
      skipCom(p)
    else:
      parError(p, "object expected after `of` in procedure type")
  if isClosure:
    let pragmas = newNode(nkPragma, p.tok.info)
    pragmas.add(newIdentNode("closure", p.tok.info))
    result.add(pragmas)
  else:
    result.add(emptyNode(p.tok.info))

# ---------------------------------------------------------------------------
# declarations

proc dirPicksName(p: var TParser): bool =
  ## True when the current `{$IFDEF}`/`{$IF}` group selects a DECLARATION
  ## NAME rather than wrapping statements:
  ##   {$IFDEF POSIX}s6_addr{$ELSE}u6_addr8{$ENDIF}: Integer;
  ## A record/class member loop cannot hand such a group to
  ## declDirective: the taken identifier would parse as a STATEMENT and
  ## the `:` is left stranded, which is exactly the corpus failure
  ## "field or `case` expected in record body, got :".
  ##
  ## The test scans the raw source from the current position to the end of
  ## the logical line: a `:` or `=` BEFORE any `;`, `)` or statement
  ## keyword means a name followed by its type or `=`. Braced groups are
  ## skipped whole, so the `else` of `{$ELSE}` is never mistaken for the
  ## statement keyword; a group left open at the line end means the rest
  ## of the declaration is on later lines, and the field parser handles
  ## those (condPickIdent walks the tokens itself). Nothing is consumed.
  result = false
  if p.tok.xkind notin {pxCurlyDirLe, pxStarDirLe}:
    return
  # Only the forms a frontend can answer select a name. A `{$if <expr>}`
  # is not one of them; it falls through to the ordinary directive path,
  # which reports that it cannot be evaluated.
  if p.tok.ident.toLowerAscii notin ["ifdef", "ifndef"]:
    return
  let buf = p.lex.buf
  var i = p.lex.bufpos
  var guard = 0
  while i < buf.len and guard < 600:
    inc guard
    let c = buf[i]
    if c == '\c' or c == '\l':
      return                       # line ended with no `:`/`=`
    if c == '{':
      # skip the whole `{$...}` group, respecting nesting
      var depth = 0
      var ok = false
      while i < buf.len:
        if buf[i] == '{':
          inc depth
        elif buf[i] == '}':
          dec depth
          if depth == 0:
            inc i
            ok = true
            break
        elif buf[i] == '\c' or buf[i] == '\l':
          return
        inc i
      if not ok:
        return
      continue
    if c == '}':
      inc i
      continue
    if c == ';' or c == ')':
      return                       # a statement cell, not a name
    if c == ':' or c == '=':
      result = true
      return
    if c in {'a'..'z', 'A'..'Z', '_'}:
      let a = i
      while i < buf.len and (buf[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}):
        inc i
      let w = buf[a ..< i].toLowerAscii
      if w in ["begin", "end", "else", "case", "try", "for",
               "while", "repeat", "record", "class", "type", "var",
               "const", "procedure", "function", "property", "unit",
               "implementation", "interface"]:
        return
      continue
    inc i

proc wordAt2(s: string; i: var int): string =
  result = ""
  while i < s.len and s[i] in {' ', '\t', '\r', '\n'}:
    inc i
  let a = i
  while i < s.len and (s[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}):
    inc i
  if i > a:
    result = s[a ..< i]

proc delimAfter(decl: string; startAt: int): int =
  ## index of the `:` or `=` that terminates a declaration NAME, starting
  ## at `startAt`. Whole `{$...}` groups are stepped over, so a conditional
  ## between the name and the delimiter cannot supply a false hit.
  result = decl.len
  var i = startAt
  while i < decl.len:
    if decl[i] == '{':
      while i < decl.len and decl[i] != '}':
        inc i
      if i < decl.len:
        inc i
      continue
    if decl[i] in {':', '='}:
      return i
    inc i

proc declTextToEol(L: TLexer; lineStart: var int): string =
  ## the raw source of the declaration that starts at the current lexer
  ## position, taken to the end of its line. The position is walked back
  ## over the directive body already consumed, so a group that began
  ## earlier on the line is included in full.
  result = ""
  let buf = L.buf
  var i = L.bufpos
  while i > 0 and buf[i - 1] notin {'\c', '\l'}:
    dec i
  # `lineStart` must point at the FIRST NON-BLANK character of the line,
  # because the offsets `pickNameFromText` reports are indices into the
  # returned text. Leaving indentation in would shift every offset and
  # make the caller resume in the middle of the conditional.
  var blank = i
  while blank < buf.len and buf[blank] in {' ', '\t'}:
    inc blank
  i = blank
  var e = i
  while e < buf.len and buf[e] notin {'\c', '\l'}:
    inc e
  lineStart = i
  if e > i:
    result = buf[i ..< e]

proc pickNameFromText(decl: string; p: var TParser;
                      skipLen: var int): string =
  ## the identifier the TAKEN arms of a name conditional contribute, read
  ## straight from the declaration's source text:
  ##   {$IFDEF POSIX}s6_addr{$ELSE}u6_addr8{$ENDIF}: Integer;
  ## `skipLen` receives the offset just past the group that was consumed,
  ## so the caller can continue at the type name.
  result = ""
  skipLen = 0
  var arm = true
  var i = 0
  while i < decl.len:
    if decl[i] in {' ', '\t', '\r', '\n'}:
      inc i
      continue
    if decl[i] != '{' and arm:
      # a plain identifier before any group: that IS the name
      var k = i
      let w = wordAt2(decl, k)
      if w.len > 0:
        result = w
        skipLen = delimAfter(decl, k)
        return result
      inc i
      continue
    if decl[i] == '{':
      let gs = i
      inc i
      let d = wordAt2(decl, i).toLowerAscii
      # skip to the `}` that closes this group, then past it
      while i < decl.len and decl[i] != '}':
        inc i
      var ge = i
      if ge < decl.len:
        ge += 1
      if d in ["ifdef", "ifndef"]:
        var k = gs + 1
        let nm2 = wordAt2(decl, k).toLowerAscii
        let def = p.syms.defines.getOrDefault(nm2, false)
        arm = if d == "ifdef": def else: not def
      elif d == "else":
        # the braces sit around the keyword: `{$ELSE}`
        discard
      elif d == "endif":
        return result
      if d in ["else", "elseif"]:
        # opposite of the branch that ran before this `{$ELSE}`
        arm = not arm
        if arm:
          # the taken arm begins right after the `}`
          var k = ge
          let w = wordAt2(decl, k)
          if w.len > 0:
            result = w
            skipLen = delimAfter(decl, k)
            return result
          i = ge
          continue
        i = ge
        continue
      i = ge
      continue
    if arm and decl[i] notin {' ', '\t', '\r', '\n'}:
      var k = i
      let w = wordAt2(decl, k)
      if w.len > 0:
        result = w
        # `k` has advanced past the identifier, so this offset is the first
        # character AFTER the name. The lexer must resume exactly there:
        # landing INSIDE the following `{$...}` would make `getTok` read it
        # as a directive opener instead of the `:` that ends the field.
        skipLen = k
        return result
      inc i
      continue
    inc i
  result = result

proc condPickIdent(p: var TParser): string =
  ## the declaration NAME a `{$IFDEF}`/`{$IFNDEF}` group selects, e.g.
  ##   {$IFDEF POSIX}s6_addr{$ELSE}u6_addr8{$ENDIF}: Integer;
  ##
  ## The name is read from the declaration's SOURCE TEXT rather than by
  ## walking tokens. The lexer's `{$...}` handling stops the opener after
  ## the directive name and re-scans the tail, so a token walk sees
  ## phantom openers (`{$` carrying the NEXT directive's name) and cannot
  ## reliably find the `}` that ends the group. Text has none of those
  ## artefacts: the arm that is taken is selected by evaluating the
  ## directives in order, and only that arm's identifier is produced.
  ## On success the lexer is moved past the whole group, so the caller
  ## continues at the `:` (or `=`).
  result = ""
  if p.tok.xkind notin {pxCurlyDirLe, pxStarDirLe}:
    return
  let kwd = p.tok.ident.toLowerAscii
  if kwd notin ["ifdef", "ifndef"]:
    if kwd == "if":
      # Not answerable by a frontend. Nim could express this as a `when`
      # around the field, but that is a different emission (two whole
      # field declarations, not one name), so refuse instead of guessing.
      parError(p, "{$if <expr>} cannot select a declaration name; a " &
        "frontend cannot evaluate the condition")
    return
  var lineStart = 0
  let text = declTextToEol(p.lex[], lineStart)
  var skipLen = 0
  let nm = pickNameFromText(text, p, skipLen)
  if nm.len == 0:
    return
  # `skipLen` is an offset into `text`, which begins at the first
  # non-blank character of the declaration's line, so translate it with
  # `lineStart` and rewind the lexer there via `rewindTo` (which also
  # clears the lexer's pending end marker). Resuming exactly ON the `:`
  # matters: a position inside the following `{$...}` would make `getTok`
  # read that group as a directive opener instead of the `:`.
  let target = lineStart + skipLen
  p.lex[].rewindTo(target)
  p.lex[].getTok(p.tok)
  skipCom(p)
  result = nm

proc parseIdentColonEquals*(p: var TParser; withVis: bool): Node =
  ## `a, b: Type = init;`  (var/field declaration group)
  result = newNodeP(nkIdentDefs, p)
  let exportNames = p.section == seInterface and p.visibility != visPrivate
  while true:
    var fieldName = ""
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      fieldName = condPickIdent(p)
    if fieldName.len == 0:
      if p.tok.xkind != pxSymbol:
        parError(p, "identifier expected, got " & $p.tok)
      fieldName = p.tok.ident
      getTokP(p)
    p.syms.declareName(fieldName)
    result.add(exSymbol(newIdentNode(fieldName, p.tok.info), exportNames))
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
    if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "absolute":
      # `Name: T absolute Target;` binds a SECOND NAME to an existing
      # variable (the corpus uses only this form - no absolute
      # addresses). Nim has no `absolute`, but a template whose body is
      # the target reads and writes through to it, at module and local
      # scope alike. The alias REPLACES the declaration, and `template`
      # may not sit inside a `var` block, so the renderers hoist it out.
      getTokP(p)
      skipCom(p)
      let target = parseExpr(p)
      for i in 0 ..< defs.len - 2:
        if defs[i].kind != nkIdent:
          continue
        let t = newNode(nkTemplateDef, defs[i].info)
        t.add(exSymbol(newIdentNode(defs[i].strVal, defs[i].info),
                       defs[i].exported))
        t.add(emptyNode(defs[i].info))
        let params = newNode(nkFormalParams, defs[i].info)
        # `defs` is discarded here, so its type node has no other parent
        params.add(defs[defs.len - 2])
        t.add(params)
        t.add(emptyNode(defs[i].info))
        t.add(emptyNode(defs[i].info))
        let body = newNode(nkStmtList, defs[i].info)
        body.add(target)
        t.add(body)
        result.add(t)
      p.opt(pxSemiColon)
      skipCom(p)
      continue
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
      if tyNode.len > 0:
        let et = setElemTypeText(p, tyNode[0])
        if et.len > 0:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.setElemTexts[defs[i].strVal.toLowerAscii] = et
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
      # class/record-element array vars: the element spelling for
      # `with S[i] do` lowering
      var elty2 = ""
      if tyNode.len > 0 and tyNode[tyNode.len - 1].kind == nkIdent:
        elty2 = tyNode[tyNode.len - 1].strVal
      if elty2.len > 0:
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.arrayVarElemTypes[defs[i].strVal.toLowerAscii] = elty2
        let elKey2 = elty2.toLowerAscii
        if p.syms.isClass(elKey2) or p.recordTypes.hasKey(elKey2):
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.arrayVarElems[defs[i].strVal.toLowerAscii] = elty2
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
      if p.procTyTypes.hasKey(tyKey):
        let rr = p.procTyReturns.getOrDefault(tyKey)
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.procVarTypes[defs[i].strVal.toLowerAscii] = true
            p.varRawTypes[defs[i].strVal.toLowerAscii] = tyNode.strVal
            if rr.len > 0:
              p.procVarReturns[defs[i].strVal.toLowerAscii] = rr
      # a NAMED array alias (`var r: TArr` where `TArr = array[..] of
      # TRec`): the element spelling has to reach `arrayVarElems` too,
      # or `with r[i] do` cannot resolve the record and emits bare
      # field names ("undeclared identifier: Node/Value/Key")
      let aliasEl = p.arrayAliases.getOrDefault(tyKey, "")
      if aliasEl.len > 0:
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.arrayVarElems[defs[i].strVal.toLowerAscii] = aliasEl
            p.arrayVarElemTypes[defs[i].strVal.toLowerAscii] = aliasEl
      # a record is registered in the class registry too (so its
      # members resolve), but it is a VALUE type: test it FIRST, or a
      # record variable is filed as "class:" and `with` lowering then
      # binds a hidden temp whose writes never reach the original.
      if p.recordTypes.hasKey(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            # record vars carry their spelling for `with` lowering
            p.varTypes[defs[i].strVal.toLowerAscii] = "record:" & tyNode.strVal
      elif p.pointerAliases.hasKey(tyKey):
        # a typed-pointer local: the assignment site needs the pointee
        # alias to insert the Pointer->ptr conversion (`it2 = MemAlloc(..)`).
        # varRawTypes keeps the bare alias the deref pass (bracketExprList)
        # keys on, so `it2[x]` still lowers to the explicit `it2[][x]`.
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.varTypes[defs[i].strVal.toLowerAscii] = "ptr:" & tyNode.strVal
            p.varRawTypes[defs[i].strVal.toLowerAscii] = tyKey
      elif p.syms.isClass(tyKey):
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.varTypes[defs[i].strVal.toLowerAscii] = "class:" & tyKey
      else:
        let mapped = rtlSpelling(tyKey)
        if mapped.len > 0:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.varTypes[defs[i].strVal.toLowerAscii] = mapped
        # the *Pascal* spelling as well: Currency and TDateTime are plain
        # float64 here but carry their own Variant tag (measured)
        for i in 0 ..< defs.len - 2:
          if defs[i].kind == nkIdent:
            p.varRawTypes[defs[i].strVal.toLowerAscii] = tyKey

proc foldLiteralConcat(n: Node): Node =
  ## Pascal's `AnsiChar + AnsiChar` builds a string (`#$40 +#$40 ...`,
  ## ReTablebase64 in synacode). nimony has no char+char concatenation, so
  ## fold an all-literal `+` chain into one string literal.
  result = n
  if n.kind == nkInfix and n.len == 3 and n[0].kind == nkIdent and
      n[0].strVal == "+":
    let a = foldLiteralConcat(n[1])
    let b = foldLiteralConcat(n[2])
    if a.kind in {nkCharLit, nkStrLit} and b.kind in {nkCharLit, nkStrLit}:
      let s = newNode(nkStrLit, n.info)
      s.strVal = a.strVal & b.strVal
      return s

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
    # interface consts/resourcestrings travel as module-level consts in
    # the generated unit; without the export marker a consuming unit
    # (import <Unit>) sees "undeclared identifier" (the parseIdentColon-
    # Equals rule applies to consts too)
    def.add(exSymbol(newIdentNode(name, info),
        p.section == seInterface and p.visibility != visPrivate))
    if p.tok.xkind == pxColon:
      getTokP(p)
      skipCom(p)
      def.add(parseTypeDesc(p, emptyNode(p.tok.info)))
      skipCom(p)
    else:
      def.add(emptyNode(info))
    p.eat(pxEquals)
    skipCom(p)
    def.add(foldLiteralConcat(parseExpr(p)))
    # an untyped integer const that fits in int32 is an Integer in Pascal
    # (context-adaptive). nimony types a bare literal as int64 (its int),
    # which does not coerce to an int32 field/local on assignment.
    if def.len > 2 and def[1].kind == nkEmpty and
        def[2].kind in {nkIntLit, nkInt64Lit} and
        def[2].intVal >= -2147483648 and def[2].intVal <= 2147483647:
      let wc = newNode(nkCall, def[2].info)
      wc.add(newIdentNode("int32", def[2].info))
      wc.add(def[2])
      def[2] = wc
    # A Pascal typed RECORD constant `(Key: ''; Link: nil)` parses as an
    # `nkPar` of `kv` pairs, which is not a record constructor in any
    # target: the NIF shape is `(oconstr TY (kv K V) ...)`. Real code
    # depends on this (`nilTemplate: TStrListRec = (Key: ''; Link: nil)`
    # in a corpus type), and rendering it as a plain list emits the
    # invalid `(kv(Key, ""), ...)`. A `kv` pair is nkCall
    # [kv-ident, name, value].
    if def.len > 2 and def[2].kind == nkPar and
        def[1].kind == nkIdent and def[2].len > 0 and
        def[2][0].kind == nkCall and def[2][0].len == 3 and
        def[2][0][0].kind == nkIdent and def[2][0][0].strVal == "kv" and
        p.recordTypes.hasKey(def[1].strVal.toLowerAscii):
      let oc = newNode(nkOconstr, def[2].info)
      oc.add(def[1])
      for pair in def[2].sons:
        oc.add(pair)
      def[2] = oc
    # a `[...]` initializer whose declared type is a SET must become
    # `{...}`: nimony's `[...]` is an array literal and will not coerce
    # to `set[T]` ("got: array[0..15, char] but wanted: set[char]").
    # Covers both an inline `set of X` and a named alias registered in
    # `p.setTypes` (`TSpecials = set[char]`).
    if def.len > 2 and def[2].kind == nkBracket and
        (def[1].kind == nkSetTy or
         (def[1].kind == nkIdent and
          p.setTypes.hasKey(def[1].strVal.toLowerAscii))):
      def[2].kind = nkCurly
    # `const Values: array[Boolean] of string = ('0', '1')`: char
    # literals in a string-element array const become strings
    if def[1].kind == nkArrayTy and def[1].len > 0 and
        def[1][def[1].len - 1].kind == nkIdent and
        def[1][def[1].len - 1].strVal.toLowerAscii in
            ["string", "ansistring", "shortstring", "unicodestring"]:
      let arr = def[2]
      if arr.kind == nkBracket:
        for ei in 0 ..< arr.sons.len:
          if arr.sons[ei].kind == nkCharLit:
            let sd = newNode(nkStrLit, arr.sons[ei].info)
            sd.strVal = ""
            sd.strVal.add(arr.sons[ei].strVal[0])
            arr.sons[ei] = sd
    # a typed const's type spelling: `with C do` must classify the base
    # (a const is absent from varTypes) and treat it as read-only
    if def.len > 2 and def[1].kind == nkIdent:
      p.constTypes[name.toLowerAscii] = def[1].strVal
    elif def.len > 2 and def[1].kind in {nkArrayTy, nkSeqTy} and
        def[1].len > 0 and def[1][def[1].len - 1].kind == nkIdent:
      let el = def[1][def[1].len - 1].strVal
      p.arrayVarElemTypes[name.toLowerAscii] = el
      if p.recordTypes.hasKey(el.toLowerAscii) or
          p.syms.isClass(el.toLowerAscii):
        p.arrayVarElems[name.toLowerAscii] = el
    result.add(def)
    p.opt(pxSemiColon)
    skipCom(p)

proc elemAliasSpelling(p: var TParser; ty: Node): string =
  ## element spelling of an array-shaped declaration, following ONE
  ## pointer level: `Items: PItems` where `PItems = ^TItemArray` and
  ## `TItemArray = array[..] of TItem` yields `TItem`. The
  ## `with X.Field[i] do` resolver looks the member up through
  ## `classFieldTypes`, so without this the tplbtree node layout
  ## (`with n.Items[x] do`) emitted the body's bare field names
  ## ("undeclared identifier: Node/Value/Key").
  result = ""
  if ty.kind == nkIdent:
    result = p.syms.classSpelling(ty.strVal)
    if result.len == 0:
      result = p.arrayAliases.getOrDefault(ty.strVal.toLowerAscii, "")
    if result.len == 0:
      # a pointer alias: deref once, then try the array alias
      let deref = p.pointerAliases.getOrDefault(ty.strVal.toLowerAscii, "")
      if deref.len > 0:
        result = p.syms.classSpelling(deref)
        if result.len == 0:
          result = p.arrayAliases.getOrDefault(deref.toLowerAscii, "")
  elif ty.kind == nkPtrTy and ty.len > 0:
    result = p.elemAliasSpelling(ty[ty.len - 1])
  elif ty.kind in {nkArrayTy, nkSeqTy} and ty.len > 0 and
      ty[ty.len - 1].kind == nkIdent:
    let el = ty[ty.len - 1].strVal
    if p.syms.isClass(el.toLowerAscii) or p.recordTypes.hasKey(el.toLowerAscii):
      result = el

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
    parent = typeLeafName(parentTy)
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
  if p.tok.xkind == pxSemiColon:
    # forward declaration `IProvider = interface;` - no body follows
    getTokP(p)
    skipCom(p)
    result = newNodeP(nkCommentStmt, p)
    result.strVal = "# interface forward: " & definition.strVal
    return result
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
    if p.tok.xkind == pxIntLit:
      # `procedure X; dispid 2;` - a trailing dispid tail: consume it
      getTokP(p)
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
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    # `T = {$IFDEF X}{$ELSE}packed{$ENDIF} record` - the conditional
    # sits between the `=` and the type keyword
    if not declDirective(p): break
    skipCom(p)
  if p.tok.xkind == pxPacked: getTokP(p)  # {.packed.} handled by renderer
  # the live branch's trailing {$endif} comes after the type keyword
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    if not declDirective(p): break
    skipCom(p)
  case p.tok.xkind
  of pxCommand:
    result = parseDirective(p)
  of pxProcedure, pxFunction:
    result = parseRoutineType(p)
  of pxRecord:
    # anonymous record -> object type
    let oldSelfClass = p.selfClass
    result = newNodeP(nkObjectTy, p)
    result.isRecordType = true
    getTokP(p)
    skipCom(p)
    if definition.kind == nkIdent:
      p.recordTypes[definition.strVal.toLowerAscii] = true
      # `record` bodies are parsed HERE (not by parseRecordBody, which
      # serves `class`/`object`), so this is where the self-class must
      # be published before the fields. The field registry below only
      # records a field when p.selfClass is set, and a record method's
      # bare field reference self-qualifies only if it is recorded.
      p.selfClass = definition.strVal
      # Register the record in the CLASS registry too. It is a value
      # type (`isRef = false`), but the member registry is shared: the
      # field/routine sets hung off a class entry are what
      # `isMemberName` consults, and `selfQualifyAll` only rewrites a
      # bare field reference to `self.X` when that lookup succeeds.
      # Without this, a record method's `X := a` stays unqualified and
      # nimony rejects it with "undeclared identifier: X" - verified on
      # TRec.Init. `addField` also requires the entry to exist, so this
      # must happen before the body is parsed. Records have no class
      # ancestor: the root is `RootObj`, matching the object emission.
      p.syms.registerClass(definition.strVal, "RootObj", false)
    result.add(emptyNode(p.tok.info))     # no inheritance
    let body = newNode(nkRecList, p.tok.info)
    while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
      case p.tok.xkind
      of pxCurlyDirLe, pxStarDirLe:
        # conditionals between record members
        # ({$IFDEF X} field {$ELSE} field {$ENDIF}) - but a group that
        # selects the field NAME (`{$IFDEF POSIX}s6_addr{$ELSE}u6_addr8
        # {$ENDIF}: Integer;`, a corpus unit) must go to the field parser,
        # which condPickIdent handles.
        if dirPicksName(p):
          body.add(parseIdentColonEquals(p, false))
          skipCom(p)
          p.opt(pxSemiColon)
          skipCom(p)
          continue
        if not declDirective(p):
          break
        skipCom(p)
      of pxSymbol:
        let defs = parseIdentColonEquals(p, false)
        # Register the fields, exactly as parseRecordBody does for
        # `class`/`object` bodies. `isMemberName` reads the field set to
        # decide whether a bare name inside a method is `self.X`, so
        # without this a record method's `FX := a` stays unqualified and
        # nimony rejects the member. `classFieldTypes` additionally
        # records the field's class, which the `with` and member-access
        # qualifiers consult.
        if p.selfClass.len > 0 and defs[1].kind != nkProcTy:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind != nkIdent:
              continue
            p.syms.addField(p.selfClass, defs[i].strVal)
            let fkey = p.selfClass.toLowerAscii & "." &
                       defs[i].strVal.toLowerAscii
            var fcls = ""
            if defs[1].kind == nkIdent:
              fcls = p.syms.classSpelling(defs[1].strVal)
              if fcls.len == 0:
                fcls = p.arrayAliases.getOrDefault(
                    defs[1].strVal.toLowerAscii, "")
              if fcls.len == 0:
                fcls = p.elemAliasSpelling(defs[1])
            if fcls.len == 0 and defs[1].kind in {nkArrayTy, nkSeqTy} and
                defs[1].len > 0 and defs[1][defs[1].len - 1].kind == nkIdent:
              fcls = p.syms.classSpelling(defs[1][defs[1].len - 1].strVal)
              if fcls.len == 0 and p.recordTypes.hasKey(
                  defs[1][defs[1].len - 1].strVal.toLowerAscii):
                fcls = defs[1][defs[1].len - 1].strVal
            if fcls.len > 0:
              p.classFieldTypes[fkey] = fcls
        # field types for 1-based string indexing (`rec.field[i]`)
        let mty = p.mappedTypeName(defs[defs.len - 2])
        if definition.kind == nkIdent and mty.len > 0:
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.fieldTypes[definition.strVal.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] = mty
        elif definition.kind == nkIdent and defs[1].kind == nkIdent and
            p.pointerAliases.hasKey(defs[1].strVal.toLowerAscii):
          # pointer-to-array field: the index site needs the explicit
          # deref (`Items: PItems` -> `Items[][x]`)
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.fieldTypes[definition.strVal.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] = defs[1].strVal
        elif definition.kind == nkIdent and
            defs[defs.len - 2].kind == nkIdent and
            (p.procTyTypes.hasKey(defs[defs.len - 2].strVal.toLowerAscii) or
             p.methodPtrTypes.hasKey(defs[defs.len - 2].strVal.toLowerAscii)):
          # a proc-typed record field (`call: TVisitLeaveCall`): remember
          # the spelling so the TMethod(...).Code assignment can cast
          for i in 0 ..< defs.len - 2:
            if defs[i].kind == nkIdent:
              p.fieldTypes[definition.strVal.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] =
                defs[defs.len - 2].strVal
        # array-of-class/record fields: element type for with-index
        if definition.kind == nkIdent:
          var fel = p.syms.classSpelling(defs[1].strVal)
          if fel.len == 0 and defs[1].kind == nkIdent:
            fel = p.arrayAliases.getOrDefault(
                defs[1].strVal.toLowerAscii, "")
          if fel.len == 0 and defs[1].kind in
              {nkIdent, nkPtrTy, nkArrayTy, nkSeqTy}:
            fel = p.elemAliasSpelling(defs[1])
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
      of pxProperty:
        # `property` inside a `record`/`object` body:
        #   property At[i: Integer]: AnsiChar read GetChar; default;
        #   property Content: AnsiString read _s;
        # Three corpus units declare properties on
        # value types. parseProperty already handles the accessor
        # lowering; the record path simply never called it.
        discard parseProperty(p)
        p.opt(pxSemiColon)
        skipCom(p)
      of pxFunction, pxProcedure, pxConstructor, pxDestructor:
        # Methods declared INSIDE a `record`/`object` body:
        #   TSingleLinkedList = record
        #     FFirst: PSingleLinkedItem;
        #     procedure Init;
        #     function Step(out AItem): Boolean; inline;
        #   end;
        # Delphi value types with methods are pervasive in the corpus
        # (72 sites across 21 units). The bodiless
        # declaration stays in the body: the emitter already hoists an
        # `nkProcDef` out of an object type into a module-level forward,
        # and the implementation (`procedure TSingleLinkedList.Init`)
        # defines it. Field access inside those bodies self-qualifies
        # through the field registry, which is why p.selfClass is set.
        # `class procedure/function` is handled in the pxClass branch.
        body.add(parseRoutine(p, true))
        p.opt(pxSemiColon)
        skipCom(p)
      of pxCase:
        let flat = parseRecordCase(p)
        for k in 0 ..< flat.len:
          body.add(flat[k])
        p.opt(pxSemiColon)
        skipCom(p)
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
    p.selfClass = oldSelfClass
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
      # static array: array[lo..hi] of T; multi-dim dims lower to
      # nested arrays (v1): array[a..b, c..d] of T is
      # array[0..n-1] of array[0..m-1] of T
      getTokP(p)
      let idx = rangeExpr(p)
      var dims: seq[Node] = @[idx]
      while p.tok.xkind == pxComma:
        getTokP(p)
        skipCom(p)
        dims.add(rangeExpr(p))
      if definition.kind == nkIdent and idx.kind == nkRange and
          idx.len == 2 and idx[0].kind in {nkIntLit, nkInt64Lit}:
        # alias types carry their low bound for var declarations
        p.arrayTypeLows[definition.strVal.toLowerAscii] = int(idx[0].intVal)
      p.eat(pxBracketRi)
      p.syms = p.syms  # no-op; keep table
      if dims.len == 1:
        result.add(idx)
      else:
        # `array[a..b, c..d] of T` -> `array[.] of array[.] of T`: each
        # dimension contributes exactly one link, the first on `result`.
        # The old loop never added dims[0], so the second dimension
        # landed in the INDEX slot and the emitted type was
        # `array[<array>, T]` - nifler's "expected: ']', but got: ...".
        result.add(dims[0])
        var prev: Node = result
        for i in 1 ..< dims.len:
          var outer = newNode(nkArrayTy, p.tok.info)
          outer.add(dims[i])
          outer.add(emptyNode(p.tok.info))
          prev.add(outer)
          prev = outer
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
    let elemTy = parseTypeDesc(p, emptyNode(p.tok.info))
    if result.len == 2 and result[1].kind == nkArrayTy:
      # a multi-dimension chain built just above: walk to the innermost
      # link (only those links carry an nkArrayTy in the element slot
      # while the element type is still missing) and attach it there
      var link = result
      while link.len == 2 and link[1].kind == nkArrayTy and
          link[1].len == 2:
        link = link[1]
      link[1] = elemTy
    else:
      result.add(elemTy)
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
    if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "dispinterface":
      # `X = dispinterface;` (forward) or a full body: a COM dual
      # automation interface - v1 has no lowering, skip it
      getTokP(p)
      skipCom(p)
      p.opt(pxSemiColon)
      result = newNodeP(nkCommentStmt, p)
      result.strVal = "# dispinterface: " & definition.strVal & " (v1 skip)"
      p.context = oldcontext
      return
    elif p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "reference" and
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
            elif defs[1].kind == nkIdent and
                p.pointerAliases.hasKey(defs[1].strVal.toLowerAscii):
              # a pointer-to-array field: record the ALIAS spelling so
              # the index site can tell it needs an explicit deref
              # (`n.Items[][x]` - nimony cannot index a `ptr array`)
              p.fieldTypes[p.selfClass.toLowerAscii & "." &
                           defs[i].strVal.toLowerAscii] = defs[1].strVal
            var fcls = p.syms.classSpelling(defs[1].strVal)
            if fcls.len == 0 and defs[1].kind == nkIdent:
              # array-alias field: `FBuckets: TBucketArray` -> element
              fcls = p.arrayAliases.getOrDefault(defs[1].strVal.toLowerAscii, "")
            if fcls.len == 0 and defs[1].kind in
                {nkIdent, nkPtrTy, nkArrayTy, nkSeqTy}:
              fcls = p.elemAliasSpelling(defs[1])
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
      elif pk == pxProperty:
        # `class property Name[id: Integer]: String read getName;`
        # (hashBtree, threadpool). A class property is a property whose
        # accessors are class routines; parseProperty already lowers the
        # accessor pair, and the accessor names were registered by the
        # `class function` declarations above, so no extra lowering is
        # needed - only the `class` prefix has to be consumed here.
        getTokP(p)                  # consume `class`
        skipCom(p)
        discard parseProperty(p)
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
  # Delphi class/record HELPER:
  #   TFooHelper = class helper for TFoo
  #     procedure CallClearFixups; inline;
  #   end;
  # A helper adds methods to the helped type without changing it
  # (three corpus units use them). The
  # translation lowers the helper to a class OF the helped type, so
  # p.syms.registerClass below records the inheritance and every method
  # body resolves `self` against the real class. For a `record helper`
  # the helped type is a value type; the same lowering applies, since
  # the emitter already models records as objects.
  if p.tok.xkind == pxHelper:
    getTokP(p)
    skipCom(p)
    # the optional helper KIND: `class helper`, `record helper`,
    # `type helper` - already consumed the `class`/`object` above
    if p.tok.xkind == pxFor:
      getTokP(p)
      skipCom(p)
      if p.tok.xkind == pxSymbol:
        # publish the helped type as the parent before the body, so the
        # helper's methods inherit field/method resolution from it
        let helped = p.tok.ident
        getTokP(p)
        skipCom(p)
        block:
          let ofInh = newNode(nkOfInherit, definition.info)
          ofInh.add(newIdentNode(helped, definition.info))
          if kind == nkRefTy:
            record.add(ofInh)
          else:
            record.add(ofInh)
        p.syms.registerClass(definition.strVal, helped, kind == nkRefTy)
        parseRecordBody(p, record, definition)
        p.opt(pxSemiColon)
        return result
      parError(p, "type name expected after `helper for`")
    else:
      parError(p, "`for` expected after `helper`")
  if p.tok.xkind == pxOf:
    # metaclass type: `TPersistentClass = class of TPersistent;` and
    # `procedure Foo(A: TComponentClass)` - a class-reference type;
    # v1 has no lowering, so it becomes a comment alias
    getTokP(p)
    skipCom(p)
    let base = parseTypeDesc(p, emptyNode(p.tok.info))
    p.opt(pxSemiColon)
    result = newNode(nkCommentStmt, definition.info)
    result.strVal = "# metaclass: " & definition.strVal & " = class of " &
                    base.strVal & " (v1 skip)"
    return
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
    parent = typeLeafName(parentTy)
    # Delphi implements list: (Parent, IIntf1, IInt2)
    var interfaces: seq[string] = @[]
    while p.tok.xkind == pxComma:
      getTokP(p)
      skipCom(p)
      let itfTy = parseTypeDesc(p, emptyNode(p.tok.info))
      let itfName = typeLeafName(itfTy)
      if itfName.len > 0:
        interfaces.add(itfName)
    let ofInh = newNode(nkOfInherit, parentTy.info)
    if interfaces.len > 0:
      # v1: the implements list is plumbing - the class body declares
      # the interface methods itself, so the lowering keeps exactly
      # the parent type. TInterfacedObject dissolves into the first
      # interface's generated class (the parentless-interface case).
      if parent.toLowerAscii == "tinterfacedobject" or parent.len == 0:
        parent = interfaces[0]
      ofInh.add(newIdentNode(parent, parentTy.info))
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
  # a bare `property Name;` re-exposes the ancestor's property: no
  # type clause follows (ZLib's `property OnProgress;`)
  if p.tok.xkind == pxSemiColon:
    getTokP(p)
    skipCom(p)
    result.strVal = "# property re-exposed: " & propName
    return
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
          decl.readPath = @[p.tok.ident]
          getTokP(p)
          # A dotted accessor (`read slice.Last`, `read FValue.VBoolean`)
          # reads THROUGH a field. Keep the whole path: the getter must be
          # `self.slice.last`, not a bare `self.last`.
          while p.tok.xkind == pxDot:
            getTokP(p)
            skipCom(p)
            if p.tok.xkind != pxSymbol:
              parError(p, "identifier expected after '.' in property accessor")
            decl.readId = p.tok.ident
            decl.readPath.add(p.tok.ident)
            getTokP(p)
      elif word == "write":
        getTokP(p)
        if p.tok.xkind == pxSymbol:
          decl.writeId = p.tok.ident
          getTokP(p)
          while p.tok.xkind == pxDot:
            getTokP(p)
            skipCom(p)
            if p.tok.xkind != pxSymbol:
              parError(p, "identifier expected after '.' in property accessor")
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
      elif word == "implements":
        # `property List: TAdrList read FList implements IAdrList;`
        # - a COM interface delegation. v1 has no
        # interface plumbing, so the clause is parsed and dropped.
        getTokP(p)
        while p.tok.xkind == pxSymbol or p.tok.xkind == pxDot:
          getTokP(p)
        skipCom(p)
      elif word == "readonly" or word == "writeonly":
        # COM automation access specifiers (StdVCL): v1 no-op
        getTokP(p)
      elif word == "stored" or word == "immutable":
        # streaming specifiers: `stored False` / bare `stored`
        getTokP(p)
        if p.tok.xkind notin {pxSemiColon, pxComma, pxBracketLe} and
            p.tok.xkind != pxEof:
          discard parseExpr(p)
      elif word == "dispid":
        # `dispid N` tail: consume the integer
        getTokP(p)
        if p.tok.xkind in {pxIntLit, pxMinus, pxPlus}:
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
  p.recordTypes[nameNode.strVal.toLowerAscii] = true
  p.fieldTypes[nameNode.strVal.toLowerAscii & ".evproc"] = "proc:ev"
  p.fieldTypes[nameNode.strVal.toLowerAscii & ".evobj"] = "pointer"
  let info = nameNode.info
  var recList = newNode(nkRecList, info)
  var fp = newNode(nkFormalParams, info)
  fp.add(params[0])                  # return type (function method-ptrs)
  var sd = newNode(nkIdentDefs, info)
  sd.add(newIdentNode("self", info))
  sd.add(newIdentNode("pointer", info))
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
  od.add(newIdentNode("pointer", info))
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
  fp.add(params[0])                  # return type (function method-ptrs)
  var sd = newNode(nkIdentDefs, info)
  sd.add(newIdentNode("self", info))
  sd.add(newIdentNode("pointer", info))
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
  if params.len > 0 and params[0].kind != nkEmpty:
    var ra = newNode(nkAsgn, info)
    ra.add(newIdentNode("result", info))
    ra.add(call)
    body.add(ra)
  else:
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
        objAsgn.add(hardCastNode("pointer", b[0]))
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
        objAsgn.add(hardCastNode("pointer", newIdentNode("self", info)))
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

proc lowerTMethodField(p: var TParser, n: Node): Node =
  ## `TMethod(x).Code`/`.Data` -> the method-pointer's evProc/evObj
  result = n
  if n.kind == nkDotExpr and n.len == 2 and n[1].kind == nkIdent:
    let f = n[1].strVal.toLowerAscii
    if f in ["code", "data"]:
      var b = n[0]
      while b.kind == nkPar and b.len >= 1: b = b[0]
      if b.kind == nkCall and b.len == 2 and b[0].kind == nkIdent and
          b[0].strVal.toLowerAscii == "tmethod":
        result = newDotP(b[1], if f == "code": "evProc" else: "evObj")

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
        result[2].len > 1 and result[2][1].kind == nkPragma and
        result[2][1].len > 0 and result[2][1][0].kind == nkIdent and
        result[2][1][0].strVal == "closure":
      result[2] = p.methodPtrRecord(result[0], result[2])
    elif result.len == 3 and result[2].kind == nkProcTy:
      # plain proc-type alias: Assigned(x) lowers to `x != nil`
      p.procTyTypes[name.toLowerAscii] = true
      if result[2].len > 0 and result[2][0].len > 0:
        p.procTyParams[name.toLowerAscii] = result[2][0]
      if result[2].len > 0 and result[2][0].len > 0 and
          result[2][0][0].kind != nkEmpty:
        let rn = result[2][0][0]
        let rs = if rn.kind == nkIdent: rn.strVal else: p.mappedTypeName(rn)
        if rs.len > 0: p.procTyReturns[name.toLowerAscii] = rs
  else:
    result.add(emptyNode(nameInfo))
  if p.tok.xkind == pxSemiColon:
    getTokP(p)
    skipCom(p)
  # a calling convention after a proc-type alias: `TFoo = function
  # (...): HRESULT; stdcall;` - stdcall/cdecl forward as pragmas on
  # the proc type (nimony's proc pragma must match the callee's);
  # register/pascal/safecall have no nimony model and are consumed
  while p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii in
      ["stdcall", "cdecl", "register", "pascal", "safecall"]:
    let ccName = p.tok.ident.toLowerAscii
    getTokP(p)
    skipCom(p)
    if ccName in ["stdcall", "cdecl"] and result.len == 3 and
        result[2].kind == nkProcTy:
      var pr = newNode(nkPragma, p.tok.info)
      if result[2].len > 1 and result[2][1].kind == nkPragma:
        pr = result[2][1]
      pr.add(newIdentNode(ccName, p.tok.info))
      if result[2].len > 1:
        result[2][1] = pr
      else:
        result[2].add(pr)
    if p.tok.xkind == pxSemiColon:
      getTokP(p)
      skipCom(p)
  # a `set of X` alias is remembered for set-literal assignment
  if result.len == 3 and result[2].kind == nkSetTy:
    p.setTypes[name.toLowerAscii] = true
  # a `specialize` alias is a first-class class type
  if result.len == 3 and result[2].kind == nkIndexExpr:
    p.syms.registerSpecializedAlias(result[2][0].strVal, name)
  p.curTypeParams = @[]

proc parseTypeDefList(p: var TParser): Node =
  ## the interior of a `type` section: definitions until something that
  ## cannot start one. A forwarded `{$if <expr>}` group is kept INSIDE
  ## the section as an nkWhenExpr so a conditional may wrap definitions
  ## the way Delphi allows; the module renderer splits the section around
  ## it, because nimony rejects a `when` inside `type`.
  result = newNodeP(nkTypeSection, p)
  while true:
    skipCom(p)                  # comments between definitions
    # directives between type definitions ({$EXTERNALSYM ...},
    # conditionals) must not close the section
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      let dir = p.tok.ident.toLowerAscii
      # a closer is only OURS when the enclosing group is forwarded to
      # Nim; a parse-time group ({$IFDEF}/{$IFNDEF}) owns its own closer
      # and declDirective below consumes it (same rule as there).
      if dir in ["else", "elseif", "endif", "ifend"] and
          condWhenStack.len > 0 and condWhenStack[^1] != 0:
        break
      if dir == "if":
        # forwarded, not evaluated: the arm holds TYPE DEFINITIONS
        result.add(parseIfDir(p, succ(p.tok.xkind), cmType))
        continue
      if declDirective(p):
        continue
      break
    if p.tok.xkind != pxSymbol:
      break
    let def = parseTypeDef(p)
    skipCom(p)
    # `PRec = ^TRec` alias: track the element for with-deref
    # resolution (`with PRec(expr)^ do`)
    if def.len == 3 and def[0].kind == nkIdent and
        def[2].kind == nkPtrTy and def[2].len > 0 and
        def[2][def[2].len - 1].kind == nkIdent:
      p.pointerAliases[def[0].strVal.toLowerAscii] =
        def[2][def[2].len - 1].strVal
    # `TArr = array of TRec` alias: track the element for with-index
    # resolution (`with propOfTArr do`)
    if def.len == 3 and def[0].kind == nkIdent and
        def[2].kind in {nkArrayTy, nkSeqTy} and def[2].len > 0 and
        def[2][def[2].len - 1].kind == nkIdent:
      let el = def[2][def[2].len - 1].strVal.toLowerAscii
      if p.syms.isClass(el) or p.recordTypes.hasKey(el):
        p.arrayAliases[def[0].strVal.toLowerAscii] =
          def[2][def[2].len - 1].strVal
    # a simple `TAlias = TTarget` alias: remember the target so the
    # pointer/ref classification resolves through it (TValueType = TObject,
    # the RootRef alias)
    if def.len == 3 and def[0].kind == nkIdent and def[2].kind == nkIdent:
      p.typeAliasTargets[def[0].strVal.toLowerAscii] =
        def[2].strVal.toLowerAscii
    result.add(def)

proc parseTypeSection*(p: var TParser): Node =
  result = newNodeP(nkTypeSection, p)
  getTokP(p)                    # skip `type`
  skipCom(p)
  let body = parseTypeDefList(p)
  for d in body.sons: result.add(d)

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
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # `function F(...): T; {$IFDEF} external; {$ENDIF}` - the
      # conditional may wrap the specifiers; dead branches skip at
      # the token level, live ones flow back into the loop
      if not declDirective(p):
        break
      continue
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
    of "platform", "experimental", "assembler":
      # genuine no-ops: documentation annotations with no
      # nimony-side semantic (assembler marks free-form bodies that
      # v1 has no backend for)
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

proc scanParamAssigned(n: Node; names: Table[string, bool];
                       assigned: var Table[string, bool]) =
  ## direct reassignment / Inc / Dec of a value parameter in a body
  if n.kind == nkAsgn and n.len == 2 and n[0].kind == nkIdent:
    let k = n[0].strVal.toLowerAscii
    if names.hasKey(k): assigned[k] = true
  if n.kind == nkCall and n.len >= 2 and n[0].kind == nkIdent and
      n[0].strVal.toLowerAscii in ["inc", "dec"] and n[1].kind == nkIdent:
    let k = n[1].strVal.toLowerAscii
    if names.hasKey(k): assigned[k] = true
  for c in n.sons: scanParamAssigned(c, names, assigned)

proc lowerMutableValueParams(p: var TParser; params, body: Node) =
  ## Pascal value parameters are mutable; nimony value parameters are
  ## not. For each value param directly reassigned in the body, rename
  ## the formal and prepend `var <name> = <fresh>`, so the body keeps
  ## its own mutable local and call sites stay pass-by-value.
  if body.kind != nkStmtList: return
  var names = initTable[string, bool]()
  var tyOf = initTable[string, Node]()
  var identOf = initTable[string, Node]()
  var order: seq[string] = @[]
  for i in 1 ..< params.len:
    let d = params[i]
    if d.kind != nkIdentDefs: continue
    let ty = d[d.len - 2]
    if ty.kind == nkVarTy: continue     # var/out: already mutable
    for j in 0 ..< d.len - 2:
      if d[j].kind == nkIdent:
        let k = d[j].strVal.toLowerAscii
        if not names.hasKey(k):
          names[k] = true
          order.add(k)
        tyOf[k] = ty
        identOf[k] = d[j]
  if names.len == 0: return
  var assigned = initTable[string, bool]()
  scanParamAssigned(body, names, assigned)
  if assigned.len == 0: return
  var vs = newNode(nkVarSection, body.info)
  for k in order:
    if not assigned.hasKey(k): continue
    let idn = identOf.getOrDefault(k)
    let nm = idn.strVal
    let fresh = "pasV_" & nm
    idn.strVal = fresh
    var d = newNode(nkIdentDefs, body.info)
    d.add(newIdentNode(nm, body.info))
    d.add(tyOf.getOrDefault(k))
    d.add(newIdentNode(fresh, body.info))
    vs.add(d)
  var ns: seq[Node] = @[]
  ns.add(vs)
  for c in body.sons: ns.add(c)
  body.sons = ns
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
      if p.tok.ident.toLowerAscii == "if":
        # `{$if <expr>}` guarding local declarations (`{$IF not
        # declared(_maxspan)} const _maxspan = 7; {$IFEND}`, every
        # tplQSort*.inc): answerable only by a compiler, so forward it
        # as a `when` whose arms hold the local decls. The arms are
        # parsed as statements, and a `const`/`var`/`type` section in
        # statement position yields exactly the node the emitter wants.
        stmts.add(parseIfDir(p, succ(p.tok.xkind)))
        if p.tok.xkind == pxSemiColon:
          getTokP(p)
          skipCom(p)
        continue
      # a closer of an enclosing group: its owner must see it
      break
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
  let oldOuterResultTy = p.outerResultTy
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
    # qualified: `MyClass.doIt`, or an interface-method mapping clause
    # `function IUnknown.QueryInterface = ObjQueryInterface;`
    let cls = name
    p.removeNextTok()
    skipCom(p)
    if p.tok.xkind != pxSymbol:
      parError(p, "method name expected, got " & $p.tok)
    name = p.tok.ident
    getTokP(p)
    skipCom(p)
    if p.tok.xkind == pxEquals:
      # mapping clause: `function IIntf.Member = ImplName;` - v1
      # lowers it to a comment; the implementation keeps its own name
      var implName = ""
      if p.tok.xkind == pxParLe:
        # mapping clauses may repeat the parameter list
        discard parseParamList(p)
        skipCom(p)
      if p.tok.xkind == pxColon:
        getTokP(p)
        skipCom(p)
        discard parseTypeDesc(p, emptyNode(p.tok.info))
        skipCom(p)
      if p.tok.xkind == pxEquals:
        getTokP(p)
        skipCom(p)
        if p.tok.xkind == pxSymbol:
          implName = p.tok.ident
          getTokP(p)
          skipCom(p)
      p.opt(pxSemiColon)
      skipCom(p)
      result = newNodeP(nkCommentStmt, p)
      result.strVal = "# interface method: " & cls & "." & name & " = " &
                      implName & " (v1: implementation keeps its name)"
      return
    p.selfClass = cls
    p.classOfProc = cls
    isMethod = true
    isDotted = true
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
  block:
    # which parameters are Variant-typed: nimony applies no converters, so
    # the call site has to build the Variant value itself. Recorded for
    # procedures as well (routineParams is only filled for functions).
    var vps: seq[bool] = @[]
    for pi in 1 ..< params.len:
      let d = params[pi]
      vps.add(d.kind == nkIdentDefs and d.len >= 2 and
              d[d.len - 2].kind == nkIdent and
              d[d.len - 2].strVal.toLowerAscii in ["variant", "olevariant"])
    if true in vps:
      p.variantParams[name.toLowerAscii] = vps
      if p.classOfProc.len > 0:
        p.variantParams[p.classOfProc.toLowerAscii & "." & name.toLowerAscii] = vps
      if p.selfClass.len > 0:
        p.variantParams[p.selfClass.toLowerAscii & "." & name.toLowerAscii] = vps
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
  # interface defaults must repeat on the implementation redeclaration
  # (constructors have an empty params[0]: the return rides the class
  # registry, so this runs before the return-slot registration)
  var pspD = 0
  var pdef: seq[Node] = @[]
  for piD in 1 ..< params.len:
    let dD = params[piD]
    if dD.kind == nkIdentDefs:
      pspD = pspD + 1
      pdef.add(dD[dD.len - 1])
  if p.section != seInterface and p.classOfProc.len > 0 and pspD > 0:
    # implementation header: splice the interface's defaults (Delphi
    # defaults live on the interface header only; nimony treats a
    # missing default as a distinct overload - ambiguous calls)
    let dkey = p.classOfProc.toLowerAscii & "." & name.toLowerAscii & ":" & $pspD
    let defs = p.routineParamDefaults.getOrDefault(dkey)
    if defs.len > 0:
      var pi2 = 1
      var di = 0
      while pi2 < params.len and di < defs.len:
        let d2 = params[pi2]
        if d2.kind == nkIdentDefs:
          if d2[d2.len - 1].kind == nkEmpty and defs[di].kind != nkEmpty:
            d2[d2.len - 1] = defs[di]
          inc di
        inc pi2
  elif p.section == seInterface and p.classOfProc.len > 0 and pspD > 0:
    p.routineParamDefaults[p.classOfProc.toLowerAscii & "." &
                           name.toLowerAscii & ":" & $pspD] = pdef
  if params[0].kind == nkIdent:
    # the routine's return spelling for `with Call(...)` lowering
    let retSp = params[0].strVal
    p.routineReturns[name.toLowerAscii] = retSp
    # the param spellings for char-arg call-site decisions
    var psp: seq[string] = @[]
    for pi in 1 ..< params.len:
      let d = params[pi]
      if d.kind == nkIdentDefs and d[d.len - 2].kind == nkIdent:
        psp.add(d[d.len - 2].strVal)
    if psp.len > 0:
      p.routineParams[name.toLowerAscii] = psp
      if p.classOfProc.len > 0:
        p.routineParams[p.classOfProc.toLowerAscii & "." & name.toLowerAscii] = psp
      if p.selfClass.len > 0:
        p.routineParams[p.selfClass.toLowerAscii & "." & name.toLowerAscii] = psp
    if p.classOfProc.len > 0:
      p.routineReturns[p.classOfProc.toLowerAscii & "." &
                       name.toLowerAscii] = retSp
    if p.classOfProc.len == 0 and p.selfClass.len > 0:
      p.routineReturns[p.selfClass.toLowerAscii & "." &
                       name.toLowerAscii] = retSp
  # `result` carries the routine's return type for conversion
  # insertion in the body (`result := intExpr` on a float/record
  # return)
  if params[0].kind == nkIdent:
    let retKey = params[0].strVal.toLowerAscii
    if p.recordTypes.hasKey(retKey):
      p.varTypes["result"] = "record:" & params[0].strVal
    elif p.setTypes.hasKey(retKey):
      p.varTypes["result"] = "set"
    elif p.syms.isClass(retKey):
      # a class return must reset the slot: the stale previous
      # routine's width would cast the next body's result asgn
      p.varTypes["result"] = "class:" & params[0].strVal
    else:
      let rtl = rtlSpelling(retKey)
      if rtl.len > 0: p.varTypes["result"] = rtl
      elif p.pointerAliases.hasKey(retKey):
        p.varTypes["result"] = "ptr:" & params[0].strVal
      elif ptrKindOf(p, params[0].strVal).startsWith("ref:"):
        p.varTypes["result"] = "class:" & params[0].strVal
      else: p.varTypes["result"] = "unknown" 
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
  if oldOuterName.len > 0:
    # a nested standalone proc captures the enclosing routine's locals;
    # nimony requires an explicit `.closure` pragma for that
    pragmas.add(newIdentNode("closure", nameInfo))
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
    p.outerResultTy = p.mappedTypeName(params[0]).toLowerAscii
    # register the member's param type spellings for inherited-call
    # arg coercion (the corpus's Pointer-vs-TObject params)
    if p.selfClass.len > 0:
      var ptyParts: seq[string] = @[]
      for i in 1 ..< params.len:
        let d = params[i]
        if d.kind == nkIdentDefs:
          let dmty = p.mappedTypeName(d[d.len - 2])
          let parts = if dmty.len > 0: dmty
                      else: d[d.len - 2].strVal.toLowerAscii
          for j in 0 ..< d.len - 2:
            ptyParts.add(parts)
      p.syms.addMemberParams(p.selfClass, name, ptyParts.join(";"))
    # param types for 1-based string indexing inside the body
    let savedParamTypes = p.paramTypes
    let savedMethodPtrVars = p.methodPtrVars
    let savedConstParams = p.constParams
    p.paramTypes = initTable[string, string]()
    p.constParams = initTable[string, bool]()
    for i in 1 ..< params.len:
      let d = params[i]
      if d.kind == nkIdentDefs:
        if d.isImmutable:
          for j in 0 ..< d.len - 2:
            if d[j].kind == nkIdent:
              p.constParams[d[j].strVal.toLowerAscii] = true
        var mty = p.mappedTypeName(d[d.len - 2])
        if mty.len == 0 and d[d.len - 2].kind == nkIdent and
            p.syms.classes.hasKey(d[d.len - 2].strVal.toLowerAscii):
          # a class-typed param: the ref-eq rewrite keys on it
          mty = "class:" & d[d.len - 2].strVal.toLowerAscii
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
        # open-array params (`const MaskStates: array of TMaskState`):
        # the element spelling for `with Param[i] do` lowering
        if pty.kind in {nkOpenArrayTy, nkSeqTy} and pty.len > 0 and
            pty[pty.len - 1].kind == nkIdent:
          let el = pty[pty.len - 1].strVal
          for j in 0 ..< d.len - 2:
            if d[j].kind == nkIdent:
              p.arrayVarElemTypes[d[j].strVal.toLowerAscii] = el
          if p.syms.isClass(el.toLowerAscii) or
              p.recordTypes.hasKey(el.toLowerAscii):
            for j in 0 ..< d.len - 2:
              if d[j].kind == nkIdent:
                p.arrayVarElems[d[j].strVal.toLowerAscii] = el
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
    lowerMutableValueParams(p, params, result[result.len - 1])
    # class-reference `=`/`<>` rewrite to sameRef while this routine's
    # param/local maps are still live (nimony has no ref equality)
    if result.len > 0:
      rewriteClassEq(p, result[result.len - 1])
      rewritePtrAddr(p, result[result.len - 1])
      result[result.len - 1] = coercePtrCasts(p, result[result.len - 1])
      coercePtrAsgns(p, result[result.len - 1])
      coercePtrArgs(p, result[result.len - 1])
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
    p.outerResultTy = oldOuterResultTy
    p.outerParams = savedOuterParams
    p.paramTypes = savedParamTypes
    p.methodPtrVars = savedMethodPtrVars
    p.constParams = savedConstParams
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
    # `inherited` in a method of a type with NO ancestor. Delphi allows
    # this and it means "the same-named method of the parent", which for
    # a parentless record is simply "this method" - the standard
    # idiom is a record whose Add calls `inherited Add(Item)`, i.e. the
    # implementation the record's own class provides (vStrLst:
    # `result := inherited Add(Item)`). Static dispatch, so leave the
    # call unqualified; selfQualifyAll turns it into `self.Add(...)`,
    # which is the very method being defined. Emitting a diagnostic
    # instead would drop the assignment and lose the result.
    if p.classOfProc.len > 0 and p.tok.xkind == pxSymbol:
      let nm = p.tok.ident
      getTokP(p)
      skipCom(p)
      result = newNode(nkCall, info)
      result.noQualCallee = true
      result.add(newIdentNode(nm, info))
      if p.tok.xkind == pxParLe:
        getTokP(p)
        skipCom(p)
        while p.tok.xkind != pxParRi and p.tok.xkind != pxEof:
          result.add(parseExpr(p))
          skipCom(p)
          if p.tok.xkind == pxComma:
            getTokP(p)
            skipCom(p)
        p.eat(pxParRi)
    else:
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
    # the arg form with the cast to the member's DECLARING class:
    # nimsem resolves the bare name by the receiver's static type, and
    # the slot at that class carries the declaring signature
    var declCls = parent
    var k = parent.toLowerAscii
    var guard = 0
    while k.len > 0 and guard < 100:
      let ci = p.syms.classes.getOrDefault(k)
      if ci.spelling.len == 0: break
      if ci.routineSet.hasKey(p.outerProcName.toLowerAscii):
        declCls = ci.spelling
        break
      k = ci.parent
      inc guard
    let declCast = newNode(nkCast, info)
    declCast.add(newIdentNode(declCls, info))
    declCast.add(selfNode)
    call.add(newIdentNode(p.outerProcName, info))
    call.add(declCast)
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
    # capture the member name before parseStmt: statement-position
    # builtin mapping may rename the call's callee (the corpus's bare
    # `Insert(s, s2, idx)` becomes strInsert) and would defeat the
    # same-name test below
    let memberName = if p.tok.xkind == pxSymbol:
      p.tok.ident.toLowerAscii
    else:
      ""
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
    # `inherited SameName(...)` binds the bare name to the current
    # (child) method; qualify the parent type so nimsem checks the
    # parent's own signature
    let sameName = p.outerProcName.len > 0 and
        (memberName == p.outerProcName.toLowerAscii or
         (a.kind == nkCall and a.len > 0 and a[0].kind == nkIdent and
          a[0].strVal.toLowerAscii == p.outerProcName.toLowerAscii) or
         (a.kind == nkIdent and
          a.strVal.toLowerAscii == p.outerProcName.toLowerAscii))
    # the corpus's Pointer-vs-TObject member params: coerce class-typed
    # args to pointer form when the ancestor's member declares Pointer
    var mpRaw = p.syms.memberParamsOf(parent, memberName)
    if mpRaw.len == 0:
      # walk the ancestor chain: the member may be declared above the
      # direct parent (TObjectStack.Push -> TOrderedList.Push)
      var wk = parent.toLowerAscii
      var wguard = 0
      while wk.len > 0 and wguard < 100:
        let wci = p.syms.classes.getOrDefault(wk)
        if wci.spelling.len == 0: break
        mpRaw = p.syms.memberParamsOf(wci.spelling, memberName)
        if mpRaw.len > 0: break
        wk = wci.parent
        inc wguard
    var mpParts: seq[string] = @[]
    if mpRaw.len > 0:
      var cur = ""
      for ch in mpRaw:
        if ch == ';':
          mpParts.add(cur)
          cur = ""
        else:
          cur.add(ch)
      mpParts.add(cur)
    if a.kind == nkCall and mpParts.len > 0:
      for i in 1 ..< a.len:
        let pi = i - 1
        if pi < mpParts.len and mpParts[pi].toLowerAscii == "pointer" and
            a[i].kind == nkIdent:
          let aty = p.paramTypes.getOrDefault(a[i].strVal.toLowerAscii)
          if aty.toLowerAscii == "rootref" or
              aty.toLowerAscii.startsWith("class:"):
            let pcn = newNode(nkCast, a[i].info)
            pcn.add(newIdentNode("pointer", a[i].info))
            pcn.add(a[i])
            a[i] = pcn
    if sameName:
      var declCls = parent
      var k = parent.toLowerAscii
      var guard = 0
      while k.len > 0 and guard < 100:
        let ci = p.syms.classes.getOrDefault(k)
        if ci.spelling.len == 0: break
        if ci.routineSet.hasKey(p.outerProcName.toLowerAscii):
          declCls = ci.spelling
          break
        k = ci.parent
        inc guard
      let declCast = newNode(nkCast, info)
      declCast.add(newIdentNode(declCls, info))
      declCast.add(selfNode)
      # the callee spelling: a constructor must render through the name
      # registry (the source may write `inherited create(...)` for a
      # member the registry spells `Create`, and nimony is
      # case-sensitive), and the prelude exception ctor is spelled
      # pasExcCreate in systempas; any other member keeps its raw
      # spelling (noQualCallee keeps the builtin map off member calls)
      var callee = p.outerProcName
      let dci = p.syms.classes.getOrDefault(declCls.toLowerAscii)
      if dci.ctorSet.hasKey(memberName):
        callee = if declCls == "PasException" and memberName == "create":
                   "pasExcCreate"
                 else:
                   p.syms.canonical(p.outerProcName)
      call = newNode(nkCall, a.info)
      call.add(newIdentNode(callee, info))
      call.add(declCast)
      if a.kind == nkCall:
        for i in 1 ..< a.len:
          call.add(a[i])
      else:
        # `inherited Row + ',' + Name` (uTest1/uTest1b): the member is
        # one OPERAND of a larger expression, not a call. Rebinding only
        # the leftmost leaf keeps the rest of the expression; the old
        # code dropped everything but the member reference, silently
        # losing every operand after it. The leaf is usually already a
        # paren-less CALL (`Row(self)`), since parseStmt applied the
        # zero-arg rule to the bare name.
        # nkInfix is [op, lhs, rhs], so the leftmost operand is son 1
        var leaf = a
        while leaf.kind == nkInfix and leaf.len >= 3:
          leaf = leaf[1]
        var leafName = ""
        if leaf.kind == nkIdent:
          leafName = leaf.strVal.toLowerAscii
        elif leaf.kind == nkCall and leaf.len > 0:
          # the callee is bare (`Row`) or already self-qualified
          # (`self.Row`) by the paren-less rule
          leafName = typeLeafName(leaf[0]).toLowerAscii
        if leafName == p.outerProcName.toLowerAscii:
          if leaf.kind == nkCall:
            # sons = [callee, (self), args...]: keep the args, swap the
            # receiver for the parent cast and unbind the bare spelling
            leaf[0] = newIdentNode(callee, info)
            if leaf.len >= 2:
              leaf[1] = declCast
            else:
              leaf.add(declCast)
            leaf.noQualCallee = true
          else:
            leaf.strVal = ""
            leaf.kind = nkCall
            leaf.add(newIdentNode(callee, info))
            leaf.add(declCast)
            leaf.noQualCallee = true
          call = a
        else:
          call.add(a)
    elif a.kind == nkIndexExpr or (a.kind == nkAsgn and a.len == 2 and
        a[0].kind == nkIndexExpr):
      # `inherited Items[Index]` / `inherited Items[Index] := v`: the
      # ancestor's default indexed property lowers to the parent's
      # indexer shim (`[]` / `[]=`) on the parent cast; the call form
      # (`Items[Index](castParent)`) is not callable in nimony
      if a.kind == nkIndexExpr:
        let ie = newNode(nkIndexExpr, a.info)
        ie.add(parentCast)
        ie.add(a[1])
        # the object-typed outer result needs the pointer->object cast
        # (the shim's [] returns pointer; nimsem rejects the implicit).
        # outerResultTy is stored lowercased, and nimsem's cast check is
        # case sensitive, so `RootRef` needs its real spelling back.
        if p.outerResultTy.len > 0 and p.outerResultTy != "pointer" and
            (p.outerResultTy in ["tobject", "tclass", "rootref"] or
             p.syms.isClass(p.outerResultTy)):
          let castTy =
            if p.outerResultTy in ["tobject", "tclass", "rootref"]: "RootRef"
            else: rtlSpelling(p.outerResultTy)
          let icn = newNode(nkCast, a.info)
          icn.add(newIdentNode(castTy, a.info))
          icn.add(ie)
          call = icn
        else:
          call = ie
      else:
        let ie = newNode(nkIndexExpr, a[0].info)
        ie.add(parentCast)
        ie.add(a[0][1])
        call = newNode(nkAsgn, a.info)
        call.add(ie)
        call.add(a[1])
    elif a.kind == nkCall:
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

proc narrowCaseLabel(x: Node): Node =
  ## Pascal case labels carry the selector's ordinal type; nimony's
  ## `high`/`low`/`sizeof` and arithmetic type int64 and a non-literal
  ## label will not coerce to an int32 selector. The corpus's case
  ## selectors are Integer, so narrow non-literal labels to int32.
  if x.kind in {nkInfix, nkPrefix, nkCall}:
    result = newNode(nkCall, x.info)
    result.add(newIdentNode("int32", x.info))
    result.add(x)
  else:
    result = x

proc parseCase*(p: var TParser): Node =
  result = newNodeP(nkCaseStmt, p)
  getTokP(p)                    # skip `case`
  skipCom(p)
  result.add(parseExpr(p))
  p.eat(pxOf)
  skipCom(p)
  while p.tok.xkind != pxEnd and p.tok.xkind != pxParRi and
      p.tok.xkind != pxEof:
    var b: Node
    if p.tok.xkind == pxElse:
      b = newNodeP(nkElse, p)
      getTokP(p)
      skipCom(p)
      # a case-else takes a statement SEQUENCE in Delphi
      let body2 = newNode(nkStmtList, p.tok.info)
      while p.tok.xkind != pxEnd and p.tok.xkind != pxEof:
        body2.add(parseStmt(p))
        p.opt(pxSemiColon)
        skipCom(p)
      b.add(body2)
      result.add(b)
      break
    else:
      b = newNodeP(nkOfBranch, p)
      var noLabels = false
      while p.tok.xkind != pxEof and p.tok.xkind != pxColon:
        # branch labels may carry conditionals:
        # tkInteger, tkClass {$IFDEF FPC} ,tkBool {$ENDIF}:
        if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
          if not declDirective(p):
            noLabels = true
            break
          skipCom(p)
          if p.tok.xkind in {pxEnd, pxParRi, pxElse, pxEof}:
            # the conditional closed the branch; the case loop
            # re-checks (the branch's labels+body were spliced in)
            noLabels = true
            break
        if p.tok.xkind == pxComma:
          getTokP(p)
          skipCom(p)
        if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: continue
        var lab = rangeExpr(p)
        if lab.kind == nkRange:
          for ri in 0 ..< lab.len:
            lab[ri] = narrowCaseLabel(lab[ri])
        else:
          lab = narrowCaseLabel(lab)
        b.add(lab)
        p.opt(pxComma)
        skipCom(p)
      if noLabels:
        continue
      p.eat(pxColon)
    skipCom(p)
    b.add(parseStmt(p))
    p.opt(pxSemiColon)
    skipCom(p)
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
      # `on E: SomeEx do` or `on SomeEx do` (no variable binding)
      var varName: string
      var excTy: Node
      if p.tok.xkind == pxSymbol and p.peekTok.xkind == pxColon:
        varName = p.tok.ident
        getTokP(p)
        p.eat(pxColon)
        excTy = qualifiedIdent(p)
      else:
        inc p.thunkCounter
        varName = "pasExc" & $p.thunkCounter
        excTy = qualifiedIdent(p)
      p.syms.declareName(varName)
      skipCom(p)
      p.eat(pxDo)
      skipCom(p)
      var handler: Node
      if p.tok.xkind == pxElse or p.tok.xkind == pxSemiColon:
        # `on X do { nothing }; else ...` - an empty on-handler body.
        # A bare `;` is the same thing (`on Exception do ;`, a corpus unit)
        # and must NOT be left to the enclosing block: parseStmt would
        # read it as an empty statement, and the `end` that follows then
        # misparses.
        handler = newNode(nkDiscardStmt, p.tok.info)
        handler.add(emptyNode(p.tok.info))
      else:
        handler = parseStmt(p)
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
      # the ErrorCode filter cannot prove the instance class: the
      # pasAs checked-cast (nil on mismatch - Delphi `as` semantics;
      # nimony rejects a bare downcast between ref types)
      let instCast = newNode(nkIndexExpr, info)
      instCast.add(newIdentNode("pasAs", info))
      instCast.add(excTy)
      let instCall = newNode(nkCall, info)
      instCall.add(instCast)
      instCall.add(newIdentNode("pasCurrentExc", info))
      vd.add(instCall)
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
    if (p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "else") or
        p.tok.xkind == pxElse:
      # bare `else` handler for the whole except section
      getTokP(p)
      let b = newNodeP(nkExceptBranch, p)
      b.add(emptyNode(info))
      b.add(emptyNode(info))
      if sawOn:
        # `on X do H else E`: E runs for exceptions no `on` matched.
        # Each `on` branch lowers to its own ErrorCode case; E (v1: a
        # bare re-raise, the corpus's shape) becomes the case's else
        # body. The original else-branch node is demoted to a comment
        # so the renderer does not emit a second handler.
        let body2 = parseStmt(p)
        b.add(body2)
        b.kind = nkCommentStmt
        b.strVal = "# except-else: lowered into the on-branch case's else"
        if body2.kind == nkRaiseStmt:
          # hexer cannot resolve the hidden ErrorCode binding inside a
          # `case` scrutinee when the case's else re-raises; the same
          # shape as an if-chain compiles (probed), so splice there
          for ob in result.sons:
            if ob.kind == nkExceptBranch and ob.len >= 3:
              let caseNode = ob[2]
              if caseNode.kind == nkCaseStmt and caseNode.len >= 2:
                let ofB = caseNode[1]
                if ofB.kind == nkOfBranch and ofB.len >= 2 and
                    ofB[0].kind == nkIdent:
                  let iff = newNode(nkIfStmt, info)
                  let elifb = newNode(nkElifBranch, info)
                  let cond = newNode(nkInfix, info)
                  cond.add(newIdentNode("==", info))
                  cond.add(newIdentNode(ob[1].strVal, info))
                  cond.add(newIdentNode(ofB[0].strVal, info))
                  elifb.add(cond)
                  elifb.add(ofB[1])
                  iff.add(elifb)
                  let elseS = newNode(nkElse, info)
                  let esl = newNode(nkStmtList, info)
                  esl.add(body2)
                  elseS.add(esl)
                  iff.add(elseS)
                  ob[2] = iff
      else:
        let body2 = parseStmt(p)
        b[1] = body2
      result.add(b)
      p.opt(pxSemiColon)
      skipCom(p)
    if not sawOn and result.len == 1:
      # `except <stmts> end` without on/else: plain handler; the
      # body is a statement SEQUENCE
      let b = newNodeP(nkExceptBranch, p)
      b.add(emptyNode(info))
      b.add(emptyNode(info))
      let body2 = newNode(nkStmtList, info)
      while p.tok.xkind notin {pxFinally, pxEnd, pxEof}:
        let s = parseStmt(p)
        if s.kind != nkEmpty: body2.add(s)
        p.opt(pxSemiColon)
        skipCom(p)
      b.add(body2)
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
      p.opt(pxSemiColon)
      skipCom(p)
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
    # nimony var params match exactly (no implicit widening): a
    # non-literal bound of a wider integer type (len(...) is int64)
    # must cast to the declared loop variable's width
    let vt = p.varTypes.getOrDefault(forLoopVarName.toLowerAscii)
    var bounds = @[a, b]
    if vt in ["int8", "int16", "int32", "int64", "uint8", "uint16",
              "uint32", "uint64"]:
      for bi in 0 ..< bounds.len:
        let bound = bounds[bi]
        if bound.kind notin {nkIntLit, nkInt64Lit, nkCharLit} and
            bound.kind != nkCast:
          let cn = newNode(nkCast, bound.info)
          cn.add(newIdentNode(vt, bound.info))
          cn.add(bound)
          bounds[bi] = cn
    let iter = newNode(nkCall, b.info)
    iter.add(newIdentNode(if down: "pforDownto" else: "pforTo", b.info))
    iter.add(newIdentNode(forLoopVarName, result[0].info))
    iter.add(bounds[0])
    iter.add(bounds[1])
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
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
      # a conditional wrapping the `until` condition:
      # {$IFDEF A} until X {$ELSE} until Y {$ENDIF}
      if declDirective(p):
        skipCom(p)
        continue
      break
    let s = parseStmt(p)
    if s.kind != nkEmpty: body.add(s)
    if p.tok.xkind == pxSemiColon:
      getTokP(p)
      skipCom(p)
  # the `until` condition may carry the conditional:
  # {$IFDEF A} until X {$ELSE} until Y {$ENDIF}
  while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
    if not declDirective(p): break
    skipCom(p)
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

proc isPtrCastRecv*(p: var TParser, recv: Node): bool =
  ## the receiver is a class cast of a pointer-typed identifier, in
  ## either the cast[form](x) or the T(x) call spelling
  result = false
  if recv.kind == nkCast and recv.len == 2 and recv[1].kind == nkIdent and
      p.paramTypes.getOrDefault(recv[1].strVal.toLowerAscii) == "pointer":
    return true
  if recv.kind == nkCall and recv.len == 2 and recv[0].kind == nkIdent and
      (p.syms.classes.hasKey(recv[0].strVal.toLowerAscii) or
       recv[0].strVal.toLowerAscii in ["rootref", "tobject"]) and
      recv[1].kind == nkIdent and
      p.paramTypes.getOrDefault(recv[1].strVal.toLowerAscii) == "pointer":
    return true

proc mapStringBuiltins*(p: var TParser, n: Node): Node =
  ## expression-level rewrites of the 1-based string family
  ## (also invoked from mapBuiltinCall for statement-position calls)
  # `TObject(Ptr).Free` / `TObject(Ptr).FreeNotification(x)` on a
  # pointer-typed receiver: nimony rejects a pointer-to-ref cast, so
  # lower to the pasFreeObj / pasFreeNotification shims (v1: no
  # destroy dispatch on notification pointers). The bare member
  # statement arrives as a dot (`TObject(Ptr).Free`), the paren form
  # as a call; handle both.
  var fm = ""
  if n.kind == nkDotExpr and n.len == 2 and n[1].kind == nkIdent:
    fm = n[1].strVal.toLowerAscii
  elif n.kind == nkCall and n.len >= 2 and n[0].kind == nkDotExpr and
      n[0].len == 2 and n[0][1].kind == nkIdent:
    fm = n[0][1].strVal.toLowerAscii
  if fm in ["free", "freenotification", "removefreenotification"]:
    let recv = if n.kind == nkDotExpr: n[0] else: n[0][0]
    if isPtrCastRecv(p, recv):
      var fc = newNode(nkCall, n.info)
      if fm == "free":
        fc.add(newIdentNode("pasFreeObj", n.info))
      else:
        fc.add(newIdentNode("pasFreeNotification", n.info))
      fc.add(recv[1])
      if n.kind == nkCall:
        for i in 1 ..< n.len:
          fc.add(n[i])
      return fc
  if n.kind != nkCall or n.len == 0: return n
  if n[0].kind != nkIdent: return n
  if n.noQualCallee: return n   # inherited calls keep the member spelling
  # class-cast treatment: the corpus's TObject(x)/TComponent(ptr)/...
  # call-form casts across the pointer/object boundary
  let clsCallee = n[0].strVal.toLowerAscii
  if p.syms.isClass(clsCallee) or
      clsCallee in ["tobject", "rootref", "tclass"] or
      ptrKindOf(p, n[0].strVal).startsWith("ref:"):
    if n.len == 2:
      var isPtrArg = false
      if n[1].kind == nkIdent:
        isPtrArg = p.paramTypes.getOrDefault(n[1].strVal.toLowerAscii) == "pointer"
      elif n[1].kind in {nkCall, nkIndexExpr}:
        # the corpus's TObject(inherited X) family rides Pointer results
        isPtrArg = true
      if not isPtrArg:
        isPtrArg = ptrKindOf(p, rhsExprType(p, n[1])) == "pointer"
      if isPtrArg:
        # the pointer->object cast; RootRef must keep its exact
        # capitalization (nimsem's cast check is case sensitive)
        var tgt = ""
        if clsCallee in ["tobject", "tclass", "rootref"]:
          tgt = "RootRef"
        else:
          tgt = n[0].strVal
        if n[1].kind == nkIdent:
          var cn = newNode(nkCast, n.info)
          cn.add(newIdentNode(tgt, n.info))
          cn.add(n[1])
          return cn
        # indexing/inherited-call arguments: keep the cast (the shim's
        # RootRef-returning members and an existing wrap need no wrap)
        if n[1].kind == nkCall and n[1].len > 0 and
            n[1][0].kind == nkIdent and
            n[1][0].strVal.toLowerAscii in ["extract", "remove"]:
          return n
        var cn2 = newNode(nkCast, n.info)
        cn2.add(newIdentNode(tgt, n.info))
        cn2.add(n[1])
        return cn2
    return n
  case n[0].strVal.toLowerAscii
  of "pointer":
    # the `Pointer(x)` reinterpretation: Delphi allows it when Integer
    # and Pointer are the same size (the Win32 origin), and FPC still
    # accepts it on x86-64 with a "Conversion between ordinals and
    # pointers is not portable" warning. nimsem rejects the CONVERSION
    # on every target (probed: kind-based, also under --cpu:i386
    # --bits:32), so an ordinal argument must become the explicit bit
    # cast - which sign-extends a negative int32 exactly like FPC's
    # `Pointer(-1)` -> $FFFFFFFFFFFFFFFF
    if n.len == 2:
      # `Pointer(AnsiString)`: Delphi yields the address of the string
      # data. nimony has neither a `pointer(string)` conversion nor a
      # string<->pointer cast, so route it through the shim (which handles
      # toCString's needs-var parameter).
      var sot = rhsExprType(p, n[1]).toLowerAscii
      if sot.len == 0 and n[1].kind == nkIdent:
        sot = p.paramTypes.getOrDefault(
            n[1].strVal.toLowerAscii).toLowerAscii
      if sot in ["string", "ansistring", "widestring", "shortstring",
                 "utf8string"]:
        var sc = newNode(nkCall, n.info)
        sc.add(newIdentNode("pasPAnsiChar", n.info))
        sc.add(n[1])
        return sc
      # a numeric operand (`Pointer(1)`, `Pointer(Integer(x))`,
      # `Pointer(NativeInt(x))`) or a ref operand crosses domains: cast.
      # `Pointer(somePtr)` and an unknown operand stay a conversion.
      let pd = operandDomain(p, n[1])
      if pd == "num" or pd == "ref":
        var cn = newNode(nkCast, n.info)
        cn.add(newIdentNode("pointer", n.info))
        cn.add(n[1])
        return cn
    return n
  of "integer":
    # `Integer(x)`: a reinterpretation only across domains - the low 32
    # bits of a pointer (Win32's own view of the same value), on
    # inherited-call results (which ride Pointer results) and on
    # pointer/object-typed identifiers alike. `Integer(someInt64)` is a
    # plain numeric conversion and stays one.
    if n.len == 2:
      if n[1].kind in {nkCall, nkIndexExpr} or
          operandDomain(p, n[1]) in ["ptr", "ref"]:
        var cn = newNode(nkCast, n.info)
        cn.add(newIdentNode("int32", n.info))
        cn.add(n[1])
        return cn
    return n
  of "nativeint", "ptrint":
    # `NativeInt(p)`: the word-size-correct spelling of the same
    # reinterpretation (`Integer(p)` is the Win32-only form above).
    # Pointer-sized in nimony == `int`, whose width IS the target word
    # size, so no 32-bit assumption remains
    if n.len == 2:
      if n[1].kind in {nkCall, nkIndexExpr} or
          operandDomain(p, n[1]) in ["ptr", "ref"]:
        var cn = newNode(nkCast, n.info)
        cn.add(newIdentNode("int", n.info))
        cn.add(n[1])
        return cn
    return n
  of "nativeuint", "ptruint":
    # the unsigned twin of `NativeInt(p)`
    if n.len == 2:
      if n[1].kind in {nkCall, nkIndexExpr} or
          operandDomain(p, n[1]) in ["ptr", "ref"]:
        var cn = newNode(nkCast, n.info)
        cn.add(newIdentNode("uint", n.info))
        cn.add(n[1])
        return cn
    return n
  case n[0].strVal.toLowerAscii
  of "length":
    # Pascal Length() returns Integer (int32); nimony's len returns
    # int64. Route through the int32-returning shim (a local variable
    # named Len must not hijack the builtin's spelling)
    if n.len == 2:
      var lc = newNode(nkCall, n.info)
      lc.add(newIdentNode("Len32", n.info))
      lc.add(n[1])
      return lc
    return n
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
      # plain proc vars: nimony has no assigned() builtin
      if n[1].kind == nkIdent and
          p.procVarTypes.hasKey(n[1].strVal.toLowerAscii):
        var ne = newNode(nkInfix, n.info)
        ne.add(newIdentNode("!=", n.info))
        ne.add(n[1])
        ne.add(newNode(nkNilLit, n.info))
        return ne
      # object-typed refs (locals, params, class fields): non-nil test
      if n[1].kind in {nkIdent, nkDotExpr, nkDeref}:
        var ne = newNode(nkInfix, n.info)
        ne.add(newIdentNode("!=", n.info))
        ne.add(n[1])
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
      # a reserved shim name: the corpus's own `Find` methods must not
      # pollute canon for nimony's system `find`
      call.add(newIdentNode("pasFind", n.info))
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
      call.add(newIdentNode("pasFind", n.info))
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
    if result.len == 0:
      result = p.varRawTypes.getOrDefault(n.strVal.toLowerAscii)
    if result.len == 0 and p.selfClass.len > 0:
      result = p.fieldTypes.getOrDefault(
          p.selfClass.toLowerAscii & "." & n.strVal.toLowerAscii)
  of nkDotExpr:
    if n.len == 2 and n[1].kind == nkIdent:
      var owner = ""
      if n[0].kind == nkIdent:
        let rv = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
        if rv.startsWith("record:"): owner = rv[7 .. ^1]
        elif rv.startsWith("class:"): owner = rv[6 .. ^1]
        elif rv.startsWith("ptr:"):
          owner = rv[4 .. ^1]
          if p.pointerAliases.hasKey(owner.toLowerAscii):
            owner = p.pointerAliases.getOrDefault(owner.toLowerAscii)
        else:
          owner = n[0].strVal
          if p.pointerAliases.hasKey(owner.toLowerAscii):
            owner = p.pointerAliases.getOrDefault(owner.toLowerAscii)
        if owner.len == 0 and n[0].strVal.toLowerAscii == "self" and
            p.selfClass.len > 0:
          owner = p.selfClass
      if owner.len == 0:
        owner = p.withExprClass(n[0])
      result = ""
      if owner.len > 0:
        result = p.fieldTypes.getOrDefault(
            owner.toLowerAscii & "." & n[1].strVal.toLowerAscii)
      if result.len == 0:
        let wc = p.withExprClass(n[0])
        if wc.len > 0:
          result = p.fieldTypes.getOrDefault(
              wc.toLowerAscii & "." & n[1].strVal.toLowerAscii)
    else: result = ""
  of nkCall:
    if n.len > 0 and n[0].kind == nkIdent:
      let ck = n[0].strVal.toLowerAscii
      if ck in ["pointer", "addr"]:
        # `Pointer(x)` / `addr(x)`: a raw address (the cstring rule
        # needs to see it to insert the cast at a cstring target)
        result = "pointer"
      elif p.pointerAliases.hasKey(ck):
        result = n[0].strVal
      else:
        result = p.routineReturns.getOrDefault(ck,
            p.procVarReturns.getOrDefault(ck, ""))
    elif n.len > 0 and n[0].kind == nkDotExpr and n[0].len == 2 and
        n[0][1].kind == nkIdent:
      let rk = n[0][1].strVal.toLowerAscii
      let ck = if n[0][0].kind == nkIdent: n[0][0].strVal.toLowerAscii else: ""
      result = p.routineReturns.getOrDefault(ck & "." & rk,
          p.routineReturns.getOrDefault(rk, ""))
    else: result = ""
  of nkAddr:
    # `addr(x)` / `@x`: a raw address, same as `Pointer(x)`
    result = "pointer"
  of nkPar:
    if n.len > 0:
      result = rhsExprType(p, n[0])
    else: result = ""
  of nkInfix:
    if n.len == 3:
      let lt = rhsExprType(p, n[1])
      let rt = rhsExprType(p, n[2])
      if lt.startsWith("record:"): result = lt
      elif rt.startsWith("record:"): result = rt
      elif lt in ["float32", "float64"] or rt in ["float32", "float64"]:
        result = "float64"
      elif n[0].kind == nkIdent and n[0].strVal in
          ["and", "or", "xor", "shl", "shr"] and
          (lt in ["int8", "uint8", "int16", "uint16", "int32", "uint32"] or
           rt in ["int8", "uint8", "int16", "uint16", "int32", "uint32"]):
        # bit operators keep the narrow operand's own width (Delphi
        # semantics; the other side may be a call of unknown type)
        result = if lt in ["int8", "uint8", "int16", "uint16", "int32",
                           "uint32"]: lt else: rt
      elif lt.len > 0 and rt.len > 0: result = "int32"
      else: result = ""
    else: result = ""
  else: result = ""

proc ptrKindOf(p: TParser, t: string): string =
  ## classify a registered type spelling into "ptr:<alias>", "pointer",
  ## "ref:<spelling>" or "" (not a pointer/reference type)
  if t.len == 0: return ""
  let tl = t.toLowerAscii
  if t.startsWith("ptr:"):
    let alias = t[4 .. ^1]
    if alias.toLowerAscii == "pointer": return "pointer"
    return "ptr:" & alias
  if tl == "pointer": return "pointer"
  if tl == "cstring": return "cstring"
  if p.pointerAliases.hasKey(tl): return "ptr:" & t
  if t.startsWith("class:"): return "ref:" & t[6 .. ^1]
  if p.typeAliasTargets.hasKey(tl):
    let tk = ptrKindOf(p, p.typeAliasTargets.getOrDefault(tl))
    if tk.startsWith("ref:"): return "ref:" & t
    if tk.startsWith("ptr:"): return "ptr:" & t
    if tk == "pointer": return "pointer"
  if p.syms.isClass(tl): return "ref:" & t
  case tl
  of "rootref", "tobject", "tclass": return "ref:" & t
  else: return ""

proc placeExprType(p: var TParser, n: Node): string =
  ## declared type of an assignment target / place expression
  result = ""
  case n.kind
  of nkIdent:
    let k = n.strVal.toLowerAscii
    result = p.varTypes.getOrDefault(k)
    if result.len == 0: result = p.paramTypes.getOrDefault(k)
    if result.len == 0:
      result = p.varRawTypes.getOrDefault(k)
    if result.len == 0 and p.selfClass.len > 0:
      result = p.fieldTypes.getOrDefault(
          p.selfClass.toLowerAscii & "." & k)
  of nkDotExpr:
    if n.len == 2 and n[1].kind == nkIdent:
      var base = ""
      let bt = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
      if bt.startsWith("ptr:"): base = bt[4 .. ^1]
      elif bt.startsWith("record:"): base = bt[7 .. ^1]
      elif bt.startsWith("class:"): base = bt[6 .. ^1]
      elif n[0].kind == nkIdent:
        base = p.varRawTypes.getOrDefault(n[0].strVal.toLowerAscii)
      if base.len == 0 and n[0].kind == nkIdent and
          n[0].strVal.toLowerAscii == "self" and p.selfClass.len > 0:
        base = p.selfClass
      if base.len == 0:
        base = p.withExprClass(n[0])
      if p.pointerAliases.hasKey(base.toLowerAscii):
        base = p.pointerAliases.getOrDefault(base.toLowerAscii)
      if base.len > 0:
        result = p.fieldTypes.getOrDefault(
            base.toLowerAscii & "." & n[1].strVal.toLowerAscii)
  else: result = ""

proc convPtrNode(target: string; e: Node): Node =
  ## T(pointerExpr): a Nim conversion from Pointer to a typed pointer
  result = newNode(nkCall, e.info)
  result.add(newIdentNode(target, e.info))
  result.add(e)

proc hardCastNode(target: string; e: Node): Node =
  result = newNode(nkCast, e.info)
  result.add(newIdentNode(target, e.info))
  result.add(e)

proc coercePtrAsgns*(p: var TParser, n: Node) =
  ## Pascal lets Pointer, typed pointers and object refs assign freely;
  ## nimony needs an explicit conversion (Pointer->ptr) or bit cast
  ## (between pointer/ref domains)
  if n.kind == nkAsgn and n.len == 2:
    # a Delphi variable typecast on the left (TValueType(Value) := ...):
    # it is an assignment-compatible lvalue - assign to the operand
    if n[0].kind == nkCast and n[0].len == 2:
      n[0] = n[0][1]
    elif n[0].kind == nkCall and n[0].len == 2 and n[0][0].kind == nkIdent:
      n[0] = n[0][1]
    n[1] = lowerTMethodField(p, n[1])
    let lt = placeExprType(p, n[0])
    let rt = rhsExprType(p, n[1])
    let lk = ptrKindOf(p, lt)
    let rk = ptrKindOf(p, rt)
    if lk.startsWith("ptr:"):
      let alias = lk[4 .. ^1]
      if rk == "pointer":
        n[1] = convPtrNode(alias, n[1])
        return
      elif rk == "cstring":
        n[1] = hardCastNode(alias, n[1])
        return
      elif rk.startsWith("ptr:") and rk != lk:
        n[1] = hardCastNode(alias, n[1])
        return
    elif lk == "cstring":
      # PAnsiChar := Pointer / typed pointer / `addr` result: nimony has
      # no coercion, so take the raw cast. A string source already
      # arrives as cstring (literal / toCString) and is left alone.
      if rk.len > 0 and rk != "cstring":
        n[1] = hardCastNode("cstring", n[1])
        return
    elif lk == "pointer" and rk == "cstring":
      n[1] = hardCastNode("pointer", n[1])
      return
    elif lk == "pointer" and rk.startsWith("ptr:"):
      n[1] = convPtrNode("pointer", n[1])
      return
    elif lk == "pointer" and rk.startsWith("ref:"):
      n[1] = hardCastNode("pointer", n[1])
      return
    elif lk.startsWith("ref:") and rk == "pointer":
      n[1] = hardCastNode(lk[4 .. ^1], n[1])
      return
  for s in n.sons:
    coercePtrAsgns(p, s)

proc coercePtrCasts*(p: var TParser, n: Node): Node =
  ## an explicit Pascal pointer cast `PItems(x)` between different
  ## pointee types is a bit cast in Nim, not a conversion
  result = n
  if n.kind == nkCall and n.len == 2 and n[0].kind == nkIdent and
      p.pointerAliases.hasKey(n[0].strVal.toLowerAscii):
    let target = n[0].strVal
    let rk = ptrKindOf(p, rhsExprType(p, n[1]))
    if rk.startsWith("ptr:") and rk != "ptr:" & target:
      result = hardCastNode(target, n[1])
      return
  for i in 0 ..< n.sons.len:
    if n.sons[i].kind != nkEmpty:
      n.sons[i] = coercePtrCasts(p, n.sons[i])

proc calleeProcParams(p: TParser; callee: Node): Node =
  ## formal params of the proc type a call target denotes (`visit.call`,
  ## a proc-typed local/param), or nil when unknown
  var sp = ""
  if callee.kind == nkIdent:
    sp = p.varRawTypes.getOrDefault(callee.strVal.toLowerAscii)
    if sp.len == 0:
      sp = p.varTypes.getOrDefault(callee.strVal.toLowerAscii)
  elif callee.kind == nkDotExpr and callee.len == 2 and callee[1].kind == nkIdent:
    if callee[0].kind == nkIdent:
      let oty = p.varTypes.getOrDefault(callee[0].strVal.toLowerAscii)
      if oty.startsWith("record:"):
        sp = p.fieldTypes.getOrDefault(oty[7 .. ^1].toLowerAscii & "." &
            callee[1].strVal.toLowerAscii)
  if sp.len == 0: return emptyNode(callee.info)
  let sl = sp.toLowerAscii
  if p.methodPtrTypes.hasKey(sl): return p.methodPtrTypes.getOrDefault(sl)
  if p.procTyParams.hasKey(sl): return p.procTyParams.getOrDefault(sl)
  result = emptyNode(callee.info)

proc addrLvalueCast(tgt, inner: Node): Node =
  ## cast[ptr T](addr x)[] - an lvalue reinterpreting x as T; a Delphi
  ## variable typecast passed to a `var` parameter needs an lvalue, and
  ## a plain cast is not passable by var
  var ad = newNode(nkAddr, inner.info)
  ad.add(inner)
  var pt = newNode(nkPtrTy, inner.info)
  pt.add(tgt)
  var cn = newNode(nkCast, inner.info)
  cn.add(pt)
  cn.add(ad)
  result = newNode(nkDeref, inner.info)
  result.add(cn)

proc coercePtrArgs*(p: var TParser, n: Node) =
  ## Pascal lets Pointer, typed pointers and object refs pass at the
  ## argument boundary; nimony needs an explicit conversion/cast
  if n.kind == nkCall and n.len > 1 and n[0].kind in {nkIdent, nkDotExpr}:
    let fp = calleeProcParams(p, n[0])
    if fp.kind != nkEmpty:
      for i in 1 ..< n.len:
        if i < fp.len and fp[i].kind == nkIdentDefs and
            fp[i][fp[i].len - 2].kind == nkVarTy and n[i].len == 2 and
            (n[i].kind == nkCast or
             (n[i].kind == nkCall and n[i][0].kind == nkIdent)) and
            n[i][1].kind in {nkIdent, nkDotExpr, nkIndexExpr, nkDeref}:
          n[i] = addrLvalueCast(n[i][0], n[i][1])
  if n.kind == nkCall and n.len > 1:
    var argTypes: seq[string] = @[]
    if n[0].kind == nkIdent:
      argTypes = p.routineParams.getOrDefault(n[0].strVal.toLowerAscii)
      if argTypes.len == 0 and p.selfClass.len > 0:
        argTypes = p.routineParams.getOrDefault(
            p.selfClass.toLowerAscii & "." & n[0].strVal.toLowerAscii)
    elif n[0].kind == nkDotExpr and n[0].len == 2 and n[0][1].kind == nkIdent:
      argTypes = p.routineParams.getOrDefault(n[0][1].strVal.toLowerAscii)
    if argTypes.len > 0:
      for ai in 1 ..< n.len:
        if ai - 1 < argTypes.len:
          let pk = ptrKindOf(p, argTypes[ai - 1])
          let ak = ptrKindOf(p, rhsExprType(p, n[ai]))
          if pk.startsWith("ptr:"):
            if ak == "pointer":
              n[ai] = convPtrNode(pk[4 .. ^1], n[ai])
            elif ak.startsWith("ptr:") and ak != pk:
              n[ai] = hardCastNode(pk[4 .. ^1], n[ai])
          elif pk == "pointer" and ak.startsWith("ref:"):
            n[ai] = hardCastNode("pointer", n[ai])
          elif pk.startsWith("ref:") and ak == "pointer":
            n[ai] = hardCastNode(pk[4 .. ^1], n[ai])
  if n.kind == nkCall and n.len == 3 and n[0].kind == nkIdent and
      n[0].strVal.toLowerAscii in ["incl", "include"] and n[1].kind == nkIdent:
    let et = p.setElemTexts.getOrDefault(n[1].strVal.toLowerAscii)
    if et.len > 0 and n[2].kind != nkCast:
      var cn = newNode(nkCast, n[2].info)
      cn.add(newIdentNode(et, n[2].info))
      cn.add(n[2])
      n[2] = cn
  if n.kind == nkInfix and n.len == 3 and n[0].kind == nkIdent and
      n[0].strVal.toLowerAscii in ["in", "notin"] and n[2].kind == nkIdent:
    let et = p.setElemTexts.getOrDefault(n[2].strVal.toLowerAscii)
    if et.len > 0 and n[1].kind != nkCast:
      var cn = newNode(nkCast, n[1].info)
      cn.add(newIdentNode(et, n[1].info))
      cn.add(n[1])
      n[1] = cn
  for s in n.sons:
    coercePtrArgs(p, s)
# --- Variant construction at Variant-typed sites ---------------------------
# nimony applies no converters at all (measured: `f(3)` fails even with a
# matching converter in scope), so every place Pascal relies on an implicit
# Variant conversion must be rewritten into an explicit construction:
#   toVariant(typed value)   - Integer/Int64/Double/Single/string/bool/char
#   pasVarLit(integer literal) - Delphi types a literal by its value
#   pasVarCurrF(currency)    - Currency is float64 here but varCurrency there
# and arithmetic/comparison on Variant operands routes to the named pasVar*
# procs, which also marks the call as {.raises.} for nimony.

proc variantOpName(op: string): string =
  ## the shim proc for a Pascal operator over Variants ("" = not Variant)
  case op
  of "+": result = "pasVarAdd"
  of "-": result = "pasVarSub"
  of "*": result = "pasVarMul"
  of "/": result = "pasVarDiv"
  of "div": result = "pasVarIDiv"
  of "mod": result = "pasVarMod"
  of "==": result = "pasVarEq"
  of "!=": result = "pasVarNe"
  of "<": result = "pasVarLt"
  of "<=": result = "pasVarLe"
  of ">": result = "pasVarGt"
  of ">=": result = "pasVarGe"
  else: result = ""

proc variantShimArgPositions(name: string): seq[int] =
  ## 1-based argument positions the shim declares as Variant
  case name
  of "vartype", "vartostr", "vartowidestr", "varisnull", "varisempty",
     "varisclear", "varisarray", "varastype", "vararrayhighbound",
     "vararraylowbound", "vararraydimcount", "vararrayget":
    result = @[1]
  of "vartostrdef": result = @[1, 2]
  of "vararrayput": result = @[2]
  else: result = @[]

proc isVariantExpr(p: var TParser; e: Node): bool =
  ## already a Variant value? (so it must not be wrapped twice)
  case e.kind
  of nkIdent:
    result = e.strVal.toLowerAscii in ["null", "unassigned", "emptyparam"] or
             p.varTypes.getOrDefault(e.strVal.toLowerAscii).toLowerAscii ==
               "variant"
  of nkCall:
    result = e.len > 0 and e[0].kind == nkIdent and
             e[0].strVal.toLowerAscii in ["tovariant", "pasvarlit",
               "pasvarcurr", "pasvarcurrf", "pasvaradd", "pasvarsub",
               "pasvarmul", "pasvardiv", "pasvaridiv", "pasvarmod",
               "pasvarneg", "varastype", "vararrayget", "vararrayof",
               "vararraycreate"]
  of nkPar:
    result = e.len > 0 and isVariantExpr(p, e[0])
  of nkIndexExpr:
    # indexing a Variant array yields a Variant
    result = e.len > 1 and e[0].kind == nkIdent and
             p.varTypes.getOrDefault(e[0].strVal.toLowerAscii).toLowerAscii ==
               "variant"
  of nkDeref:
    # dereferencing a `ptr Variant` (Delphi `v^`) yields a Variant
    result = false
    if e.len > 0:
      let pk = ptrKindOf(p, placeExprType(p, e[0]))
      if pk.startsWith("ptr:"):
        let al = pk[4 .. ^1].toLowerAscii
        result = al == "variant" or
            p.pointerAliases.getOrDefault(al).toLowerAscii == "variant"
  else: result = false

proc variantCoerce(p: var TParser; e: Node): Node =
  ## make `e` a Variant value *explicitly* (no-op if it already is one)
  if isVariantExpr(p, e): return e
  if e.kind in {nkIntLit, nkInt64Lit}:
    result = newNode(nkCall, e.info)
    result.add(newIdentNode("pasVarLit", e.info))
    result.add(e)
    return
  if e.kind == nkPrefix and e.len == 2 and e[0].kind == nkIdent and
      e[0].strVal == "-" and e[1].kind in {nkIntLit, nkInt64Lit}:
    # a negative literal is a prefix node; fold the sign so pasVarLit can
    # apply Delphi's by-value typing (measured: -3 -> varShortInt(0010),
    # -70000 -> varInteger(0003))
    let lit = newNode(nkIntLit, e.info)
    lit.intVal = -e[1].intVal
    result = newNode(nkCall, e.info)
    result.add(newIdentNode("pasVarLit", e.info))
    result.add(lit)
    return
  if e.kind == nkIdent:
    # the Pascal type is not the nimony type: Currency and TDateTime are
    # float64 aliases, WideString is a string, and each carries its own tag
    let ctor = case p.varRawTypes.getOrDefault(e.strVal.toLowerAscii)
      of "currency", "comp": "pasVarCurrF"
      of "tdatetime": "pasVarDateF"
      of "widestring": "pasVarWStr"
      else: ""
    if ctor.len > 0:
      result = newNode(nkCall, e.info)
      result.add(newIdentNode(ctor, e.info))
      result.add(e)
      return
  result = newNode(nkCall, e.info)
  result.add(newIdentNode("toVariant", e.info))
  result.add(e)

proc variantInt32Arg(p: var TParser; e: Node): Node =
  ## the shim's int32 array parameters (VarArrayCreate/VarArrayGet bounds):
  ## a literal array is `array[0..n, int64]` in nimony
  if e.kind in {nkIntLit, nkInt64Lit}:
    result = newNode(nkCall, e.info)
    result.add(newIdentNode("int32", e.info))
    result.add(e)
  else:
    result = e

proc variantCoerceTyped(p: var TParser; e: Node): Node =
  ## like variantCoerce, but a literal is *not* typed by its value: an
  ## assignment to an element of a variant array converts to the array's
  ## element type instead (measured: `a[0] := 10` on a varInteger array gives
  ## varInteger(0003), where a plain `v := 10` gives the literal policy's
  ## varByte(0011))
  if isVariantExpr(p, e): return e
  var src = e
  if src.kind in {nkIntLit, nkInt64Lit}:
    # an untyped literal is int64 in nimony: the element conversions all
    # start from Integer (measured 0003 for a varInteger array)
    let c = newNode(nkCall, src.info)
    c.add(newIdentNode("int32", src.info))
    c.add(src)
    src = c
  result = newNode(nkCall, e.info)
  result.add(newIdentNode("toVariant", e.info))
  result.add(src)

proc variantArrayElem(p: var TParser; e: Node): Node =
  ## one element of `VarArrayOf([...])`, whose parameter is an open
  ## `array of Variant`. Measured (test/variant/varray.pas, both oracles
  ## agreeing): a *string-typed* element becomes varOleStr (0008), where the
  ## same value assigned to a Variant gives varString (0100).
  if isVariantExpr(p, e): return e
  var isStr = e.kind in {nkStrLit, nkCharLit}
  if e.kind == nkIdent:
    if p.varRawTypes.getOrDefault(e.strVal.toLowerAscii) in
        ["string", "ansistring", "widestring", "unicodestring",
         "shortstring", "char", "ansichar", "widechar", "pchar"]:
      isStr = true
  if isStr:
    result = newNode(nkCall, e.info)
    result.add(newIdentNode("pasVarWStr", e.info))
    result.add(e)
    return
  result = variantCoerce(p, e)

proc mapBuiltinCall*(p: var TParser; n: Node): Node =
  ## rewrite builtins that need argument changes:
  ## write(x) -> write(stdout, x); writeln(...) -> echo(...);
  ## Pos(sub, s) -> find(s, sub); Copy(s, a, b) -> substr(s, a, b);
  ## Exit / Exit(x) -> return / return x
  let m = mapStringBuiltins(p, n)
  if m != n: return m
  if n.kind != nkCall or n.len == 0: return n
  if n[0].kind != nkIdent: return n
  if n.noQualCallee: return n   # inherited calls keep the member spelling
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
    # a with-scope already closed: the lowered body still refers to the
    # hidden temp, so resolve it from the persistent map
    let wo = p.withTempOwners.getOrDefault(e.strVal.toLowerAscii)
    if wo.len > 0: return wo
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
    let ct = p.constTypes.getOrDefault(e.strVal.toLowerAscii)
    if ct.len > 0:
      # a typed const's Pascal type: resolve record / class / ptr-alias /
      # array-alias exactly like a var's raw type, so `with C do`
      # classifies its body instead of leaving the fields unqualified
      let ctk = ct.toLowerAscii
      if p.recordTypes.hasKey(ctk): return ct
      if p.syms.isClass(ctk): return p.syms.classSpelling(ct)
      let pel = p.pointerAliases.getOrDefault(ctk, "")
      if pel.len > 0:
        var sp = p.syms.classSpelling(pel)
        if sp.len == 0: sp = p.arrayAliases.getOrDefault(pel.toLowerAscii, "")
        return (if sp.len > 0: sp else: pel)
      let ael = p.arrayAliases.getOrDefault(ctk, "")
      if ael.len > 0: return ael
    # a pointer-typed base (`n: PNode`) names the pointee record/class
    # through its alias; without this `with n.Items[x] do` cannot
    # classify the body and leaves every field unqualified
    if vt.startsWith("ptr:") or vt.len == 0:
      var alias = if vt.startsWith("ptr:"): vt[4 .. ^1] else: ""
      if alias.len == 0:
        alias = p.varRawTypes.getOrDefault(e.strVal.toLowerAscii, "")
      if alias.len == 0:
        alias = p.paramTypes.getOrDefault(e.strVal.toLowerAscii, "")
      if alias.len > 0:
        let el = p.pointerAliases.getOrDefault(alias.toLowerAscii, "")
        if el.len > 0:
          var sp = p.syms.classSpelling(el)
          if sp.len == 0:
            sp = p.arrayAliases.getOrDefault(el.toLowerAscii, "")
          return (if sp.len > 0: sp else: el)
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
  elif e.kind == nkCall and e.len == 2 and e[0].kind == nkIdent:
    # a type-cast `THashedStringList(expr)`: the cast's class
    return p.syms.classSpelling(e[0].strVal)
  elif e.kind == nkCall and e.len >= 1 and e[0].kind == nkDotExpr and
      e[0][0].kind == nkIdent and e[0][1].kind == nkIdent:
    # `HtScriptGlobal.RegisterClass(...)`: the routine's return type
    let rk = e[0][1].strVal.toLowerAscii
    let ck = e[0][0].strVal.toLowerAscii
    return p.routineReturns.getOrDefault(ck & "." & rk,
        p.routineReturns.getOrDefault(rk, ""))
  elif e.kind == nkCall and e.len >= 1 and e[0].kind == nkIdent:
    # a bare routine call: the routine's return type
    return p.routineReturns.getOrDefault(e[0].strVal.toLowerAscii, "")
  elif e.kind == nkDotExpr and e.len == 2:
    let baseCls = p.withExprClass(e[0])
    if baseCls.len > 0 and e[1].kind == nkIdent:
      return p.classFieldTypes.getOrDefault(
          baseCls.toLowerAscii & "." & e[1].strVal.toLowerAscii)
  elif e.kind in {nkIndexExpr, nkBracket} and e.len == 2:
    # `Buckets[i]` / `Slice.Fields[i]`: the element class of an
    # array-typed base (v1: only class/record-element arrays resolve)
    let r = p.withExprClass(e[0])
    if r.len > 0: return r
    if e[0].kind == nkIdent:
      return p.arrayVarElems.getOrDefault(e[0].strVal.toLowerAscii, "")
  elif e.kind == nkDeref and e.len == 1:
    # `PtrExpr^`: the element class/record of a pointer-typed base
    if e[0].kind == nkCall and e[0].len == 2 and
        e[0][0].kind == nkIdent:
      # a pointer-cast `PWideStrData(Data)^`
      return p.pointerAliases.getOrDefault(
          e[0][0].strVal.toLowerAscii, "")
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

proc withBaseText(p: TParser; e: Node): string =
  ## source-text rendering of a simple with-base (idents, dots,
  ## indexes, literals) for record-with qualification. Identifiers are
  ## CANONICALISED to their emitted spelling: rebuilding the raw source
  ## text used `i` where the emitter declared `I`, and Nim treats the
  ## first letter as case-sensitive (`undeclared identifier: i`).
  result = ""
  case e.kind
  of nkIdent: result = p.syms.canonical(e.strVal)
  of nkDotExpr:
    if e.len == 2 and e[0].kind in {nkIdent, nkDotExpr, nkBracket} and
        e[1].kind == nkIdent:
      result = withBaseText(p, e[0]) & "." &
               p.syms.canonicalMember(e[1].strVal)
  of nkIndexExpr, nkBracket:
    if e.len >= 2:
      result = withBaseText(p, e[0]) & "[" &
               withBaseText(p, e[e.len - 1]) & "]"
  of nkDeref:
    # the explicit deref the index builder inserts for a
    # pointer-to-array base (`r.Items[][1]`)
    if e.len == 1:
      result = withBaseText(p, e[0]) & "[]"
  of nkInfix:
    # an index like `cCenter - 1`: without this the index rendered as
    # an EMPTY bracket, producing an invalid `Left.Items[][]`
    if e.len == 3:
      result = withBaseText(p, e[1]) & e[0].strVal & withBaseText(p, e[2])
  of nkPrefix:
    if e.len == 2:
      result = withBaseText(p, e[0]) & withBaseText(p, e[1])
  of nkIntLit: result = $e.intVal
  else: result = ""

proc withBaseWritable(p: var TParser; e: Node): bool =
  ## is a with base a WRITABLE designator? Only then may the lowering
  ## bind an address temp. A const/immutable base must use a value copy:
  ## valid Pascal never writes through one, and nimony addr of
  ## const/immutable storage either fails late (scalar const, in C
  ## codegen) or silently violates immutability (writing through the
  ## address of a const record segfaults at run, a let record is mutated).
  case e.kind
  of nkIdent:
    let k = e.strVal.toLowerAscii
    if k in ["nil", "true", "false"]: return false
    if p.constTypes.hasKey(k) or p.constParams.hasKey(k): return false
    result = true
  of nkDotExpr:
    result = false
    if e.len == 2 and e[1].kind == nkIdent:
      var ro = false
      if e[0].kind == nkIdent:
        let cls = p.chainRecordSpelling(e[0]).toLowerAscii
        for pr in p.props:
          if pr.cls.toLowerAscii == cls and
              pr.name.toLowerAscii == e[1].strVal.toLowerAscii:
            ro = pr.writeId.len == 0
            break
      if not ro: result = withBaseWritable(p, e[0])
  of nkIndexExpr, nkBracket:
    result = e.len >= 2 and withBaseWritable(p, e[0])
  of nkDeref:
    result = e.len == 1 and withBaseWritable(p, e[0])
  else:
    result = false

proc withTempBase(p: var TParser; i: int; info: TLineInfo): Node =
  ## the expression a with-body qualifies against; an address temp (or a
  ## captured pointer) is deref-qualified so field writes go through to
  ## the original designator, and a captured index selects the element
  result = newIdentNode(p.withTemps[i], info)
  if i < p.withPtrs.len and p.withPtrs[i]:
    let d = newNode(nkDeref, info)
    d.add(result)
    result = d
  if i < p.withIdxNames.len and p.withIdxNames[i].len > 0:
    let at = newNode(nkIndexExpr, info)
    at.add(result)
    at.add(newIdentNode(p.withIdxNames[i], info))
    result = at


proc setElemTypeText(p: var TParser, en: Node): string =
  ## the nimony type spelling of a set's element, for the narrowing cast
  ## nimony's `incl` requires (`incl(s, cast[range[..]](i))`)
  result = ""
  if en.kind == nkRangeTy and en.len > 0:
    result = setElemTypeText(p, en[0])
  elif en.kind == nkRange and en.len == 2:
    result = "range[" & withBaseText(p, en[0]) & ".." &
             withBaseText(p, en[1]) & "]"
  elif en.kind == nkIdent:
    result = p.syms.canonical(en.strVal)

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
        return newDotP(withTempBase(p, i, n.info), n.strVal)
      let ci = p.syms.classes.getOrDefault(c)
      if ci.spelling.len == 0: break
      if ci.fieldSet.hasKey(k) or ci.routineSet.hasKey(k) or
          ci.ctorSet.hasKey(k):
        return newDotP(withTempBase(p, i, n.info), n.strVal)
      # properties of the class qualify too
      for pr in p.props:
        if pr.params == nil and pr.cls.toLowerAscii == c and
            pr.name.toLowerAscii == k:
          return newDotP(withTempBase(p, i, n.info), n.strVal)
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
    # the with-expression's implicit-self members must qualify here:
    # the lowered temp's init lands in a var section the general
    # self-qualify walk stops at (nkVarSection/nkIdentDefs are skipped
    # to protect declaration names)
    var e = parseExpr(p)
    var ws: seq[string] = @[]
    # the with-expression parses before the def-level self-qualify
    # pass runs, so bring the method's class in here
    let savedQual = p.qualClass
    if p.qualClass.len == 0: p.qualClass = p.classOfProc
    e = selfQualifyInPlace(p, e, ws)
    p.qualClass = savedQual
    var cls = p.withExprClass(e)
    if cls.len == 0:
      # unresolvable class (a call on a unit-level variable of an
      # un-absorbed unit): lower to a hidden temp with an inferred
      # type; the body's members stay unqualified (v1)
      cls = "" 
    inc p.withCounter
    let temp = "pasW" & $p.withCounter
    let info = p.tok.info
    # Every with must evaluate its base exactly ONCE, and writes must
    # reach the original designator (FPC binds a hidden address for an
    # lvalue base and a value copy for an rvalue/const base).
    #
    # The temp KIND is chosen from the base shape:
    #   * record designator addr can address directly -> address temp
    #     var w = addr(base); the body qualifies w[].field;
    #   * record reached through a raw pointer (P^, or a pointer-array
    #     element X[][i]) -> capture the POINTER (and the index) in temps:
    #     nimony rejects addr of an explicit-deref operand (addr(P[]) and
    #     addr(X[][1]) are errors), while p[][i].field writes through.
    #     Capturing the index too stops a body that reassigns it from
    #     retargeting the qualifier;
    #   * class/ref -> typed temp (a ref copy: already one evaluation);
    #   * const / rvalue -> value-copy temp (one evaluation; valid Pascal
    #     never writes through an immutable base).
    # The old text-qualification re-evaluated a record base at every field
    # access, so a base whose sub-state changed in the body wrote elsewhere.
    let isRecord = p.recordTypes.hasKey(cls.toLowerAscii)
    var wantPtr = false
    var ptrInit = e          # non-nil at the d.add below (overridden below)
    var idxName = ""
    if isRecord and withBaseWritable(p, e):
      if e.kind == nkDeref and e.len == 1:
        # with P^ do: the pointer value already IS the address
        ptrInit = e[0]
        wantPtr = true
      elif e.kind == nkIndexExpr and e.len >= 2 and e[0].kind == nkDeref and
          e[0].len == 1:
        # pointer-array element: capture the pointer and the index
        ptrInit = e[0][0]
        wantPtr = true
        inc p.withCounter
        idxName = "pasI" & $p.withCounter
      else:
        let ad = newNode(nkAddr, info)
        ad.add(e)
        ptrInit = ad
        wantPtr = true
    let vd = newNode(nkVarSection, info)
    let d = newNode(nkIdentDefs, info)
    d.add(newIdentNode(temp, info))
    if isRecord:
      if not wantPtr and cls.len > 0:
        d.add(newIdentNode(cls, info))
      else:
        # the temp infers its type from its initializer
        d.add(emptyNode(info))
    elif cls.len > 0:
      d.add(newIdentNode(cls, info))
    else:
      d.add(emptyNode(info))
    if wantPtr:
      d.add(ptrInit)
    else:
      d.add(e)
    vd.add(d)
    result.add(vd)
    if idxName.len > 0:
      let vd2 = newNode(nkVarSection, info)
      let d2 = newNode(nkIdentDefs, info)
      d2.add(newIdentNode(idxName, info))
      d2.add(emptyNode(info))
      d2.add(e[e.len - 1])
      vd2.add(d2)
      result.add(vd2)
    if cls.len > 0:
      p.withTempOwners[temp.toLowerAscii] = cls
    if p.withDepth < p.withTemps.len:
      p.withTemps[p.withDepth] = temp
      p.withClasses[p.withDepth] = cls
      p.withPtrs[p.withDepth] = wantPtr
      p.withIdxNames[p.withDepth] = idxName
    else:
      p.withTemps.add(temp)
      p.withClasses.add(cls)
      p.withPtrs.add(wantPtr)
      p.withIdxNames.add(idxName)
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
  of pxSemiColon:
    # Delphi's EMPTY STATEMENT. It appears in real code in two shapes:
    #   `on Exception do ;`            an empty exception handler
    #   `{$IFDEF X}...{$ENDIF};`       a stray `;` after a conditional
    # GLocks.pas:212 is the second: a vertical {$IFDEF}/{$ELSE}
    # /{$ENDIF} group ends with `{$ENDIF};`, and the conditional token
    # loop leaves the `;` for the enclosing block. Consume it here so it
    # never reaches the expression parser ("expression expected, got ;").
    result = emptyNode(p.tok.info)
    getTokP(p)
  of pxComment:
    result = newNode(nkCommentStmt, p.tok.info)
    result.strVal = p.tok.literal
    getTokP(p)
  of pxCommand:
    # `{@exclude}` and the other documentation directives are annotations,
    # not declarations, and carry no semantics. Skipping one is what
    # Delphi does; leaving it unconsumed made the unit-level loop call
    # skipCom forever (a corpus unit, and every unit absorbing it).
    # The lexer stops the opener right after its name, so the closing
    # brace arrives as a token of its own.
    result = emptyNode(p.tok.info)
    getTokP(p)
  of pxCurlyDirRi:
    # the closing brace of a directive the lexer handed over separately
    # (`{@exclude}`: opener, then `}`). A brace with no opener left is
    # nothing to declare.
    result = emptyNode(p.tok.info)
    getTokP(p)
  of pxCurlyDirLe, pxStarDirLe:
    if isHandledDirective(p):
      result = parseDirective(p)
    elif declDirective(p):
      # a trailing {$else}/{$endif} of a live branch: zero statements
      # at this position; the following statement (if any) takes the
      # slot, so `while ... do {$ENDIF} Inc(x);` keeps Inc inside
      if p.tok.xkind in {pxEnd, pxEof, pxSemiColon, pxElse, pxUntil}:
        result = emptyNode(p.tok.info)
      else:
        result = parseStmt(p)
    else:
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
        if p.tok.ident.toLowerAscii == "if":
          # `{$if <expr>}` cannot be answered here: forward it as a
          # `when` INSIDE the block and let a real compiler choose.
          # Breaking out instead would end the block early and report
          # "expected end but got: {$" (a conditional inside a begin block).
          result.add(parseIfDir(p, succ(p.tok.xkind)))
          if p.tok.xkind == pxSemiColon:
            getTokP(p)
            skipCom(p)
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
      # A comment may sit between the condition and `then`:
      #   if (Stack = nil) or (pfBINDRIGHT in flags)   // comment
      #   then begin
      # two corpus units wrap that way. parseExpr
      # leaves the comment as the current token, and without this the
      # `then` check reports "expected then but got: # comment".
      skipCom(p)
      p.eat(pxThen)
      skipCom(p)
      while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        # `if C then {$IFDEF} ... {$ENDIF} begin` - a conditional
        # may wrap the statement boundary
        if declDirective(p):
          continue
        break
      if p.tok.xkind == pxElse:
        # `then <comment-only>` - the branch body is empty
        branch.add(emptyNode(p.tok.info))
        result.add(branch)
      else:
        branch.add(parseStmt(p))
        result.add(branch)
      skipCom(p)
      while p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}:
        # a conditional wrapping the else chain:
        # if A then B {$IFDEF X} else if C then {$ELSE} else if ... {$ENDIF}
        if not declDirective(p): break
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
    if p.tok.xkind in {pxComment, pxSemiColon}:
      # `while C do { nothing };` / `while C do;` - an empty body
      # (skipCom has usually consumed the comment already)
      skipCom(p)
      if p.tok.xkind == pxSemiColon:
        getTokP(p)
        p.opt(pxSemiColon)
        skipCom(p)
        var d = newNode(nkDiscardStmt, p.tok.info)
        d.add(emptyNode(p.tok.info))
        result.add(d)
        return
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
    if p.tok.xkind notin {pxSemiColon, pxEnd, pxFinally, pxElse, pxEof}:
      # `raise SomeExc.Create(args)` / `raise excInstance`:
      # stash the instance in the current-exception slot, then raise
      # the mapped ErrorCode (nimony raise only transports ErrorCode)
      let e = parseExpr(p)
      if p.tok.xkind == pxSymbol and p.tok.ident.toLowerAscii == "at":
        # `raise E at ErrorAddr;` - the raise location; v1 keeps the
        # plain raise and consumes the qualifier
        getTokP(p)
        skipCom(p)
        discard parseExpr(p)
        skipCom(p)
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
      var b = parseExpr(p)
      # Variant targets: Pascal converts implicitly, nimony not at all
      # (measured) - build the Variant value at the assignment
      block:
        var lhsVT = ""
        if a.kind == nkIdent:
          lhsVT = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
        elif a.kind == nkIndexExpr and a.len >= 1 and a[0].kind == nkIdent:
          # an element of a variant array is itself a Variant, but the store
          # converts to the array's *element* type
          lhsVT = p.varTypes.getOrDefault(a[0].strVal.toLowerAscii)
          if lhsVT.toLowerAscii == "variant":
            b = variantCoerceTyped(p, b)
            lhsVT = "variant:element"
        elif a.kind == nkDotExpr and a.len == 2 and a[0].kind == nkIdent and
            a[1].kind == nkIdent:
          lhsVT = p.fieldTypes.getOrDefault(a[0].strVal.toLowerAscii & "." &
                                            a[1].strVal.toLowerAscii)
        if lhsVT.toLowerAscii == "variant":
          b = variantCoerce(p, b)
        elif lhsVT.toLowerAscii in ["uint64", "qword", "nativeuint",
                                    "uint32", "longword", "cardinal",
                                    "uint16", "word", "uint8", "byte"] and
            b.kind in {nkIntLit, nkInt64Lit}:
          # a literal wider than int32 renders with an 'i64 suffix (int64),
          # which an unsigned target will not take
          let uc = newNode(nkCall, b.info)
          uc.add(newIdentNode(lhsVT, b.info))
          uc.add(b)
          b = uc
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
      if a.kind in {nkIdent, nkIndexExpr} and
          b.kind in {nkInfix, nkCall, nkPrefix}:
        # (nkPrefix: a negative literal like `-512` types as int in
        # nimony - an int32 target needs the width cast too)
        # Pascal computes Integer arithmetic in the declared width;
        # nimony types pure-literal arithmetic as int (64), so
        # `x = 21 * 2` on an int32 x needs an explicit cast
        var lhsTy = ""
        if a.kind == nkIdent:
          lhsTy = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
        elif a.len >= 1 and a[0].kind == nkIdent:
          # an ARRAY ELEMENT target: `digest[i] = i + 1`,
          # `Ar[i] = Value and 255`. The element type comes from the
          # dedicated registry (class/record elements live in
          # arrayVarElems, a different namespace).
          let el = p.arrayVarElemTypes.getOrDefault(
              a[0].strVal.toLowerAscii, "")
          if el.len > 0:
            lhsTy = p.syms.canonical(el).toLowerAscii
            if lhsTy.len == 0: lhsTy = el.toLowerAscii
        if lhsTy in ["int8", "uint8", "int16", "uint16", "int32",
                    "uint32", "int64", "uint64"]:
          let castN = newNode(nkCall, b.info)
          castN.add(newIdentNode(lhsTy, b.info))
          castN.add(b)
          result[1] = castN
      # Delphi's literal cross-type assignments: `s := 'x'` (a char
      # literal into a string target) and `c := ''` (empty into char)
      if a.kind == nkIdent or (a.kind == nkDotExpr and a.len == 2):
        var lhsKey = ""
        var lhsTy2 = ""
        if a.kind == nkIdent:
          lhsTy2 = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
          if lhsTy2.len == 0 and p.selfClass.len > 0:
            # a bare class-field target inside a method: the mapped
            # field types are keyed by the declaring class
            lhsTy2 = p.fieldTypes.getOrDefault(
                p.selfClass.toLowerAscii & "." & a.strVal.toLowerAscii)
            if lhsTy2.len == 0:
              lhsTy2 = p.classFieldTypes.getOrDefault(
                  p.selfClass.toLowerAscii & "." & a.strVal.toLowerAscii)
        else:
          if a[0].kind == nkIdent and a[1].kind == nkIdent:
            lhsTy2 = p.fieldTypes.getOrDefault(
                a[0].strVal.toLowerAscii & "." & a[1].strVal.toLowerAscii)
            if lhsTy2.len == 0:
              lhsTy2 = p.classFieldTypes.getOrDefault(
                  a[0].strVal.toLowerAscii & "." & a[1].strVal.toLowerAscii)
        if lhsTy2.len == 0 and a.kind == nkIdent:
          lhsTy2 = p.varTypes.getOrDefault(a.strVal.toLowerAscii)
        let l2 = lhsTy2.toLowerAscii
        if b.kind == nkCharLit and l2 == "string":
          let cat = newNode(nkInfix, b.info)
          cat.add(newIdentNode("&", b.info))
          cat.add(newNode(nkStrLit, b.info))
          cat.add(b)
          result[1] = cat
        elif b.kind == nkStrLit and b.strVal.len == 0 and
            l2 in ["char", "ansichar", "widechar"]:
          let cc = newNode(nkCast, b.info)
          cc.add(newIdentNode("char", b.info))
          let zero = newNode(nkIntLit, b.info)
          zero.strVal = "0"
          cc.add(zero)
          result[1] = cc
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
        if callee.len > 0 and not retFlag:
          # the RTL spelling map may have renamed the callee
          # (sem_wait -> semInit-style shim spellings); the shim's
          # returnsValue entry uses the mapped spelling
          retFlag = p.syms.returnsValue.getOrDefault(
              p.syms.canonical(callee).toLowerAscii, false)

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
    # properties of the class (and every ancestor) qualify too
    for pr in p.props:
      if pr.cls.toLowerAscii == k and pr.name.toLowerAscii == lower:
        return true
    k = ci.parent
    inc guard
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
      # an unqualified constructor call inside a method binds the
      # implicit self as the first argument (Delphi semantics:
      # `Create(attrs, ...)` in a ctor body calls TEvent.Create(6))
      if n[0].kind == nkIdent and p.qualClass.len > 0 and
          isCtorName(p, p.qualClass, n[0].strVal):
        var nc = newNode(nkCall, n.info)
        nc.noQualCallee = n.noQualCallee
        nc.add(n[0])
        nc.add(newIdentNode("self", n.info))
        for i in 1 ..< n.len:
          nc.add(n[i])
        for i in 2 ..< nc.len:
          nc[i] = selfQualifyInPlace(p, nc[i], scope)
        return nc
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
      if ret.kind != nkIdent: continue
      let rk = ret.strVal.toLowerAscii
      # a value-object result (record OR the object keyword, which is NOT
      # in recordTypes) whose fields are assigned, and a string builder
      # that calls setLen/add before assigning, both fail the proof
      var isValue = p.recordTypes.hasKey(rk) or rk == "variant"
      if not isValue:
        let ci = p.syms.classes.getOrDefault(rk)
        isValue = ci.spelling.len > 0 and not ci.isRef
      # the AST keeps the Pascal spelling; AnsiString/WideString map to
      # nimony's string
      let canon = p.syms.canonical(ret.strVal).toLowerAscii
      let isStr = canon == "string" or rk in
          ["string", "ansistring", "widestring", "shortstring", "utf8string"]
      if not (isValue or isStr): continue
      if def.len == 0 or def[def.len - 1].kind != nkStmtList or
          def[def.len - 1].len == 0: continue
      let body = def[def.len - 1]
      var asgn = newNode(nkAsgn, ret.info)
      asgn.add(newIdentNode("result", ret.info))
      if isStr:
        let empty = newNode(nkStrLit, ret.info)
        empty.strVal = ""
        asgn.add(empty)
      else:
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
      # a class-typed bare index base is the type's default indexed
      # property (Delphi TStrings.Strings default); insert the member
      # so the indexed-property getter/setter lowering applies
      let vt = p.varTypes.getOrDefault(n[0].strVal.toLowerAscii)
      if vt.startsWith("class:"):
        let cls = vt[6 .. ^1].toLowerAscii
        let def = if cls in ["tstrings", "tstringlist",
                             "thashedstringlist"]: "Strings"
                  elif cls == "tlist": "Items"
                  else: ""
        if def.len > 0:
          let dot = newNode(nkDotExpr, n[0].info)
          dot.add(n[0])
          dot.add(newIdentNode(def, n[0].info))
          n[0] = dot
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

proc eqOperandClass(p: var TParser, n: Node): bool =
  ## does this comparison operand name a class-typed value? Locals
  ## register in varTypes, routine params in paramTypes; a ctor call
  ## (`TMemoryStream.Create`) yields a class instance by construction
  if n.kind == nkIdent:
    # the live per-routine param maps win over the accumulating
    # varTypes map (a same-named local/param in an earlier routine
    # leaves a stale entry behind)
    let pt = p.paramTypes.getOrDefault(n.strVal.toLowerAscii)
    if pt.startsWith("class:"):
      return true
    if pt.len > 0 and p.syms.classes.hasKey(pt.toLowerAscii):
      return true
    if p.paramClassTypes.getOrDefault(n.strVal.toLowerAscii).len > 0:
      return true
    if p.varTypes.getOrDefault(n.strVal.toLowerAscii).startsWith("class:"):
      return true
  if n.kind == nkCall and n.len >= 1 and n[0].kind == nkIdent and
      p.syms.classes.hasKey(n[0].strVal.toLowerAscii):
    return true
  return false

proc rewriteClassEq*(p: var TParser, n: Node) =
  ## nimony's `==`/`!=` are restricted (no ref-object operands); Delphi
  ## object comparisons rewrite to the sameRef shim (pointer identity)
  if n.kind == nkInfix and n.len == 3 and n[0].kind == nkIdent and
      n[0].strVal in ["==", "!="]:
    if eqOperandClass(p, n[1]) and eqOperandClass(p, n[2]):
      let call = newNode(nkCall, n.info)
      call.add(newIdentNode("sameRef", n.info))
      call.add(n[1])
      call.add(n[2])
      if n[0].strVal == "==":
        n.kind = nkCall
        n.sons = call.sons
      else:
        let pre = newNode(nkPrefix, n.info)
        pre.add(newIdentNode("not", n.info))
        pre.add(call)
        n.kind = nkPar
        n.sons = @[pre]
      return
  for s in n.sons:
    rewriteClassEq(p, s)

proc rewritePtrAddr*(p: var TParser, n: Node) =
  ## `X := @Y^.F` on a pointer Y: nimony rejects the explicit-deref form
  ## addr(y[].f) as an lvalue and the implicit form addr(y.f) will not
  ## chain through a ptr-ptr, so lower to a temp holding the deref'd
  ## pointer value; addr(temp.F) re-adds the outer level and addresses
  ## the same field
  if n.kind == nkAsgn and n.len == 2 and n[1].kind == nkAddr and
      n[1].len == 1 and n[1][0].kind == nkDotExpr and n[1][0].len == 2 and
      n[1][0][0].kind == nkDeref:
    let y = n[1][0][0][0]
    let yt = rhsExprType(p, y)
    if yt.startsWith("ptr:"):
      let target = p.pointerAliases.getOrDefault(yt[4 .. ^1].toLowerAscii)
      if target.len > 0:
        inc p.ptrTmpCounter
        let tmp = "pasPtrTmp" & $p.ptrTmpCounter
        let info = n.info
        let varDecl = newNode(nkVarSection, info)
        let vd = newNode(nkIdentDefs, info)
        vd.add(newIdentNode(tmp, info))
        vd.add(newIdentNode(target, info))
        vd.add(n[1][0][0])
        varDecl.add(vd)
        let inner = newNode(nkDotExpr, info)
        inner.add(newIdentNode(tmp, info))
        inner.add(n[1][0][1])
        let addrNode = newNode(nkAddr, info)
        addrNode.add(inner)
        let asgn = newNode(nkAsgn, info)
        asgn.add(n[0])
        asgn.add(addrNode)
        n.kind = nkStmtList
        n.sons = @[varDecl, asgn]
      return
  for s in n.sons:
    rewritePtrAddr(p, s)

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
        var access: Node
        if pr.readPath.len > 0:
          access = newIdentNode("self", pr.typ.info)
          for comp in pr.readPath:
            let d = newNode(nkDotExpr, pr.typ.info)
            d.add(access)
            d.add(newIdentNode(comp, pr.typ.info))
            access = d
        else:
          access = newNode(nkDotExpr, pr.typ.info)
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

# ---------------------------------------------------------------------------
# raising call sites (nimony's checked-exception model): a routine whose
# body contains a raise must announce it, and its call sites may only sit
# inside an except-bearing try (a re-raising except still counts - the
# checker only rejects unprotected call sites). Every raising call site
# inside a routine body is wrapped in `try: <stmt> except: raise`; the
# added bare raises pull the enclosing routine into the raising set, so
# the wrap runs to a fixpoint over the call graph. The module's top-level
# main block is already wrapped by the M4-2 machinery.

proc bodyRaises(n: Node): bool =
  if n.kind == nkRaiseStmt: return true
  if n.kind == nkCommentStmt: return false
  for i in 0 ..< n.len:
    if n[i].len > 0:
      if bodyRaises(n[i]): return true
  return false

proc pasDefaultExpr(p: var TParser, ty: Node): Node =
  ## zero value for the flow check's result slot (v1: primitives, refs)
  if ty.kind == nkIdent:
    let k = ty.strVal.toLowerAscii
    if k in ["int8", "int16", "int32", "int64", "uint8", "uint16",
             "uint32", "uint64", "byte", "shortint", "smallint",
             "longint", "longword", "integer", "cardinal", "word",
             "char", "ansichar", "widechar", "nativeint", "nativeuint",
             "sizeint", "ptrdiff", "boolean"]:
      return newIntNode(nkIntLit, 0, ty.info)
    if k in ["float32", "float64", "single", "double", "real",
             "tdatetime", "texttime", "comp", "currency"]:
      return newFloatNode(0.0, ty.info)
    if k == "bool":
      return newIdentNode("false", ty.info)
    if k == "string" or k == "ansistring" or k == "widestring":
      return newStrNode("", ty.info)
    # a class/ref type: nil
    if p.syms.classes.hasKey(k):
      return newIdentNode("nil", ty.info)
  if ty.kind in {nkRefTy, nkPtrTy}:
    return newIdentNode("nil", ty.info)
  return nil

proc raisingCallTarget(n: Node): string =
  ## lowercased callee selector of a call/command, "" when none
  if n.kind in {nkCall, nkCommand} and n.len >= 1:
    if n[0].kind == nkIdent:
      return n[0].strVal.toLowerAscii
    if n[0].kind == nkDotExpr and n[0].len == 2 and
        n[0][1].kind == nkIdent:
      return n[0][1].strVal.toLowerAscii
  return ""

proc exprCallsRaising(p: var TParser, n: Node,
                      set: Table[string, bool]): bool =
  ## does any call in this expression tree target a raising routine
  let sel = raisingCallTarget(n)
  if sel.len > 0 and set.hasKey(sel):
    return true
  for i in 0 ..< n.len:
    if n[i].len > 0:
      if exprCallsRaising(p, n[i], set): return true
  return false

proc wrapRaisingAt(p: var TParser, n: Node, i: int,
                   set: Table[string, bool], inProt: bool,
                   retTy: Node): bool =
  ## may wrap statement n[i]; returns true when wrapped
  let s = n[i]
  case s.kind
  of nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef,
     nkTypeSection, nkTypeDef, nkImportStmt, nkVarSection, nkConstSection,
     nkCommentStmt, nkWhenExpr:
    return false
  of nkTryStmt:
    var hasExcept = false
    for k in 1 ..< s.len:
      if s[k].kind == nkExceptBranch: hasExcept = true
    var changed = false
    for k in 0 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt or hasExcept, retTy) or changed
    return changed
  of nkStmtList, nkElse, nkFinally, nkIfStmt:
    var changed = false
    for k in 0 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt, retTy) or changed
    return changed
  of nkElifBranch, nkOfBranch:
    # kid 0 is the condition / of-value list: not a statement
    var changed = false
    for k in 1 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt, retTy) or changed
    return changed
  of nkWhileStmt:
    # kid 0 is the loop condition
    var changed = false
    for k in 1 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt, retTy) or changed
    return changed
  of nkForStmt:
    # kids 0/1 are the hidden var and the shim iterator call
    var changed = false
    for k in 2 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt, retTy) or changed
    return changed
  of nkCaseStmt:
    var changed = false
    for k in 1 ..< s.len:
      changed = wrapRaisingAt(p, s, k, set, inProt, retTy) or changed
    return changed
  else:
    if inProt: return false
    if not exprCallsRaising(p, s, set): return false
    # wrap in try/except/raise
    let t = newNode(nkTryStmt, s.info)
    let wbody = newNode(nkStmtList, s.info)
    wbody.add(s)
    t.add(wbody)
    let eb = newNode(nkExceptBranch, s.info)
    eb.add(emptyNode(s.info))
    eb.add(emptyNode(s.info))
    let hbody = newNode(nkStmtList, s.info)
    if retTy.kind != nkEmpty:
      let dflt = pasDefaultExpr(p, retTy)
      if dflt != nil:
        let asgn = newNode(nkAsgn, s.info)
        asgn.add(newIdentNode("result", s.info))
        asgn.add(dflt)
        hbody.add(asgn)
    let rn = newNode(nkRaiseStmt, s.info)
    rn.add(emptyNode(s.info))
    hbody.add(rn)
    eb.add(hbody)
    t.add(eb)
    n[i] = t
    return true

proc wrapRaisingCalls*(p: var TParser, module: Node) =
  ## fixpoint: wrap raising call sites, transitively marking callers
  var rounds = 0
  while rounds < 12:
    inc rounds
    var set = initTable[string, bool]()
    for def in module.sons:
      if def.kind in {nkProcDef, nkFuncDef, nkMethodDef} and def.len > 0 and
          def[def.len - 1].kind == nkStmtList:
        if bodyRaises(def[def.len - 1]):
          set[def[0].strVal.toLowerAscii] = true
    var changed = false
    for def in module.sons:
      if def.kind in {nkProcDef, nkFuncDef, nkMethodDef} and def.len > 0 and
          def[def.len - 1].kind == nkStmtList:
        var retTy = emptyNode(def.info)
        if def.len >= 3 and def[2].kind == nkFormalParams and
            def[2][0].kind != nkEmpty:
          retTy = def[2][0]
        let body = def[def.len - 1]
        if body.len > 0:
          changed = wrapRaisingAt(p, body, 0, set, false, retTy) or changed
          for i in 1 ..< body.len:
            changed = wrapRaisingAt(p, body, i, set, false, retTy) or changed
    if not changed: break

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
    if n.kind == nkAsgn and i == 0:
      # never wrap an assignment's LHS in a read call: the shim's
      # `Prop=` setter sugar needs the bare member form (`x.Position = v`)
      continue
    if n.sons[i].len > 0:
      n.sons[i] = wrapMemberCalls(p, n.sons[i])
  return n

const routineDefKinds = {nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef}

proc formalTypeKey(n: Node): string =
  ## a structural key for a formal's TYPE node (spelling + shape)
  result = $n.kind
  if n.strVal.len > 0: result.add(":" & n.strVal)
  for c in n.sons: result.add("/" & formalTypeKey(c))

proc routineFormalKey(n: Node): string =
  ## name + flattened formal types. Needed because two overloads can
  ## share a name AND arity (`WeakSlice.Assign` vs `PartialString.Assign`),
  ## and a name/arity key would let one clobber the other.
  result = n[0].strVal.toLowerAscii & "("
  var first = true
  for j in 1 ..< n[2].len:
    let d = n[2][j]
    if d.kind != nkIdentDefs: continue
    let tk = formalTypeKey(d[d.len - 2])
    for k in 0 ..< d.len - 2:
      if d[k].kind == nkIdent:
        if not first: result.add(",")
        first = false
        result.add(tk)
  result.add(")")

proc collectImplFormals(n: Node; tbl: var Table[string, seq[string]]) =
  ## bodied routine definitions, keyed by name + formal types -> names
  if n.kind in routineDefKinds and n.len >= 3 and
      n[n.len - 1].kind != nkEmpty and n[2].kind == nkFormalParams:
    var names: seq[string] = @[]
    for j in 1 ..< n[2].len:
      let d = n[2][j]
      if d.kind != nkIdentDefs: continue
      for k in 0 ..< d.len - 2:
        if d[k].kind == nkIdent: names.add(d[k].strVal)
    tbl[routineFormalKey(n)] = names
  for c in n.sons: collectImplFormals(c, tbl)

proc applyForwardFormals(n: Node; tbl: Table[string, seq[string]]) =
  ## nimony's routine identity includes PARAMETER NAMES, so a forward
  ## declaration left with the Pascal names and its implementation
  ## renamed to `pasV_X` by lowerMutableValueParams register as two
  ## overloads and every call is "ambiguous call". Reuse the
  ## implementation's formal names on the matching bodiless declaration.
  if n.kind in routineDefKinds and n.len >= 3 and
      n[n.len - 1].kind == nkEmpty and n[2].kind == nkFormalParams:
    var idents: seq[Node] = @[]
    for j in 1 ..< n[2].len:
      let d = n[2][j]
      if d.kind != nkIdentDefs: continue
      for k in 0 ..< d.len - 2:
        if d[k].kind == nkIdent: idents.add(d[k])
    let impl = tbl.getOrDefault(routineFormalKey(n))
    if impl.len == idents.len:
      for k in 0 ..< idents.len: idents[k].strVal = impl[k]
  for c in n.sons: applyForwardFormals(c, tbl)

proc parseUnit*(p: var TParser): Node =
  ## parse a whole unit/program; returns the module statement list with
  ## all post-passes applied
  # the prelude runtime modules are always importable: register their
  # exported spellings for case-insensitive resolution
  absorbNimModule(p, "systempas")
  absorbNimModule(p, "pasdatetime")
  var skippedTokens = 0
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
      # A closer that declDirective declines belongs to an ENCLOSING
      # group. parseStmt would return without consuming it, so the loop
      # below would call skipCom forever - an apparent hang with no
      # output at all. Stop and let the owner of the group see it.
      if p.tok.ident.toLowerAscii in ["else", "elseif", "endif", "ifend"]:
        break
    let dLine = p.tok.info.line
    let dCol = p.tok.info.col
    let s = parseStmt(p)
    if p.tok.info.line == dLine and p.tok.info.col == dCol and
        p.tok.xkind != pxEof:
      # A statement that consumes nothing spins here forever, with NO
      # output at all - the worst failure mode for a batch translation.
      # Report the token, step over it, and give up if that keeps
      # happening, so the unit fails loudly instead of hanging.
      parWarning(p, "unit-noprogress-" & $p.tok.xkind,
                 renderInfo(p.tok.info) & " Warning: skipping token the " &
                 "statement parser did not consume (" & $p.tok.xkind & ")")
      inc skippedTokens
      if skippedTokens > 32:
        parError(p, "too many unconsumed tokens at unit level")
      getTokP(p)
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
  # nimony's checked-exception model: raising call sites must sit inside
  # an except-bearing try; wrap them and let the added bare raises pull
  # transitive callers into the raising set
  wrapRaisingCalls(p, p.module)
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
  block:
    var implFormals = initTable[string, seq[string]]()
    collectImplFormals(m2, implFormals)
    applyForwardFormals(m2, implFormals)
  result = m2
