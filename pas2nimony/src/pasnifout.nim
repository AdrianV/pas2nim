#
#           pasnifout - direct .p.nif emission from the Pascal AST
#
# Stage-2 milestone: emit the parsed-NIF module (`nim-parsed` dialect)
# straight from paspars.nim's AST, bypassing the Nim renderer and
# nifler entirely. The Nim renderer (pasnimout.nim) stays available as
# a debug path (--emit-nim).
#
# Design rule (from the bif investigation, doc/nimony-compat.md):
# emission goes into a TokenBuf, never into strings. Text output is
# writeFileAndIndex; the binary .bif switch stays a one-liner
# (bif.store) once upstream readers sniff it.
#
# Reference shapes were taken from nifler output (see doc/toolchain.md)
# and macro_plugin.nim's emission helpers.

import std/[strutils, syncio, os, tables]
import pasast, passym, paspars
import ../../nimony/src/lib/nifpools
import ../../nimony/src/lib/bitabs
import ../../nimony/src/lib/nifindexes
import ../../nimony/src/lib/symparser
import ../../nimony/src/lib/nifcore

type
  NifEmitter = ref object
    buf: TokenBuf
    fileId: FileId
    syms: ptr SymTab
    srcFile: string   ## the name recorded in line infos (as passed)

proc initEmitter(e: var NifEmitter; module: Node; syms: var SymTab;
                 srcFile: string) =
  e.buf = nifpools.createTokenBuf(1024)
  e.syms = addr syms
  e.srcFile = srcFile
  e.fileId = e.buf.pool.filenames.getOrIncl(srcFile)

proc info(e: NifEmitter; n: Node): NifLineInfo =
  if n == nil:
    NifLineInfo(file: e.fileId, line: 0, col: 0)
  else:
    NifLineInfo(file: e.fileId, line: int32(n.info.line),
                col: int32(n.info.col))

proc canon(e: NifEmitter; name: string): string =
  if e.syms != nil:
    result = e.syms[].canonical(name)
  else:
    result = name

proc nameOf(e: NifEmitter; n: Node): string =
  ## canonical spelling of an identifier node
  result = e.canon(n.strVal)

# ---------------------------------------------------------------------------
# expressions

proc emitExpr(e: var NifEmitter; n: Node)

proc emitAtom(e: var NifEmitter; n: Node) =
  let i = e.info(n)
  case n.kind
  of nkIdent:
    if n.strVal.toLowerAscii == "nil":
      # Pascal nil -> the (nil) tag, not an identifier
      e.buf.copyInto(globalTags.registerTag("nil"), i):
        discard
    else:
      e.buf.addIdent(e.nameOf(n), i)
  of nkIntLit, nkInt64Lit: e.buf.addIntLit(n.intVal, i)
  of nkFloatLit: e.buf.addFloatLit(n.floatVal, i)
  of nkStrLit: e.buf.addStrLit(n.strVal, i)
  of nkCharLit:
    if n.strVal.len > 0: e.buf.addCharLit(n.strVal[0], i)
    else: e.buf.addCharLit(' ', i)
  of nkNilLit:
    # the (nil) tag, not an identifier
    e.buf.copyInto(globalTags.registerTag("nil"), i):
      discard
  of nkTrue: e.buf.addIdent("true", i)
  of nkFalse: e.buf.addIdent("false", i)
  of nkEmpty: e.buf.addDotToken(i)
  else: e.buf.addDotToken(i)

proc emitCall(e: var NifEmitter; n: Node) =
  ## (call callee arg...); set ops are plain calls to incl/excl, the
  ## parser already canonicalizes Include/Exclude to those names
  let i = e.info(n)
  when defined(PASLER_DEBUG_AST):
    if n.len > 0 and n[0].kind notin {nkIdent, nkDotExpr}:
      echo "DBG call callee kind=", $n[0].kind, " sons=", n.len
      for s in n.sons:
        echo "DBG   son: kind=", $s.kind, " len=", s.len, " strVal=", s.strVal
  e.buf.copyInto(globalTags.registerTag("call"), i):
    for son in n.sons:
      e.emitExpr(son)

proc emitAnonProc(e: var NifEmitter; n: Node) =
  ## anonymous method value: (proc . . . . (params ...) ret . . (stmts))
  let i = e.info(n)
  e.buf.copyInto(globalTags.registerTag("proc"), i):
    for k in 0 ..< 4:          # name, export, pattern, typevars
      e.buf.addDotToken(i)
    e.emitParamList(n[2])
    e.emitTypeDesc(n[2][0])
    e.buf.addDotToken(i)       # pragmas
    e.buf.addDotToken(i)       # effects
    e.emitStmts(n[n.len - 1])

proc emitExpr(e: var NifEmitter; n: Node) =
  let i = e.info(n)
  case n.kind
  of nkInfix:
    e.buf.copyInto(globalTags.registerTag("infix"), i):
      e.buf.addIdent(escapeNifOp(e.canon(n[0].strVal)), i)
      e.emitExpr(n[1])
      e.emitExpr(n[2])
  of nkPrefix:
    e.buf.copyInto(globalTags.registerTag("prefix"), i):
      e.buf.addIdent(escapeNifOp(e.canon(n[0].strVal)), i)
      e.emitExpr(n[1])
  of nkCall, nkCommand:
    e.emitCall(n)
  of nkProcDef, nkFuncDef:
    # anonymous method value
    e.emitAnonProc(n)
  of nkCurly:
    # set literal: empty sets are the bare (curly) tag
    e.buf.copyInto(globalTags.registerTag("curly"), i):
      for son in n.sons:
        e.emitExpr(son)
  of nkRange:
    # case-label range a..b
    e.buf.copyInto(globalTags.registerTag("infix"), i):
      e.buf.addIdent("..", i)
      e.emitExpr(n[0])
      e.emitExpr(n[1])
  of nkDotExpr:
    e.buf.copyInto(globalTags.registerTag("dot"), i):
      e.emitExpr(n[0])
      e.emitExpr(n[1])
  of nkIndexExpr:
    # (at receiver index)
    e.buf.copyInto(globalTags.registerTag("at"), i):
      e.emitExpr(n[0])
      for j in 1 ..< n.len:
        e.emitExpr(n[j])
  of nkCast:
    # nifler parses `cast[T](x)` as (cast T x)
    e.buf.copyInto(globalTags.registerTag("cast"), e.info(n)):
      e.emitTypeDesc(n[0])
      e.emitExpr(n[1])
  of nkPar:
    if n.len == 1:
      e.emitExpr(n[0])
    else:
      e.emitAtom(n)

  of nkStmtList:
    # rare wrapper; emit inline
    for son in n.sons:
      e.emitExpr(son)
  of nkBracket:
    # array literal (array-of-const args): (bracket elem1 elem2 ...)
    e.buf.copyInto(globalTags.registerTag("bracket"), i):
      for son in n.sons:
        e.emitExpr(son)
  else:
    e.emitAtom(n)

# ---------------------------------------------------------------------------
# type descriptors

proc emitTypeVars(e: var NifEmitter; d: Node; i: NifLineInfo) =
  ## the `(typevars (typevar K . . . .) ...)` slot; a dot when the def
  ## carries no type parameters
  if d.len > 1 and d[1].kind == nkBracket and d[1].len > 0:
    e.buf.copyInto(globalTags.registerTag("typevars"), i):
      for pn in d[1].sons:
        let pi = e.info(pn)
        e.buf.copyInto(globalTags.registerTag("typevar"), pi):
          e.buf.addIdent(e.nameOf(pn), pi)
          for k in 0 ..< 4:
            e.buf.addDotToken(pi)
  else:
    e.buf.addDotToken(i)

proc emitTypeDesc(e: var NifEmitter; n: Node) =
  let i = e.info(n)
  case n.kind
  of nkIdent:
    e.buf.addIdent(e.nameOf(n), i)
  of nkArrayTy:
    # (at array len elem)
    e.buf.copyInto(globalTags.registerTag("at"), i):
      e.buf.addIdent("array", i)
      e.emitExpr(n[0])
      e.emitTypeDesc(n[1])
  of nkSetTy:
    e.buf.copyInto(globalTags.registerTag("at"), i):
      e.buf.addIdent("set", i)
      e.emitTypeDesc(n[0])
  of nkOpenArrayTy:
    # nimony: (array len elem) with openarray spelling is not expressible
    # pre-sem; use the plain spelling for now
    e.buf.addIdent("openArray", i)
  of nkSeqTy:
    e.buf.addIdent("seq", i)
  of nkVarTy:
    # (mut <type>) - var parameters
    e.buf.copyInto(globalTags.registerTag("mut"), i):
      e.emitTypeDesc(n[0])
  of nkPtrTy, nkRefTy:
    e.buf.addIdent("ptr", i)
  of nkProcTy:
    # inline procedure type: (proctype .... (params ...) ret . . .)
    e.buf.copyInto(globalTags.registerTag("proctype"), i):
      for k in 0 ..< 4:
        e.buf.addDotToken(i)
      e.emitParamList(n[0])
      e.emitTypeDesc(n[0][0])
      for k in 0 ..< 3:
        e.buf.addDotToken(i)
  of nkEmpty:
    e.buf.addDotToken(i)
  of nkIndexExpr:
    # generic instantiation: (at Base arg1 arg2)
    e.buf.copyInto(globalTags.registerTag("at"), i):
      e.emitTypeDesc(n[0])
      for j in 1 ..< n.len:
        e.emitTypeDesc(n[j])
  else:
    e.buf.addDotToken(i)

proc emitDefaultInit(e: var NifEmitter; ty: Node; i: NifLineInfo) =
  ## initializer satisfying nimony's definite-assignment analysis;
  ## dot when the type needs none (mirrors pasnimout.defaultInit)
  if ty == nil or ty.kind == nkEmpty:
    e.buf.addDotToken(i)
    return
  case ty.kind
  of nkIdent:
    let lower = ty.strVal.toLowerAscii
    case lower
    of "integer", "int8", "int16", "int32", "int64", "int", "longint",
       "smallint", "byte", "word", "cardinal", "longword", "uint8",
       "uint16", "uint32", "uint64", "nativeint", "nativeuint", "qword",
       "shortint", "usize", "isize":
      e.buf.addIntLit(0, i)
    of "char", "ansichar", "widechar":
      e.buf.addCharLit('\x00', i)
    of "bool", "boolean":
      e.buf.addIdent("false", i)
    of "string", "ansistring", "widestring", "unicodestring", "shortstring",
       "tstring":
      e.buf.addStrLit("", i)
    of "single", "double", "float", "float32", "float64", "real", "extended":
      e.buf.addFloatLit(0.0, i)
    of "tobject", "rootref", "pointer", "pchar", "pwidechar":
      e.buf.copyInto(globalTags.registerTag("nil"), i):
        discard
    else:
      let ci = e.syms[].lookupClass(lower)
      if ci.spelling.len > 0 and ci.isRef:
        # class instances are nilable, value objects need default
        e.buf.copyInto(globalTags.registerTag("nil"), i):
          discard
      else:
        e.buf.copyInto(globalTags.registerTag("call"), i):
          e.buf.addIdent("default", i)
          e.buf.addIdent(e.canon(ty.strVal), i)
  of nkRefTy, nkPtrTy, nkProcTy:
    e.buf.copyInto(globalTags.registerTag("nil"), i):
      discard
  of nkSetTy:
    e.buf.copyInto(globalTags.registerTag("curly"), i):
      discard
  of nkArrayTy:
    e.buf.copyInto(globalTags.registerTag("call"), i):
      e.buf.addIdent("default", i)
      e.buf.copyInto(globalTags.registerTag("at"), i):
        e.buf.addIdent("array", i)
        e.emitExpr(ty[0])
        e.emitTypeDesc(ty[1])
  else:
    e.buf.copyInto(globalTags.registerTag("call"), i):
      e.buf.addIdent("default", i)
      e.emitTypeDesc(ty)

# ---------------------------------------------------------------------------
# statements

proc emitStmts(e: var NifEmitter; n: Node)

proc emitStmtBody(e: var NifEmitter; n: Node) =
  ## a branch body: our AST holds nkStmtList; emit `(stmts ...)`
  if n.kind == nkStmtList:
    e.emitStmts(n)
  else:
    let i = e.info(n)
    e.buf.copyInto(globalTags.registerTag("stmts"), i):
      e.emitStmt(n)

proc emitStmts(e: var NifEmitter; n: Node) =
  ## emit an nkStmtList as a `(stmts ...)` tag
  let i = e.info(n)
  e.buf.copyInto(globalTags.registerTag("stmts"), i):
    for son in n.sons:
      e.emitStmt(son)

proc emitStmt(e: var NifEmitter; n: Node) =
  let i = e.info(n)
  when defined(PASLER_DEBUG_AST):
    if n.kind == nkDiscardStmt:
      echo "DBG discard son kind=", n[0].kind, " len=", n[0].len
      for s in n[0].sons:
        echo "DBG   son: ", $s.kind, " strVal=", s.strVal
  case n.kind
  of nkAsgn:
    e.buf.copyInto(globalTags.registerTag("asgn"), i):
      e.emitExpr(n[0])
      e.emitExpr(n[1])
  of nkIfStmt:
    e.buf.copyInto(globalTags.registerTag("if"), i):
      for branch in n.sons:
        if branch.kind == nkElifBranch:
          e.buf.copyInto(globalTags.registerTag("elif"), e.info(branch)):
            e.emitExpr(branch[0])
            e.emitStmtBody(branch[1])
        elif branch.kind == nkElse:
          e.buf.copyInto(globalTags.registerTag("else"), e.info(branch)):
            e.emitStmtBody(branch[0])
  of nkCaseStmt:
    # (case SELECTOR (of (ranges v...) body) ... (else body))
    e.buf.copyInto(globalTags.registerTag("case"), i):
      e.emitExpr(n[0])
      for branch in n.sons:
        if branch.kind == nkOfBranch:
          let bi = e.info(branch)
          e.buf.copyInto(globalTags.registerTag("of"), bi):
            e.buf.copyInto(globalTags.registerTag("ranges"), bi):
              for j in 0 ..< branch.len - 1:
                e.emitExpr(branch[j])
            e.emitStmtBody(branch[branch.len - 1])
        elif branch.kind == nkElse:
          e.buf.copyInto(globalTags.registerTag("else"), e.info(branch)):
            e.emitStmtBody(branch[0])
  of nkWhileStmt:
    e.buf.copyInto(globalTags.registerTag("while"), i):
      e.emitExpr(n[0])
      e.emitStmtBody(n[1])
  of nkDiscardStmt:
    e.buf.copyInto(globalTags.registerTag("discard"), i):
      if n.len > 0 and n[0].kind != nkEmpty:
        e.emitExpr(n[0])
      else:
        e.buf.addDotToken(i)
  of nkReturnStmt:
    e.buf.copyInto(globalTags.registerTag("ret"), i):
      if n.len > 0 and n[0].kind != nkEmpty:
        e.emitExpr(n[0])
      else:
        e.buf.addDotToken(i)
  of nkRaiseStmt:
    # (raise EXPR) - the ErrorCode model
    e.buf.copyInto(globalTags.registerTag("raise"), i):
      e.emitExpr(n[0])
  of nkTryStmt:
    # (try (stmts) (except (infix as T V) (stmts)) ... (fin (stmts)))
    e.buf.copyInto(globalTags.registerTag("try"), i):
      e.emitStmts(n[0])
      for bi in 1 ..< n.len:
        let b = n[bi]
        if b.kind == nkExceptBranch:
          let xi = e.info(b)
          e.buf.copyInto(globalTags.registerTag("except"), xi):
            if b[0].kind != nkEmpty and b.len > 1 and b[1].kind == nkIdent:
              e.buf.copyInto(globalTags.registerTag("infix"), xi):
                e.buf.addIdent("as", xi)
                e.emitTypeDesc(b[0])
                e.buf.addIdent(e.nameOf(b[1]), xi)
            else:
              e.emitTypeDesc(b[0])
            # handler body: single stmt node (case) or stmt list
            e.emitStmtBody(b[b.len - 1])
        elif b.kind == nkFinally:
          # nkFinally holds the body statements directly (flattened)
          e.buf.copyInto(globalTags.registerTag("fin"), e.info(b)):
            e.buf.copyInto(globalTags.registerTag("stmts"), e.info(b)):
              for s in b.sons:
                e.emitStmt(s)
  of nkBreakStmt:
    e.buf.copyInto(globalTags.registerTag("break"), i):
      if n.len > 0 and n[0].kind == nkIdent:
        e.buf.addIdent(n[0].strVal, i)
      else:
        e.buf.addDotToken(i)
  of nkBlockStmt:
    e.buf.copyInto(globalTags.registerTag("block"), i):
      e.buf.addIdent(n[0].strVal, e.info(n[0]))
      e.emitStmts(n[1])
  of nkContinueStmt:
    e.buf.copyInto(globalTags.registerTag("continue"), i):
      e.buf.addDotToken(i)
  of nkForStmt:
    if n.len == 3:
      # (for ITER (unpackflat (let _ . . ..)) (stmts body))
      e.buf.copyInto(globalTags.registerTag("for"), i):
        e.emitExpr(n[1])
        let ui = e.info(n[0])
        e.buf.copyInto(globalTags.registerTag("unpackflat"), ui):
          e.buf.copyInto(globalTags.registerTag("let"), ui):
            e.buf.addIdent("_", ui)
            e.buf.addDotToken(ui)
            e.buf.addDotToken(ui)
            e.buf.addDotToken(ui)
            e.buf.addDotToken(ui)
        e.emitStmtBody(n[2])
  of nkCall, nkCommand:
    # statement-level call
    e.emitCall(n)
  of nkVarSection, nkConstSection, nkTypeSection, nkProcDef, nkFuncDef,
     nkMethodDef, nkTemplateDef, nkImportStmt:
    e.emitDef(n)
  of nkWhenExpr:
    # {$if false} etc: (when (elif cond body) ... (else body))
    e.buf.copyInto(globalTags.registerTag("when"), i):
      for branch in n.sons:
        if branch.kind == nkElifBranch:
          e.buf.copyInto(globalTags.registerTag("elif"), e.info(branch)):
            e.emitExpr(branch[0])
            e.emitStmts(branch[1])
        elif branch.kind == nkElse:
          e.buf.copyInto(globalTags.registerTag("else"), e.info(branch)):
            e.emitStmts(branch[0])
  of nkCommentStmt:
    discard "comments carry no semantics in NIF"
  of nkStmtList:
    # top-level block (program begin..end): inline the statements into
    # the enclosing stmts, mirroring nifler's module shape
    for son in n.sons:
      e.emitStmt(son)
  else:
    # expression statement fallback
    e.emitExpr(n)

# ---------------------------------------------------------------------------
# declarations

proc emitExportSlot(e: var NifEmitter; nameNode: Node; i: NifLineInfo) =
  if nameNode != nil and nameNode.exported:
    e.buf.addIdent("x", i)
  else:
    e.buf.addDotToken(i)

proc emitParamList(e: var NifEmitter; params: Node) =
  ## nkFormalParams: [0]=rettype, [1..]=defs (nkIdentDefs, multi-name)
  let i = e.info(params)
  e.buf.copyInto(globalTags.registerTag("params"), i):
    for k in 1 ..< params.len:
      let d = params[k]
      if d.kind != nkIdentDefs: continue
      let ty = d[d.len - 2]
      let init = d[d.len - 1]
      for j in 0 ..< d.len - 2:
        let pi = e.info(d[j])
        e.buf.copyInto(globalTags.registerTag("param"), pi):
          e.buf.addIdent(e.nameOf(d[j]), pi)
          e.buf.addDotToken(pi)
          e.buf.addDotToken(pi)
          e.emitTypeDesc(ty)
          if init.kind != nkEmpty:
            e.emitExpr(init)
          else:
            e.buf.addDotToken(pi)

proc escapeNifOp(name: string): string =
  ## the writer (nifbuilder) escapes a leading (+ -) byte to \2B / \2D;
  ## pass the raw spelling through and let the writer do it
  result = name

proc isPlainIdentStr(s: string): bool =
  ## true for names that can appear as a bare NIF identifier
  if s.len == 0:
    return false
  for ch in s:
    if not ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or
            (ch >= '0' and ch <= '9') or ch == '_'):
      return false
  result = true

proc emitProcName(e: var NifEmitter; n: Node; i: NifLineInfo) =
  ## operator names like `data2 =` need the (quoted ...) form
  let name = e.nameOf(n[0])
  if name.find(' ') >= 0:
    e.buf.copyInto(globalTags.registerTag("quoted"), i):
      for part in name.split(' '):
        if part.len > 0:
          e.buf.addIdent(part, i)
  elif not isPlainIdentStr(name):
    # operator symbols: nifler writes (+ -) as hex escapes, the rest raw
    e.buf.copyInto(globalTags.registerTag("quoted"), i):
      e.buf.addIdent(escapeNifOp(name), i)
  else:
    e.buf.addIdent(name, i)

proc containsRaiseNode(n: Node): bool =
  if n.kind == nkRaiseStmt: return true
  for i in 0 ..< n.len:
    if containsRaiseNode(n.sons[i]): return true
  return false

proc emitProcDef(e: var NifEmitter; n: Node) =
  # (proc NAME export . . (params...) ret . . body)
  # templates share the shape with the `template` tag
  let i = e.info(n)
  let kw = case n.kind
           of nkFuncDef: "func"
           of nkMethodDef: "method"
           of nkTemplateDef: "template"
           else: "proc"
  e.buf.copyInto(globalTags.registerTag(kw), i):
    e.emitProcName(n, i)
    e.emitExportSlot(n[0], i)
    e.buf.addDotToken(i)      # pattern
    e.emitTypeVars(n, i)
    if n.len > 2 and n[2].kind == nkFormalParams:
      e.emitParamList(n[2])
      # rettype from params[0]
      e.emitTypeDesc(n[2][0])
    else:
      e.buf.copyInto(globalTags.registerTag("params"), i):
        discard
      e.buf.addDotToken(i)
    if n.len > 0 and n[n.len - 1].kind != nkEmpty and
        containsRaiseNode(n[n.len - 1]):
      # raising procs announce it (ErrorCode model)
      e.buf.copyInto(globalTags.registerTag("pragmas"), i):
        e.buf.addIdent("raises", i)
    else:
      e.buf.addDotToken(i)      # pragmas
    e.buf.addDotToken(i)      # effects
    let body = n[n.len - 1]
    if body.kind == nkEmpty:
      # forward declaration: no body slot; sem requires the sig-only form
      e.buf.addDotToken(i)
    else:
      e.emitStmts(body)

proc emitTypeDef(e: var NifEmitter; n: Node; hoisted: var seq[Node]) =
  # (type NAME export . (pragmas inheritable)? (ref (object PARENT flds...)))
  let i = e.info(n)
  let ty = n[2]
  e.buf.copyInto(globalTags.registerTag("type"), i):
    e.buf.addIdent(e.nameOf(n[0]), e.info(n[0]))
    e.emitExportSlot(n[0], i)
    e.emitTypeVars(n, i)
    if ty.kind == nkRefTy and ty[0].kind == nkObjectTy:
      # class
      let obj = ty[0]
      let pi = e.info(n)
      if n.len > 1 and n[1].kind == nkBracket and n[1].len > 0:
        e.buf.addDotToken(pi)   # generic: no {.inheritable.} allowed
      else:
        e.buf.copyInto(globalTags.registerTag("pragmas"), pi):
          e.buf.addIdent("inheritable", pi)
      e.buf.copyInto(globalTags.registerTag("ref"), e.info(ty)):
        let oi = e.info(obj)
        e.buf.copyInto(globalTags.registerTag("object"), oi):
          if obj[0].kind == nkOfInherit:
            e.emitTypeDesc(obj[0][0])
          else:
            e.buf.addDotToken(oi)
          let body = obj[1]
          if body.kind == nkRecList:
            for field in body.sons:
              if field.kind in {nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef}:
                hoisted.add(field)
              elif field.kind == nkIdentDefs:
                for j in 0 ..< field.len - 2:
                  let fi = e.info(field[j])
                  e.buf.copyInto(globalTags.registerTag("fld"), fi):
                    e.buf.addIdent(e.nameOf(field[j]), fi)
                    e.buf.addDotToken(fi)
                    e.buf.addDotToken(fi)
                    e.emitTypeDesc(field[field.len - 2])
                    if field[field.len - 1].kind != nkEmpty:
                      e.emitExpr(field[field.len - 1])
                    else:
                      e.buf.addDotToken(fi)
    elif ty.kind == nkEnumTy:
      # (type NAME export . . (enum . (efld NAME . . . .)...))
      let ei = e.info(ty)
      e.buf.addDotToken(i)
      e.buf.copyInto(globalTags.registerTag("enum"), ei):
        e.buf.addDotToken(ei)
        for son in ty.sons:
          let si = e.info(son)
          e.buf.copyInto(globalTags.registerTag("efld"), si):
            e.buf.addIdent(e.nameOf(son), si)
            e.buf.addDotToken(si)
            e.buf.addDotToken(si)
            e.buf.addDotToken(si)
            e.buf.addDotToken(si)
    elif ty.kind == nkProcTy:
      # (type NAME export . . (proctype . . . . . (params param...) .
      #  (pragmas closure) . .)) - method-pointer proc types
      let pi = e.info(ty)
      e.buf.addDotToken(i)
      e.buf.copyInto(globalTags.registerTag("proctype"), pi):
        for k in 0 ..< 4:
          e.buf.addDotToken(pi)
        let params = ty[0]
        e.buf.copyInto(globalTags.registerTag("params"), pi):
          for j in 1 ..< params.len:
            let d = params[j]
            if d.kind == nkIdentDefs:
              for k in 0 ..< d.len - 2:
                let fi = e.info(d[k])
                e.buf.copyInto(globalTags.registerTag("param"), fi):
                  e.buf.addIdent(e.nameOf(d[k]), fi)
                  e.buf.addDotToken(fi)
                  e.buf.addDotToken(fi)
                  e.emitTypeDesc(d[d.len - 2])
                  e.buf.addDotToken(fi)
        # the return type lives in the params' slot 0
        if params[0].kind != nkEmpty:
          e.emitTypeDesc(params[0])
        else:
          e.buf.addDotToken(pi)
        if ty.len > 1 and ty[1].kind == nkPragma:
          e.buf.copyInto(globalTags.registerTag("pragmas"), pi):
            for pr in ty[1].sons:
              e.buf.addIdent(e.nameOf(pr), pi)
        else:
          e.buf.addDotToken(pi)
        e.buf.addDotToken(pi)
        e.buf.addDotToken(pi)
    elif ty.kind == nkObjectTy:
      # (type NAME export . (pragmas inheritable)? (object parent flds))
      # plain Pascal objects are always extendable -> inheritable; the
      # dot is the empty-pragmas slot nifler always emits
      let oi = e.info(ty)
      if not ty.isRecordType:
        e.buf.copyInto(globalTags.registerTag("pragmas"), i):
          e.buf.addIdent("inheritable", i)
      else:
        e.buf.addDotToken(i)
      e.buf.copyInto(globalTags.registerTag("object"), oi):
        # parent type (son 0: nkOfInherit) or empty for a base object
        if ty[0].kind == nkOfInherit:
          e.emitTypeDesc(ty[0][0])
        else:
          e.buf.addDotToken(oi)
        # object sons: [inheritance slot, body]; fields live in the body
        if ty.len > 1 and ty[1].kind == nkRecList:
          for field in ty[1].sons:
            if field.kind == nkIdentDefs:
              for j in 0 ..< field.len - 2:
                let fi = e.info(field[j])
                e.buf.copyInto(globalTags.registerTag("fld"), fi):
                  e.buf.addIdent(e.nameOf(field[j]), fi)
                  e.buf.addDotToken(fi)
                  e.buf.addDotToken(fi)
                  e.emitTypeDesc(field[field.len - 2])
                  if field[field.len - 1].kind != nkEmpty:
                    e.emitExpr(field[field.len - 1])
                  else:
                    e.buf.addDotToken(fi)

    else:
      # type alias (ident, seq, openarray, set, ...): plain type desc
      # after the empty-pragmas slot
      e.buf.addDotToken(i)
      e.emitTypeDesc(ty)

proc emitVarDef(e: var NifEmitter; d: Node; tag: string) =
  ## one nkIdentDefs -> one (var/const/let NAME export . TYPE INIT)
  let i = e.info(d)
  for j in 0 ..< d.len - 2:
    let vi = e.info(d[j])
    e.buf.copyInto(globalTags.registerTag(tag), vi):
      e.buf.addIdent(e.nameOf(d[j]), vi)
      e.emitExportSlot(d[j], vi)
      e.buf.addDotToken(vi)     # pattern
      e.emitTypeDesc(d[d.len - 2])
      let init = d[d.len - 1]
      if init.kind != nkEmpty:
        e.emitExpr(init)
      else:
        e.emitDefaultInit(d[d.len - 2], vi)

proc emitDef(e: var NifEmitter; n: Node) =
  case n.kind
  of nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef:
    e.emitProcDef(n)
  of nkTypeSection:
    var allHoisted: seq[Node] = @[]
    for def in n.sons:
      if def.kind in {nkCommentStmt, nkEmpty}:
        # forward declarations (`Name = class;`) and comments carry no
        # NIF semantics
        continue
      var hoisted: seq[Node] = @[]
      e.emitTypeDef(def, hoisted)
      for h in hoisted: allHoisted.add(h)
    for h in allHoisted:
      e.emitProcDef(h)
  of nkVarSection:
    for d in n.sons:
      if d.kind == nkIdentDefs:
        e.emitVarDef(d, "var")
  of nkConstSection:
    for d in n.sons:
      if d.kind == nkIdentDefs:
        e.emitVarDef(d, "const")
  of nkImportStmt:
    e.emitImport(n)
  else:
    discard


proc emitUnitPath(e: var NifEmitter; name: string; i: NifLineInfo) =
  ## a unit spelling like std/strutils becomes (infix / std strutils)
  let parts = name.split('/')
  if parts.len == 1:
    e.buf.addIdent(name, i)
  else:
    e.buf.copyInto(globalTags.registerTag("infix"), i):
      e.buf.addIdent("/", i)
      e.buf.addIdent(parts[0], i)
      if parts.len == 2:
        e.buf.addIdent(parts[1], i)
      else:
        e.buf.copyInto(globalTags.registerTag("infix"), i):
          e.buf.addIdent("/", i)
          for k in 1 ..< parts.len:
            e.buf.addIdent(parts[k], i)

proc emitImport(e: var NifEmitter; n: Node) =
  let i = e.info(n)
  e.buf.copyInto(globalTags.registerTag("import"), i):
    for son in n.sons:
      e.emitImportPath(son)

proc emitImportPath(e: var NifEmitter; n: Node) =
  case n.kind
  of nkInfix:
    let i = e.info(n)
    e.buf.copyInto(globalTags.registerTag("infix"), i):
      e.buf.addIdent(e.canon(n[0].strVal), i)
      e.emitImportPath(n[1])
      e.emitImportPath(n[2])
  of nkBracket:
    let i = e.info(n)
    e.buf.copyInto(globalTags.registerTag("bracket"), i):
      for son in n.sons:
        e.emitImportPath(son)
  of nkIdent:
    if n.strVal.find('/') >= 0:
      e.emitUnitPath(n.strVal, e.info(n))
    else:
      e.buf.addIdent(e.nameOf(n), e.info(n))
  else:
    e.emitExpr(n)

# ---------------------------------------------------------------------------
# module level

proc writeNifFile(path: string; content: string) =
  var f: File
  if f.open(path, fmWrite):
    f.write(content)
    f.close()
  else:
    write(stderr, "pasnifout: cannot open " & path & "\n")
    quit(1)

proc emitNifModule*(module: Node; syms: var SymTab; infile: string;
                    outPath: string; flags: set[TParserFlag] = {}) =
  ## emit `outPath` (.p.nif) plus `outPath.deps.nif`
  var e = NifEmitter()
  e.buf = nifpools.createTokenBuf(1024)
  e.syms = addr syms
  e.srcFile = infile
  e.fileId = e.buf.pool.filenames.getOrIncl(infile)

  # the reader skips leading `(. ...)` directives, so the header stays a
  # text prefix; the TokenBuf holds the module body only
  let header = "(.nif27)\n(.vendor \"pasler\")\n(.dialect \"nim-parsed\")\n"

  # root stmts with file-anchored info
  let rootInfo = NifLineInfo(file: e.fileId, line: 1, col: 0)
  e.buf.copyInto(globalTags.registerTag("stmts"), rootInfo):
    # module pragma, mirroring the Nim renderer's lenientnils default
    e.buf.copyInto(globalTags.registerTag("pragmas"), rootInfo):
      e.buf.copyInto(globalTags.registerTag("kv"), rootInfo):
        e.buf.addIdent("feature", rootInfo)
        e.buf.addStrLit(
          if pfV2 in flags: "v2" else: "lenientnils", rootInfo)
    # standard imports mirror the Nim renderer exactly
    e.buf.copyInto(globalTags.registerTag("import"), rootInfo):
      e.buf.copyInto(globalTags.registerTag("infix"), rootInfo):
        e.buf.addIdent("/", rootInfo)
        e.buf.addIdent("std", rootInfo)
        e.buf.copyInto(globalTags.registerTag("bracket"), rootInfo):
          e.buf.addIdent("syncio", rootInfo)
          e.buf.addIdent("strutils", rootInfo)
    e.buf.copyInto(globalTags.registerTag("import"), rootInfo):
      e.buf.addIdent("systempas", rootInfo)
    for son in module.sons:
      e.emitStmt(son)

  writeNifFile(outPath, header & toString(e.buf, true))

  # the deps file mirrors the import statements in plain names; the
  # driver reads it to discover transitive dependencies
  var d = nifpools.createTokenBuf(64)
  let depsHeader = "(.nif27)\n(.vendor \"pasler\")\n(.dialect \"nim-deps\")\n"
  d.copyInto(globalTags.registerTag("stmts"), NoNifLineInfo):
    # standard imports mirror the Nim renderer exactly
    d.copyInto(globalTags.registerTag("import"), NoNifLineInfo):
      d.copyInto(globalTags.registerTag("infix"), NoNifLineInfo):
        d.addIdent("/", NoNifLineInfo)
        d.addIdent("std", NoNifLineInfo)
        d.copyInto(globalTags.registerTag("bracket"), NoNifLineInfo):
          d.addIdent("syncio", NoNifLineInfo)
          d.addIdent("strutils", NoNifLineInfo)
    d.copyInto(globalTags.registerTag("import"), NoNifLineInfo):
      d.addIdent("systempas", NoNifLineInfo)
    for son in module.sons:
      if son.kind == nkImportStmt:
        for imp in son.sons:
          d.copyInto(globalTags.registerTag("import"), NoNifLineInfo):
            emitDepsImportPath(d, imp)
  let depsPath = outPath[0 ..< outPath.len - 4] & ".deps.nif"
  writeNifFile(depsPath, depsHeader & toString(d, false))

proc emitDepsImportPath(d: var TokenBuf; n: Node) =
  ## imports for the deps file: plain idents, no line info
  case n.kind
  of nkInfix:
    d.copyInto(globalTags.registerTag("infix"), NoNifLineInfo):
      d.addIdent(n[0].strVal, NoNifLineInfo)
      emitDepsImportPath(d, n[1])
      emitDepsImportPath(d, n[2])
  of nkBracket:
    d.copyInto(globalTags.registerTag("bracket"), NoNifLineInfo):
      for son in n.sons:
        emitDepsImportPath(d, son)
  of nkIdent:
    if n.strVal.find('/') >= 0:
      let parts = n.strVal.split('/')
      d.copyInto(registerTag("infix"), NoNifLineInfo):
        d.addIdent("/", NoNifLineInfo)
        for part in parts:
          d.addIdent(part, NoNifLineInfo)
    else:
      d.addIdent(n.strVal, NoNifLineInfo)
  else:
    discard