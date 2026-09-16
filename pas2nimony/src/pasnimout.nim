#
#           pas2nimony - Pascal to Nimony translator
#
# Renders the parser's AST as Nimony-compatible Nim source.
#
# Codegen rules (all verified against the nimony compiler):
#  - `{.feature: "lenientnils".}` module pragma (nilable pointers for
#    Pascal nil semantics); `--v2` switches to `{.feature: "v2".}`
#  - `{.inheritable.}` is placed BEFORE the `=` of every class type
#  - enums use the `enum` keyword form
#  - explicit initializers for locals/globals (nimony definite assignment)
#  - every identifier goes through the symbol table's case
#    canonicalization, so it is spelled like its declaration
#  - `include`/`exclude` were already mapped to `incl`/`excl` by the
#    parser; sets are never echoed

import std/[syncio, strutils]
import pasast, passym

type
  TRendor = object
    buf: string
    indent: int
    syms: ptr SymTab
    flags: set[TParserFlag]

proc canon(s: TRendor, name: string): string =
  if s.syms != nil:
    result = s.syms[].canonical(name)
  else:
    result = name

proc line(s: var TRendor, txt: string) =
  for i in 1 .. s.indent:
    s.buf.add("  ")
  s.buf.add(txt)
  s.buf.add("\n")

proc tickName(name: string): string =
  ## backtick names that are not plain identifiers (e.g. `data2 =`)
  var needsTick = name.len == 0
  if not needsTick:
    for ch in name:
      if not ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or
              (ch >= '0' and ch <= '9') or ch == '_'):
        needsTick = true
        break
  if name.len > 0 and name[0] >= '0' and name[0] <= '9':
    needsTick = true
  if needsTick: result = "`" & name & "`"
  else: result = name

proc escStr(s: string): string =
  result = "\""
  for ch in s:
    case ch
    of '"': result.add("\\\"")
    of '\\': result.add("\\\\")
    of '\n': result.add("\\n")
    of '\r': result.add("\\r")
    of '\t': result.add("\\t")
    else:
      if ord(ch) < 32:
        result.add("\\x" & toHex(ord(ch), 2))
      else:
        result.add(ch)
  result.add("\"")

proc escChar(c: char): string =
  case c
  of '\x27': result = "'\\''"
  of '\\': result = "'\\\\'"
  else:
    if ord(c) < 32 or ord(c) > 126:
      result = "'\\x" & toHex(ord(c), 2) & "'"
    else:
      result = "'" & $c & "'"

const
  Precedences = [
    ("shl", 5), ("shr", 5), ("and", 5), ("div", 5), ("mod", 5),
    ("+", 4), ("-", 4), ("or", 4), ("xor", 4),
    ("==", 3), ("!=", 3), ("<=", 3), ("<", 3), (">=", 3), (">", 3),
    ("in", 3), ("is", 3)
  ]

proc precedenceOf(op: string): int =
  for i in 0..high(Precedences):
    if Precedences[i][0] == op: return Precedences[i][1]
  return 2

# forward declarations
proc expr(s: var TRendor, n: Node): string
proc stmt(s: var TRendor, n: Node)
proc typeStr(s: var TRendor, n: Node): string

proc isPlainExpr(n: Node): bool =
  n.kind notin {nkInfix, nkPrefix}

proc inlineLambdaBody(s: var TRendor, d: Node): string =
  ## a lambda body rendered inline: `stmt` or `(stmt1; stmt2)`
  let body = d[d.len - 1]
  if body.kind != nkStmtList:
    return s.expr(body)
  if body.len == 0:
    return "discard"
  if body.len == 1:
    return s.expr(body[0])
  result = "("
  for i in 0 ..< body.len:
    if i > 0: result.add("; ")
    result.add(s.expr(body[i]))
  result.add(")")

proc expr(s: var TRendor, n: Node): string =
  ## render `n` as an expression (single line, no side effects on indent)
  case n.kind
  of nkEmpty:
    result = ""
  of nkIdent:
    result = s.canon(n.strVal)
  of nkIntLit:
    result = $n.intVal
  of nkInt64Lit:
    result = $n.intVal & "'i64"
  of nkFloatLit:
    result = $n.floatVal
  of nkStrLit:
    result = escStr(n.strVal)
  of nkCharLit:
    result = escChar(n.strVal[0])
  of nkNilLit:
    result = "nil"
  of nkInfix:
    let op = s.canon(n[0].strVal)
    let prec = precedenceOf(op)
    # left operand: no parens at equal precedence (left assoc)
    var lhs: string
    if n[1].kind == nkInfix and precedenceOf(n[1][0].strVal) < prec:
      lhs = "(" & s.expr(n[1]) & ")"
    elif n[1].kind == nkPrefix:
      lhs = "(" & s.expr(n[1]) & ")"
    else:
      lhs = s.expr(n[1])
    var rhs: string
    if n[2].kind in {nkInfix, nkPrefix}:
      rhs = "(" & s.expr(n[2]) & ")"
    else:
      rhs = s.expr(n[2])
    result = lhs & " " & op & " " & rhs
  of nkPrefix:
    let op = s.canon(n[0].strVal)
    let arg = if n[1].kind in {nkInfix, nkPrefix}:
                "(" & s.expr(n[1]) & ")"
              else:
                s.expr(n[1])
    result = op & " " & arg
  of nkCall:
    # Delphi `PChar(x)`: a string variable passes through bare (nimony
    # accepts only string LITERALS as a cstring() conversion); pointer
    # arithmetic routes through the pasCStr shim (int<->cstring casts
    # are rejected outright)
    if n[0].kind == nkIdent and n.len == 2 and
        n[0].strVal.toLowerAscii in ["pchar", "pwidechar", "pansichar"]:
      if n[1].kind in {nkIdent, nkDotExpr}:
        result = s.expr(n[1])
      elif n[1].kind notin {nkStrLit}:
        let pc = newNode(nkCall, n.info)
        pc.add(newIdentNode("pasCStr", n.info))
        pc.add(n[1])
        result = s.expr(pc)
      else:
        result = s.expr(n[0]) & "("
        result.add(s.expr(n[1]))
        result.add(")")
    else:
      let callee = if n.noQualCallee and n[0].kind == nkIdent:
                     # inherited callees must not lose their member
                     # spelling to the canon (the corpus's `Insert`
                     # would rename to the string shim `strInsert`) -
                     # but a leading-underscore member still has to be
                     # escaped or it is not a legal nimony identifier
                     escapeNimonyName(n[0].strVal)
                   else:
                     s.expr(n[0])
      result = callee & "("
      for i in 1 ..< n.len:
        if i > 1: result.add(", ")
        result.add(s.expr(n[i]))
      result.add(")")
  of nkCommand:
    # procCall suppression of dynamic dispatch
    result = s.canon(n[0].strVal) & " " & s.expr(n[1])
  of nkDotExpr:
    let lhs = if n[0].kind in {nkInfix, nkPrefix, nkCall, nkIndexExpr,
                               nkAddr}:
                "(" & s.expr(n[0]) & ")"
              elif n[0].kind == nkDeref:
                # `x[].f` is unambiguous nim; the parens break nimony's
                # addr(x[].f) lvalue check
                s.expr(n[0])
              else:
                s.expr(n[0])
    let member = if s.syms != nil: s.syms[].canonicalMember(n[1].strVal)
                 else: n[1].strVal
    result = lhs & "." & member
  of nkIndexExpr:
    # TStrings indexed properties lower to their Delphi getter calls
    # (nimony has no indexed-property syntax on object types)
    if n[0].kind == nkDotExpr and n[0].len == 2 and n[0][1].kind == nkIdent:
      let member = n[0][1].strVal.toLowerAscii
      let getter = if member == "objects": "GetObject"
                   elif member == "strings": "Get"
                   elif member == "values": "GetValue"
                   elif member == "names": "GetName"
                   else: ""
      if getter.len > 0:
        result = s.expr(n[0][0]) & "." & getter & "(" & s.expr(n[1]) & ")"
      else:
        result = s.expr(n[0]) & "[" & s.expr(n[1]) & "]"
    else:
      result = s.expr(n[0]) & "["
      for i in 1 ..< n.len:
        if i > 1: result.add(", ")
        result.add(s.expr(n[i]))
      result.add("]")
  of nkDeref:
    result = s.expr(n[0]) & "[]"
  of nkAddr:
    if n[0].kind == nkDotExpr and n[0].len == 2 and n[0][0].kind == nkDeref:
      # @X^.F on a pointer X: nimony rejects the explicit-deref form
      # addr(x[].f) as an lvalue, but accepts the implicit-deref form
      # addr(x.f) - each dot auto-derefs one pointer level, so the
      # address is the same and the type gains back the outer level
      # (a PPHashItem result type-checks)
      let member = if s.syms != nil: s.syms[].canonicalMember(n[0][1].strVal)
                   else: n[0][1].strVal
      result = "addr(" & s.expr(n[0][0][0]) & "." & member & ")"
    else:
      result = "addr(" & s.expr(n[0]) & ")"
  of nkOconstr:
    # `TRec(Key: "", Link: nil)` - a Pascal typed record constant. Son 0
    # is the type, the rest are `kv` pairs (son 0 the literal ident "kv",
    # son 1 the field name, son 2 the value). Rendering this as a plain
    # parenthesized list would emit `(kv(Key, ""), ...)`, which is not a
    # constructor: the NIF shape is `(oconstr TY (kv NAME VAL) ...)`.
    result = s.expr(n[0]) & "("
    for i in 1 ..< n.len:
      if i > 1: result.add(", ")
      let pair = n[i]
      if pair.kind == nkCall and pair.len == 3:
        let member = if s.syms != nil: s.syms[].canonicalMember(pair[1].strVal)
                     else: pair[1].strVal
        result.add(member & ": " & s.expr(pair[2]))
      else:
        result.add(s.expr(pair))
    result.add(")")
  of nkPar:
    result = "("
    for i in 0 ..< n.len:
      if i > 0: result.add(", ")
      result.add(s.expr(n[i]))

    result.add(")")
  of nkBracket:
    result = "["
    for i in 0 ..< n.len:
      if i > 0: result.add(", ")
      result.add(s.expr(n[i]))
    result.add("]")
  of nkCurly:
    result = "{"
    for i in 0 ..< n.len:
      if i > 0: result.add(", ")
      result.add(s.expr(n[i]))
    result.add("}")
  of nkRange:
    result = s.expr(n[0]) & ".." & s.expr(n[1])
  of nkPtrTy:
    result = "ptr " & s.typeStr(n[0])
  of nkCast:
    result = "cast[" & s.expr(n[0]) & "](" & s.expr(n[1]) & ")"
  of nkAsgn:
    # assignment as an expression (inside lambda bodies)
    result = s.expr(n[0]) & " = " & s.expr(n[1])
  of nkProcDef:
    # anonymous method: proc (x: int32) = (stmts) / = stmt
    result = "proc" & s.gParams(n[2]) & " = " & s.inlineLambdaBody(n)
  else:
    result = "# unhandled expression kind: " & $n.kind

proc defName(s: var TRendor, n: Node): string =
  ## canonical spelling of a possibly exported name node
  if n.exported:
    result = s.canon(n.strVal) & "*"
  else:
    result = s.canon(n.strVal)

proc gParams(s: var TRendor, n: Node): string =
  ## render a nkFormalParams as `(a: T; b: U): R`
  result = "("
  for i in 1 ..< n.len:
    if i > 1: result.add("; ")
    let d = n[i]
    if d.kind == nkIdentDefs:
      for j in 0 ..< d.len - 2:
        if j > 0: result.add(", ")
        result.add(s.defName(d[j]))
      result.add(": " & s.typeStr(d[d.len - 2]))
      if d[d.len - 1].kind != nkEmpty:
        result.add(" = " & s.expr(d[d.len - 1]))
  result.add(")")
  if n[0].kind != nkEmpty:
    result.add(": " & s.typeStr(n[0]))

proc defaultInit(s: var TRendor, ty: Node): string =
  ## initializer satisfying nimony's definite-assignment analysis;
  ## "" when the type needs none
  case ty.kind
  of nkIdent:
    let lower = ty.strVal.toLowerAscii
    case lower
    of "uint64", "qword", "nativeuint":
      # a bare `0` is int64 in nimony and will not coerce to uint64
      result = lower & "(0)"
    of "integer", "int8", "int16", "int32", "int64", "int", "longint",
       "smallint", "byte", "word", "cardinal", "longword", "uint8",
       "uint16", "uint32", "nativeint",
       "shortint", "usize", "isize":
      result = "0"
    of "char", "ansichar", "widechar":
      result = "'\\x00'"
    of "bool", "boolean":
      result = "false"
    of "string", "widestring", "unicodestring", "shortstring",
       "tstring":
      result = "\"\""
    of "ansistring":
      result = "default(AnsiString)"
    of "single", "double", "float", "float32", "float64", "real", "extended":
      result = "0.0"
    of "tobject", "rootref", "pointer", "pchar", "pwidechar":
      result = "nil"
    else:
      # class instances are nilable; everything else gets default(T).
      # A record is registered in the same table (for its field/routine
      # lookup) but is a VALUE type, so `isRef` decides - emitting `nil`
      # for a record is a type error in nimony, not a default.
      if s.syms != nil and s.syms[].classes.hasKey(lower) and
          s.syms[].classes.getOrDefault(lower).isRef:
        result = "nil"
      else:
        result = "default(" & s.canon(ty.strVal) & ")"
  of nkRefTy, nkPtrTy, nkProcTy:
    result = "nil"
  of nkSetTy:
    result = "{}"
  of nkArrayTy:
    result = "default(array[" & s.arraySize(ty[0]) & ", " & s.typeStr(ty[1]) & "])"
  else:
    result = "default(" & s.typeStr(ty) & ")"

proc arraySize(s: var TRendor, rng: Node): string =
  ## Pascal arrays keep `array[lo..hi]` syntax but nimony's runtime
  ## index check assumes 0-based storage of length hi-lo+1
  if rng.kind == nkRange and rng.len == 2 and
      rng[0].kind in {nkIntLit, nkInt64Lit} and
      rng[1].kind in {nkIntLit, nkInt64Lit}:
    result = $(rng[1].intVal - rng[0].intVal + 1)
  else:
    result = s.expr(rng)

proc typeStr(s: var TRendor, n: Node): string =
  ## render a type node
  case n.kind
  of nkEmpty:
    result = ""
  of nkIdent:
    result = s.canon(n.strVal)
  of nkRefTy:
    if n.len > 0 and n[0].kind == nkObjectTy:
      result = s.typeStr(n[0])
    else:
      result = "ref " & s.typeStr(n[0])
  of nkPtrTy:
    result = "ptr " & s.typeStr(n[0])
  of nkDotExpr:
    # a unit-qualified alias target (`System.PLongint`): the unit
    # qualifier drops - systempas re-exports the system tier
    let member = if s.syms != nil: s.syms[].canonicalMember(n[1].strVal)
                 else: n[1].strVal
    result = member
  of nkObjectTy:
    # anonymous object
    result = "object"
  of nkEnumTy:
    result = "enum "   # rendered inline by the typedef
    for i in 0 ..< n.len:
      if i > 0: result.add(", ")
      result.add(s.expr(n[i]))
  of nkArrayTy:
    result = "array[" & s.arraySize(n[0]) & ", " & s.typeStr(n[1]) & "]"
  of nkOpenArrayTy:
    result = "openArray[" & s.typeStr(n[0]) & "]"
  of nkSeqTy:
    result = "seq[" & s.typeStr(n[0]) & "]"
  of nkSetTy:
    result = "set[" & s.typeStr(n[0]) & "]"
  of nkRangeTy:
    result = "range[" & s.expr(n[0][0]) & ".." & s.expr(n[0][1]) & "]"
  of nkIndexExpr:
    # `string[N]` (Pascal short string): the length prefix has no
    # nimony model - keep the unbounded string (documented divergence).
    # The bound must be DROPPED here: falling through to the index loop
    # below rendered it as `string# unhandled type kind: nkIntLit]`.
    if n[0].kind == nkIdent and n[0].strVal.toLowerAscii == "string":
      return "string"
    # generic instantiation in a type position
    result = s.typeStr(n[0]) & "["
    for i in 1 ..< n.len:
      if i > 1: result.add(", ")
      result.add(s.typeStr(n[i]))
    result.add("]")
  of nkProcTy:
    result = "proc ("
    let params = n[0]
    for i in 1 ..< params.len:
      if i > 1: result.add("; ")
      let d = params[i]
      if d.kind == nkIdentDefs:
        for j in 0 ..< d.len - 2:
          if j > 0: result.add(", ")
          result.add(s.defName(d[j]))
        result.add(": " & s.typeStr(d[d.len - 2]))
    result.add(")")
    if params[0].kind != nkEmpty:
      result.add(": " & s.typeStr(params[0]))
    if n.len > 1 and n[1].kind == nkPragma:
      # method-pointer shapes carry their own record conversion; a
      # plain proc type forwards the real conventions (stdcall)
      var cc = ""
      for pi in 0 ..< n[1].len:
        if n[1][pi].kind == nkIdent and
            n[1][pi].strVal.toLowerAscii in ["stdcall", "cdecl"]:
          if cc.len > 0: cc.add(", ")
          cc.add(n[1][pi].strVal)
      if cc.len > 0:
        result.add(" {." & cc & ".}")
  of nkVarTy:
    result = (if n.isOutParam: "out " else: "var ") & s.typeStr(n[0])
  of nkIntLit, nkInt64Lit, nkFloatLit:
    # a bare literal in a type position (an index bound)
    result = s.expr(n)
  else:
    result = "# unhandled type kind: " & $n.kind

proc emitBranchBody(s: var TRendor, n: Node) =
  ## render a branch body (one statement or a list) one level deeper
  s.indent = s.indent + 1
  if n.kind == nkStmtList:
    # a body of only comments/empties is an EMPTY block to nimony
    # ("nestable statement requires indentation") - emit a discard
    # first, then any comments
    var anyReal = false
    for c in n.sons:
      if c.kind != nkEmpty and c.kind != nkCommentStmt:
        anyReal = true
        break
    if not anyReal:
      s.line("discard")
    for c in n.sons:
      s.stmt(c)
  else:
    # a single-statement body that is only a comment is likewise
    # an empty block (nimony needs a real statement)
    if n.kind == nkEmpty or n.kind == nkCommentStmt:
      s.line("discard")
      if n.kind == nkCommentStmt and n.strVal.len > 0:
        s.line(n.strVal)
    else:
      s.stmt(n)
  s.indent = s.indent - 1

proc typeParamsSuffix(s: var TRendor, d: Node): string =
  ## `[K, V]` suffix for generic defs; "" when non-generic
  if d.len > 1 and d[1].kind == nkBracket and d[1].len > 0:
    result = "["
    for i in 0 ..< d[1].len:
      if i > 0: result.add(", ")
      result.add(s.canon(d[1][i].strVal))
    result.add("]")
  else:
    result = ""

proc renderDefSig(s: var TRendor, n: Node): string =
  ## `proc name(self: C; a: T): R` for a def node
  var nameNode = n[0]
  var exported = false
  if nameNode.exported:
    exported = true
  let kw = case n.kind
           of nkProcDef: "proc"
           of nkFuncDef: "func"
           of nkMethodDef: "method"
           else: "template"
  # member names must not take the type-tier RTL map: a Pascal method
  # named `Move` is not the `pasMove` memory shim. `defClass` marks
  # a method definition, so render it with member canon.
  let nm = if n.defClass.len > 0 and s.syms != nil:
             s.syms[].canonicalMember(nameNode.strVal)
           else:
             s.canon(nameNode.strVal)
  result = kw & " " & tickName(nm)
  result.add(s.typeParamsSuffix(n))
  if exported: result.add("*")
  if n.len >= 3 and n[2].kind == nkFormalParams:
    result.add(s.gParams(n[2]))

proc containsRaise(n: Node): bool =
  if n.kind == nkRaiseStmt: return true
  for i in 0 ..< n.len:
    if containsRaise(n.sons[i]): return true
  return false

proc renderDef(s: var TRendor, n: Node) =
  ## render a full routine/template definition with body
  let body = n[n.len - 1]
  if body.kind == nkEmpty:
    # bodiless (forward) declaration
    s.line(renderDefSig(s, n))
    return
  var sig = renderDefSig(s, n)
  # forward accepted routine pragmas (inline, cdecl, stdcall) and the
  # `raises` decision (the M4 machinery pre-stuffs it into the pragma
  # node, containsRaise owns it). nimony's front end rejects two ADJACENT
  # pragma blocks on one definition - `proc E() {.closure.} {.raises.} =`
  # dies with "invalid indentation" - so collect every pragma and emit a
  # single `{.a, b.}` block. (Masks.pas, VHelper.pas.)
  var prags: seq[string] = @[]
  if n.len >= 4 and n[3].kind == nkPragma:
    for pi in 0 ..< n[3].len:
      if n[3][pi].kind == nkIdent and n[3][pi].strVal != "raises":
        prags.add(n[3][pi].strVal)
  if containsRaise(body):
    # nimony: raising procs must announce it (ErrorCode model)
    prags.add("raises")
  if prags.len > 0:
    sig.add(" {." & prags.join(", ") & ".}")
  s.line(sig & " =")
  emitBranchBody(s, body)

proc renderTypeDef(s: var TRendor, def: Node, hoisted: var seq[Node]) =
  ## one nkTypeDef with object/class/enum handling
  var nameNode = def[0]
  var exported = false
  if nameNode.exported:
    exported = true
  let ty = def[2]
  if ty.kind == nkRefTy and ty[0].kind == nkObjectTy:
    # class: `Name {.inheritable.} = ref object of Parent`
    var line1 = s.canon(nameNode.strVal)
    line1.add(s.typeParamsSuffix(def))
    if exported: line1.add("*")
    if s.typeParamsSuffix(def).len > 0:
      # nimony rejects {.inheritable.} on generic object types
      line1.add(" = ref object")
    else:
      line1.add(" {.inheritable.} = ref object")
    let obj = ty[0]
    if obj[0].kind == nkOfInherit:
      line1.add(" of " & s.typeStr(obj[0][0]))
    s.line(line1)
    # body: fields; member decls are hoisted
    let body = obj[1]
    s.indent = s.indent + 1
    for field in body.sons:
      if field.kind in {nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef}:
        hoisted.add(field)
      elif field.kind == nkIdentDefs:
        var names = ""
        for j in 0 ..< field.len - 2:
          if j > 0: names.add(", ")
          names.add(s.defName(field[j]))
        s.line(names & ": " & s.typeStr(field[field.len - 2]))
      elif field.kind == nkRecCase:
        s.stmt(field)
      elif field.kind == nkCommentStmt:
        s.line(field.strVal)
    s.indent = s.indent - 1
  elif ty.kind == nkObjectTy:
    # plain object/record
    var line1 = s.canon(nameNode.strVal)
    line1.add(s.typeParamsSuffix(def))
    if exported: line1.add("*")
    var hasParent = false
    if ty[0].kind == nkOfInherit:
      line1.add(" = object of " & s.typeStr(ty[0][0]))
      hasParent = true
    elif not ty.isRecordType:
      # objects can be inherited from in Pascal
      line1.add(" {.inheritable.} = object")
    else:
      line1.add(" = object")
    s.line(line1)
    let body = ty[1]
    s.indent = s.indent + 1
    for field in body.sons:
      if field.kind in {nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef}:
        hoisted.add(field)
      elif field.kind == nkIdentDefs:
        var names = ""
        for j in 0 ..< field.len - 2:
          if j > 0: names.add(", ")
          names.add(s.defName(field[j]))
        s.line(names & ": " & s.typeStr(field[field.len - 2]))
      elif field.kind == nkRecCase:
        s.stmt(field)
      elif field.kind == nkCommentStmt:
        s.line(field.strVal)
    s.indent = s.indent - 1
  elif ty.kind == nkEnumTy:
    var line1 = s.canon(nameNode.strVal)
    line1.add(s.typeParamsSuffix(def))
    if exported: line1.add("*")
    line1.add(" = enum ")
    for i in 0 ..< ty.len:
      if i > 0: line1.add(", ")
      if ty[i].kind == nkEnumFieldDef:
        line1.add(s.canon(ty[i][0].strVal) & " = " & s.expr(ty[i][1]))
      else:
        line1.add(s.defName(ty[i]))
    s.line(line1)
  else:
    if ty.kind == nkSetTy and ty[0].kind == nkEnumTy:
      # `TMaskDirectives = set of (mdReverseDir, ...)` - the anonymous
      # enum hoists to its own named type so the set has an ordinal
      # base (nimony has no anonymous enum in a set position)
      let enumName = "pas_" & s.canon(nameNode.strVal) & "Ty"
      var line1 = enumName
      if exported: line1.add("*")
      line1.add(" = enum ")
      let ety = ty[0]
      for i in 0 ..< ety.len:
        if i > 0: line1.add(", ")
        if ety[i].kind == nkEnumFieldDef:
          line1.add(s.canon(ety[i][0].strVal) & " = " & s.expr(ety[i][1]))
        else:
          # members travel with the exported enum type (no per-member
          # `*` - nimony rejects it on enum fields)
          line1.add(s.canon(ety[i].strVal))
      s.line(line1)
      var line2 = s.canon(nameNode.strVal)
      if exported: line2.add("*")
      line2.add(" = set[" & enumName & "]")
      s.line(line2)
    else:
      var line1 = s.canon(nameNode.strVal)
      if exported: line1.add("*")
      line1.add(" = " & s.typeStr(ty))
      s.line(line1)

proc stmt(s: var TRendor, n: Node) =
  ## render one statement at the current indent
  case n.kind
  of nkEmpty:
    discard
  of nkStmtList:
    for c in n.sons:
      s.stmt(c)
  of nkCommentStmt:
    if n.strVal.len > 0:
      s.line(n.strVal)
  of nkAsgn:
    # TStrings indexed-property assignment lowers to the setter call
    # (nimony has no indexed-property write syntax on object types)
    if n[0].kind == nkIndexExpr and n[0].len == 2 and
        n[0][0].kind == nkDotExpr and n[0][0].len == 2 and
        n[0][0][1].kind == nkIdent:
      let member = n[0][0][1].strVal.toLowerAscii
      let setter = if member == "objects": "PutObject"
                   elif member == "strings": "Put"
                   else: ""
      if setter.len > 0:
        s.line(s.expr(n[0][0][0]) & "." & setter & "(" & s.expr(n[0][1]) &
            ", " & s.expr(n[1]) & ")")
        return
    if n[1].kind == nkProcDef and n[1][0].kind == nkEmpty:
      # anonymous method assignment: `F = proc (x: int32) =`
      # followed by the indented body
      s.line(s.expr(n[0]) & " = proc" & s.gParams(n[1][2]) & " =")
      s.indent = s.indent + 1
      let body = n[1][n[1].len - 1]
      for st in body.sons:
        s.stmt(st)
      s.indent = s.indent - 1
    else:
      s.line(s.expr(n[0]) & " = " & s.expr(n[1]))
  of nkIfStmt:
    var first = true
    for branch in n.sons:
      if branch.kind == nkElifBranch:
        if first:
          s.line("if " & s.expr(branch[0]) & ":")
          first = false
        else:
          s.line("elif " & s.expr(branch[0]) & ":")
        emitBranchBody(s, branch[1])
      elif branch.kind == nkElse:
        s.line("else:")
        emitBranchBody(s, branch[0])
  of nkWhileStmt:
    s.line("while " & s.expr(n[0]) & ":")
    emitBranchBody(s, n[1])
  of nkForStmt:
    if n[1].kind == nkCall and n[1].len >= 3 and
        n[1][0].kind == nkIdent and
        n[1][0].strVal.toLowerAscii in ["countup", "countdown"]:
      s.line("for " & s.canon(n[0].strVal) & " in " & s.expr(n[1]) & ":")
    else:
      s.line("for " & s.canon(n[0].strVal) & " in " & s.expr(n[1]) & ":")
    emitBranchBody(s, n[2])
  of nkCaseStmt:
    s.line("case " & s.expr(n[0]))
    var hasElse = false
    for i in 1 ..< n.len:
      let b = n[i]
      if b.kind == nkOfBranch:
        var labels = ""
        for j in 0 ..< b.len - 1:
          if j > 0: labels.add(", ")
          labels.add(s.expr(b[j]))
        s.line("of " & labels & ":")
        emitBranchBody(s, b[b.len - 1])
      elif b.kind == nkElse:
        hasElse = true
        s.line("else:")
        emitBranchBody(s, b[0])
    if not hasElse:
      s.line("else:")
      s.indent = s.indent + 1
      s.line("discard")
      s.indent = s.indent - 1
  of nkTryStmt:
    s.line("try:")
    emitBranchBody(s, n[0])
    for i in 1 ..< n.len:
      let b = n[i]
      if b.kind == nkExceptBranch:
        if b[0].kind == nkEmpty:
          s.line("except:")
        else:
          s.line("except " & s.canon(b[0].strVal) & " as " &
                 s.canon(b[1].strVal) & ":")
        emitBranchBody(s, b[2])
      elif b.kind == nkFinally:
        s.line("finally:")
        # nkFinally holds the body statements directly (flattened)
        s.indent = s.indent + 1
        if b.len == 0:
          s.line("discard")
        else:
          for c in b.sons:
            s.stmt(c)
        s.indent = s.indent - 1
  of nkReturnStmt:
    if n[0].kind == nkEmpty:
      s.line("return")
    else:
      s.line("return " & s.expr(n[0]))
  of nkBreakStmt:
    if n.len > 0 and n[0].kind == nkIdent:
      s.line("break " & s.expr(n[0]))
    else:
      s.line("break")
  of nkBlockStmt:
    s.line("block " & s.expr(n[0]) & ":")
    emitBranchBody(s, n[1])
  of nkContinueStmt:
    s.line("continue")
  of nkRaiseStmt:
    if n[0].kind == nkEmpty:
      s.line("raise")
    else:
      s.line("raise " & s.expr(n[0]))
  of nkDiscardStmt:
    s.line("discard " & s.expr(n[0]))
  of nkVarSection:
    for d in n.sons:
      if d.kind != nkIdentDefs: continue
      let tyStr = s.typeStr(d[d.len - 2])
      let initStr = if d[d.len - 1].kind != nkEmpty:
                      s.expr(d[d.len - 1])
                    elif s.flags.contains(pfNoInit):
                      ""
                    else:
                      s.defaultInit(d[d.len - 2])
      for j in 0 ..< d.len - 2:
        var ln = "var " & tickName(s.canon(d[j].strVal))
        if d[j].exported: ln.add("*")
        if tyStr.len > 0: ln.add(": " & tyStr)
        if initStr.len > 0: ln.add(" = " & initStr)
        s.line(ln)
    # an `absolute` alias is a template, and `template` may not sit
    # inside a `var` block: it follows the block at the same level
    for d in n.sons:
      if d.kind == nkTemplateDef: s.stmt(d)
  of nkConstSection:
    for d in n.sons:
      if d.kind != nkIdentDefs: continue
      let name = s.canon(d[0].strVal)
      # interface consts/resourcestrings carry the export marker (the
      # nkVarSection rule): importing units read the name
      let star = if d[0].exported: "*" else: ""
      let tyStr = s.typeStr(d[1])
      # Pascal `const X: AnsiString = 'lit'`: a read-only literal in static
      # storage. refCnt = -1 marks it so the first write detaches. A nimony
      # `const` of the AnsiString object hits a C-codegen bug (the
      # toAnsiStringLit index becomes an undeclared symbol), so the backing is
      # a ConstAnsiLit const with a module-level `let` view beside it.
      if tyStr.toLowerAscii == "ansistring" and d[2].kind in {nkStrLit, nkCharLit}:
        let lit = d[2].strVal
        let n = lit.len
        var buf = "["
        for i in 0 ..< n:
          if i > 0: buf.add(", ")
          buf.add("char(" & $uint8(lit[i]) & ")")
        if n > 0: buf.add(", ")
        buf.add("char(0)]")
        let litName = "pasLit_" & name
        s.line("const " & tickName(litName) & star & " = ConstAnsiLit[" & $n &
            "](rec: StrRec(refCnt: -1'i32, length: " & $n & "'i32), buf: " & buf & ")")
        s.line("let " & tickName(name) & star & ": AnsiString = toAnsiStringLit(" &
            tickName(litName) & ")")
        continue
      let valStr = s.expr(d[2])
      if tyStr.len > 0:
        s.line("const " & tickName(name) & star & ": " & tyStr & " = " & valStr)
      else:
        s.line("const " & tickName(name) & star & " = " & valStr)
  of nkTypeSection:
    # a routine-LOCAL type section: Nim allows `type` inside a proc, so
    # render the definitions in place. This case used to be a bare
    # discard, which silently dropped e.g. tplbtree.inc's local `RPath`
    # and left every use of it undeclared. Module-level sections are
    # rendered by renderModule, not here.
    var anyDef = false
    for def in n.sons:
      if def.kind == nkTypeDef and def[2].kind != nkCommentStmt:
        anyDef = true
        break
    if anyDef:
      s.line("type")
      s.indent = s.indent + 1
      var lhoisted: seq[Node] = @[]
      for def in n.sons:
        if def.kind != nkTypeDef: continue
        s.renderTypeDef(def, lhoisted)
      s.indent = s.indent - 1
      for h in lhoisted:
        s.stmt(h)
  of nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef:
    # nested definitions render in place; class-body defs were hoisted
    if n[n.len - 1].kind == nkEmpty:
      # bodiless (forward) declaration
      s.line(renderDefSig(s, n))
    else:
      renderDef(s, n)
  of nkWhenExpr:
    # a `{$if <expr>}` group forwarded to Nim: `when C: ... elif C2: ...
    # else: ...`. Every branch is rendered, so any number of {$elseif}
    # arms survives; a `{$ifdef}` evaluated at parse time produces the
    # same node with a single `elif` and an optional `else`.
    for i in 0 ..< n.len:
      let branch = n[i]
      if branch.kind == nkElifBranch:
        s.line((if i == 0: "when " else: "elif ") & s.expr(branch[0]) & ":")
        emitBranchBody(s, branch[1])
      elif branch.kind == nkElse:
        s.line("else:")
        emitBranchBody(s, branch[0])
  of nkImportStmt:
    for u in n.sons:
      if u.kind == nkIdent:
        var unit = u.strVal
        unit = unit.replace(" ", "")
        s.line("import " & unit)
      elif u.kind == nkCommentStmt:
        s.line(u.strVal)
      else:
        # a forwarded `{$if <expr>}` among the units: a guarded import
        s.stmt(u)
  of nkRecCase:
    # `case tag: Type of` inside an object body
    let disc = n[0]
    s.line("case " & s.canon(disc[0].strVal) & ": " &
           s.typeStr(disc[1]))
    var hasElse = false
    for i in 1 ..< n.len:
      let b = n[i]
      if b.kind == nkOfBranch:
        var labels = ""
        for j in 0 ..< b.len - 1:
          if j > 0: labels.add(", ")
          labels.add(s.expr(b[j]))
        s.line("of " & labels & ":")
        emitBranchBody(s, b[b.len - 1])
      elif b.kind == nkElse:
        hasElse = true
        s.line("else:")
        emitBranchBody(s, b[0])
    if not hasElse:
      s.line("else:")
      s.indent = s.indent + 1
      s.line("discard")
      s.indent = s.indent - 1
  of nkRecList:
    for c in n.sons:
      s.stmt(c)
  else:
    # fallback: render as expression statement
    s.line(s.expr(n))

proc typeBlock(s: var TRendor, defs: seq[Node], hoisted: var seq[Node]) =
  ## emit `defs` as one `type` section. A section holding only comments
  ## (metaclass and forward-decl placeholders) emits nothing at all.
  var anyDef = false
  for def in defs:
    if def.kind == nkTypeDef and def[2].kind != nkCommentStmt:
      anyDef = true
      break
  if not anyDef: return
  s.line("type")
  s.indent = s.indent + 1
  for def in defs:
    if def.kind != nkTypeDef: continue
    s.renderTypeDef(def, hoisted)
  s.indent = s.indent - 1

proc renderTypeSectionBody(s: var TRendor, defs: seq[Node],
                           hoisted: var seq[Node]) =
  ## emit the body of one `type` section. A forwarded `{$if <expr>}`
  ## group may sit between the definitions, and nimony rejects `when`
  ## INSIDE a `type` section - so each run of definitions becomes its own
  ## `type` block and the conditional is hoisted between them, its arm
  ## bodies rendered as nested type-section bodies:
  ##
  ##   when sizeof(pointer) == 8:
  ##     type
  ##       IntPtr = int64
  ##   else:
  ##     type
  ##       IntPtr = int32
  ##
  ## The recursion matters: an arm may itself hold a conditional. Calling
  ## the flat `typeBlock` there silently DROPPED the inner group (the
  ## `{$IF not defined(IntPtr)} {$IF sizeof(Pointer) = 4} ...` shape in
  ## p4nHelper.pas). Self-recursion keeps that reachable without a
  ## forward declaration, which this toolchain does not have.
  var run: seq[Node] = @[]
  for def in defs:
    if def.kind != nkWhenExpr:
      run.add(def)
      continue
    typeBlock(s, run, hoisted)
    run.setLen(0)
    for i in 0 ..< def.len:
      let branch = def[i]
      if branch.kind == nkElifBranch:
        s.line((if i == 0: "when " else: "elif ") & s.expr(branch[0]) & ":")
        s.indent = s.indent + 1
        # an arm may render NOTHING: its only content can be a `{$Message}`
        # diagnostic (no semantics to emit) or a type body with no real
        # definition left. An empty arm is "nestable statement requires
        # indentation" to nimony, so fall back to a discard.
        let before = s.buf.len
        if branch[1].kind == nkTypeSection:
          renderTypeSectionBody(s, branch[1].sons, hoisted)
        else:
          s.stmt(branch[1])
        if s.buf.len == before:
          s.line("discard")
        s.indent = s.indent - 1
      elif branch.kind == nkElse:
        s.line("else:")
        s.indent = s.indent + 1
        let before = s.buf.len
        if branch[0].kind == nkTypeSection:
          renderTypeSectionBody(s, branch[0].sons, hoisted)
        else:
          s.stmt(branch[0])
        if s.buf.len == before:
          s.line("discard")
        s.indent = s.indent - 1
  typeBlock(s, run, hoisted)

proc renderModule*(module: Node, syms: var SymTab, infile, outfile: string,
                   flags: set[TParserFlag]) =
  var s = TRendor(buf: "", indent: 0, syms: addr(syms), flags: flags)
  s.buf.add("# Generated by pas2nimony from " & infile & "\n")
  if flags.contains(pfV2):
    s.line("{.feature: \"v2\".}")
  else:
    s.line("{.feature: \"lenientnils\".}")
  s.line("import std/[syncio, strutils]")
  s.line("import systempas")
  s.buf.add("\n")
  for top in module.sons:
    case top.kind
    of nkTypeSection:
      # a forwarded `{$if <expr>}` group may sit between the definitions,
      # at any nesting depth. Each run of definitions is emitted as its
      # own `type` section and the conditional is hoisted between them
      # (see renderTypeSectionBody / whenInTypeSection).
      var allHoisted: seq[Node] = @[]
      renderTypeSectionBody(s, top.sons, allHoisted)
      s.buf.add("\n")
      for h in allHoisted:
        s.renderDef(h)
        s.buf.add("\n")
    of nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef:
      s.renderDef(top)
      s.buf.add("\n")
    else:
      s.stmt(top)
  try:
    writeFile(outfile, s.buf)
  except ErrorCode as e:
    write(stderr, "cannot write file: " & outfile & "\n")
    quit(1)