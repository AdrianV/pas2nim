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
    result = s.expr(n[0]) & "("
    for i in 1 ..< n.len:
      if i > 1: result.add(", ")
      result.add(s.expr(n[i]))
    result.add(")")
  of nkCommand:
    # procCall suppression of dynamic dispatch
    result = s.canon(n[0].strVal) & " " & s.expr(n[1])
  of nkDotExpr:
    let lhs = if n[0].kind in {nkInfix, nkPrefix, nkCall, nkIndexExpr,
                               nkDeref, nkAddr}:
                "(" & s.expr(n[0]) & ")"
              else:
                s.expr(n[0])
    let member = if s.syms != nil: s.syms[].canonicalMember(n[1].strVal)
                 else: n[1].strVal
    result = lhs & "." & member
  of nkIndexExpr:
    result = s.expr(n[0]) & "["
    for i in 1 ..< n.len:
      if i > 1: result.add(", ")
      result.add(s.expr(n[i]))
    result.add("]")
  of nkDeref:
    result = s.expr(n[0]) & "[]"
  of nkAddr:
    result = "addr(" & s.expr(n[0]) & ")"
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
    of "integer", "int8", "int16", "int32", "int64", "int", "longint",
       "smallint", "byte", "word", "cardinal", "longword", "uint8",
       "uint16", "uint32", "uint64", "nativeint", "nativeuint", "qword",
       "shortint", "usize", "isize":
      result = "0"
    of "char", "ansichar", "widechar":
      result = "'\\x00'"
    of "bool", "boolean":
      result = "false"
    of "string", "ansistring", "widestring", "unicodestring", "shortstring",
       "tstring":
      result = "\"\""
    of "single", "double", "float", "float32", "float64", "real", "extended":
      result = "0.0"
    of "tobject", "rootref", "pointer", "pchar", "pwidechar":
      result = "nil"
    else:
      # class instances are nilable; everything else gets default(T)
      if s.syms != nil and s.syms[].classes.hasKey(lower):
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
      result.add(" {.closure.}")
  of nkVarTy:
    result = "var " & s.typeStr(n[0])
  else:
    result = "# unhandled type kind: " & $n.kind

proc emitBranchBody(s: var TRendor, n: Node) =
  ## render a branch body (one statement or a list) one level deeper
  s.indent = s.indent + 1
  if n.kind == nkStmtList:
    if n.len == 0:
      s.line("discard")
    else:
      for c in n.sons:
        s.stmt(c)
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
  result = kw & " " & tickName(s.canon(nameNode.strVal))
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
  if containsRaise(body):
    # nimony: raising procs must announce it (ErrorCode model)
    sig.add(" {.raises.}")
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
  of nkConstSection:
    for d in n.sons:
      if d.kind != nkIdentDefs: continue
      let name = s.canon(d[0].strVal)
      let tyStr = s.typeStr(d[1])
      let valStr = s.expr(d[2])
      if tyStr.len > 0:
        s.line("const " & tickName(name) & ": " & tyStr & " = " & valStr)
      else:
        s.line("const " & tickName(name) & " = " & valStr)
  of nkTypeSection:
    discard
  of nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef:
    # nested definitions render in place; class-body defs were hoisted
    if n[n.len - 1].kind == nkEmpty:
      # bodiless (forward) declaration
      s.line(renderDefSig(s, n))
    else:
      renderDef(s, n)
  of nkWhenExpr:
    # {$ifdef X} ... {$else} ... {$endif}
    let branch = n[0]
    s.line("when " & s.expr(branch[0]) & ":")
    emitBranchBody(s, branch[1])
    if n.len > 1 and n[1].kind == nkElse:
      s.line("else:")
      emitBranchBody(s, n[1][0])
  of nkImportStmt:
    for u in n.sons:
      if u.kind == nkIdent:
        var unit = u.strVal
        unit = unit.replace(" ", "")
        s.line("import " & unit)
      elif u.kind == nkCommentStmt:
        s.line(u.strVal)
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
      var allHoisted: seq[Node] = @[]
      var anyDef = false
      for def in top.sons:
        if def.kind == nkTypeDef and def[2].kind != nkCommentStmt:
          anyDef = true
          break
      if anyDef:
        s.line("type")
        s.indent = s.indent + 1
      for def in top.sons:
        if def.kind != nkTypeDef: continue
        var hoisted: seq[Node] = @[]
        s.renderTypeDef(def, hoisted)
        for h in hoisted: allHoisted.add(h)
      if anyDef:
        s.indent = s.indent - 1
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