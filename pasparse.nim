#
#
#      Pas2nim - Pascal to Nim source converter
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module implements the parser of the Pascal variant Nim is written in.
# It transfers a Pascal module into a Nimf AST. Then the renderer can be
# used to convert the AST to its text representation.

import 
  os, llstream, paslex, idents, strutils, ast, astalgo, msgs, options, 
  deques, tables, renderer, parsetools, lineinfos

proc getIdent(s: string): PIdent = getIdent(identCache, s)
template emptyNode: untyped = newNode(nkEmpty)

type 
  TSection = enum 
    seImplementation, seInterface
  TContext = enum 
    conExpr, conStmt, conTypeDesc
  TVisibilty = enum
    visPublic, visPrivate, visProtected, visPublished
  TParserFlag* = enum
    pfRefs,             ## use "ref" instead of "ptr" for Pascal's ^typ
    pfMoreReplacements, ## use more than the default replacements
    pfImportBlackList   ## use import blacklist
  TExtraInfo = enum
    eiConstructor, eiDestructor, eiPublic
  TExtraStmt = ref object
    node: PNode
    flags: set[TExtraInfo]
  TObjectInfo = ref object
    isRefTy: bool
  TParser*{.final.} = object
    section: TSection
    inParamList: bool
    context: TContext     # needed for the @emit command
    lastVarSection: PNode
    lex: TLexer
    tok: TToken
    repl: TIdTable           # replacements
    flags: set[TParserFlag]
    ahead: Deque[TToken]
    selfClass: PIdent
    visibility: TVisibilty
    extra: seq[TExtraStmt]
    classes: Table[int, TObjectInfo]
    outerProc: PNode
    methods: Table[int, bool]
    keepOverride: bool

  TReplaceTuple* = array[0..1, string]

const 
  ImportBlackList*: array[1..3, string] = ["nsystem", "sysutils", "charsets"]
  stdReplacements*: seq[TReplaceTuple] = @[["include", "incl"], 
    ["exclude", "excl"], ["pchar", "cstring"], ["assignfile", "open"], 
    ["integer", "int"], ["longword", "int32"], ["cardinal", "int"], 
    ["boolean", "bool"], ["shortint", "int8"], ["smallint", "int16"], 
    ["longint", "int32"], ["byte", "int8"], ["word", "int16"], 
    ["single", "float32"], ["double", "float64"], ["real", "float"], 
    ["length", "len"], ["len", "length"], ["setlength", "setlen"],
    ["TObject", "RootRef"], ["true", "true"], ["false", "false"], ["string", "string"],
    ["result", "result"]]
  nimReplacements*: array[1..35, TReplaceTuple] = [["nimread", "read"], 
    ["nimwrite", "write"], ["nimclosefile", "close"], ["closefile", "close"], 
    ["openfile", "open"], ["nsystem", "system"], ["ntime", "times"], 
    ["nos", "os"], ["nmath", "math"], ["ncopy", "copy"], ["addChar", "add"], 
    ["halt", "quit"], ["nobject", "TObject"], ["eof", "EndOfFile"], 
    ["input", "stdin"], ["output", "stdout"], ["addu", "`+%`"], 
    ["subu", "`-%`"], ["mulu", "`*%`"], ["divu", "`/%`"], ["modu", "`%%`"], 
    ["ltu", "`<%`"], ["leu", "`<=%`"], ["shlu", "`shl`"], ["shru", "`shr`"], 
    ["assigned", "not isNil"], ["eintoverflow", "EOverflow"], ["format", "`%`"], 
    ["snil", "nil"], ["tostringf", "$"], ["ttextfile", "tfile"], 
    ["tbinaryfile", "tfile"], ["strstart", "0"], ["nl", "\"\\n\""],
    ["tostring", "$"]]

proc parseUnit*(p: var TParser): PNode
proc openParser*(p: var TParser, filename: string, inputStream: PLLStream,
                 flags: set[TParserFlag] = {})
proc closeParser*(p: var TParser)
proc exSymbol*(n: var PNode)
proc fixRecordDef*(n: var PNode)
proc parseRoutine(p: var TParser; noBody: bool): PNode

  # XXX: move these two to an auxiliary module

# implementation

proc idTableGet(t: TIdTable, key: PIdent): RootRef =
  when false:
    var first {.global.} = true
    if first:
      echo t.repr
      first = false
  let s = key.s.toLower
  var key = key
  var next = key.next
  while next != nil:
    if next.s.toLower == s :
      key = next
      break
    next = next.next
    # echo next.repr
  # echo key.repr
  let res = astalgo.idTableGet(t, key)
  # echo res.repr
  res

proc add(father, child: PNode): PNode =
  result = father
  result.addSon(child)

proc add(father: PNode; childs: varargs[PNode]): PNode =
  result = father
  for child in childs:
    result.addSon(child)
  
proc openParser(p: var TParser, filename: string, 
                inputStream: PLLStream, flags: set[TParserFlag] = {}) = 
  openLexer(p.lex, filename, inputStream)
  initIdTable(p.repl)
  for r in stdReplacements: 
    idTablePut(p.repl, getIdent(identCache, r[0]), 
               getIdent(identCache, r[1]))
  if pfMoreReplacements in flags: 
    for i in countup(low(nimReplacements), high(nimReplacements)): 
      idTablePut(p.repl, getIdent(identCache, nimReplacements[i][0]), 
                 getIdent(identCache, nimReplacements[i][1]))
  p.flags = flags
  p.ahead = initDeque[TToken]()
  p.classes = initTable[int, TObjectInfo]()
  p.methods = initTable[int, bool]()

proc closeParser(p: var TParser) = closeLexer(p.lex)
proc getTok(p: var TParser) = 
  if p.ahead.len > 0 :
    p.tok = p.ahead.popFirst
  else:
    getTok(p.lex, p.tok)

proc peekTok(p: var TParser): TToken =
  if p.ahead.len == 0:
    var next: TToken
    getTok(p.lex, next)
    p.ahead.addLast(next)
  result = p.ahead.peekFirst

proc removeNextTok(p: var TParser) =
  discard p.ahead.popFirst

proc parMessage(p: TParser, msg: TMsgKind, arg = "") = 
  lexMessage(p.lex, msg, arg)

proc parLineInfo(p: TParser): TLineInfo = 
  result = getLineInfo(p.lex)

proc skipCom(p: var TParser, n: PNode) = 
  while p.tok.xkind == pxComment: 
    if (n != nil): 
      if n.comment == "": n.comment = p.tok.literal
      else: add(n.comment, "\n" & p.tok.literal)
    else: 
      parMessage(p, warnCommentXIgnored, p.tok.literal)
    getTok(p)

proc expectIdent(p: TParser) = 
  if p.tok.xkind != pxSymbol: 
    lexMessage(p.lex, errXExpected, $(p.tok))
  
proc eat(p: var TParser, xkind: TTokKind) = 
  if p.tok.xkind == xkind: getTok(p)
  else: lexMessage(p.lex, errXExpected, tokKindToStr(xkind))
  
proc opt(p: var TParser, xkind: TTokKind) = 
  if p.tok.xkind == xkind: getTok(p)
  
proc newNodeP(kind: TNodeKind, p: TParser): PNode = 
  result = newNodeI(kind, getLineInfo(p.lex))

proc newIntNodeP(kind: TNodeKind, intVal: BiggestInt, p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.intVal = intVal

proc newFloatNodeP(kind: TNodeKind, floatVal: BiggestFloat, 
                   p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.floatVal = floatVal

proc newStrNodeP(kind: TNodeKind, strVal: string, p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.strVal = strVal

proc newIdentNodeP(ident: PIdent, p: TParser): PNode = 
  result = newNodeP(nkIdent, p)
  result.ident = ident

proc newIdentNameNodeP(name: string, p: TParser): PNode = 
  result = newNodeP(nkIdent, p)
  result.ident = getIdent(identCache, name)

proc createIdentNodeP(ident: PIdent, p: TParser): PNode = 
  result = newNodeP(nkIdent, p)
  var x = PIdent(idTableGet(p.repl, ident))
  if x != nil: result.ident = x
  else: result.ident = ident

proc getIdentOfNode(n: PNode): PIdent =
  if n.kind in {nkTypeDef,nkTemplateDef,nkProcDef,nkMethodDef} :
    if n.sons[0].kind == nkIdent: return n.sons[0].ident
    elif n.sons[0].kind == nkPostfix and n.sons[0].len >= 2 and
      n.sons[0][1].kind == nkIdent: return n.sons[0][1].ident
  elif n.kind == nkIdent: return n.ident

proc genSignature(n: PNode): int = 
  if n.kind in {nkMethodDef, nkProcDef, nkTemplateDef}:
    let ident = getIdentOfNode(n)
    result = ident.id
    let params = n[3]
    for p in params.sons:
      if p.kind == nkIdentDefs:
        let ty = p[1]
        if ty.kind == nkIdent:
          result = result * 223 + ty.ident.id

proc pushExtraStmt(p: var TParser, s: TExtraStmt) =
  if p.extra.len == 0: 
    # p.extra = @[]
    var n = newNodeP(nkPragma, p).add(newNodeP(nkExprColonExpr, p).add(
      newIdentNameNodeP("this", p),
      newIdentNameNodeP("self", p)))
    p.extra.add(TExtraStmt(node: n, flags: {}))
  p.extra.add(s)
  
proc parseExpr(p: var TParser): PNode
proc parseStmt(p: var TParser): PNode
proc parseTypeDesc(p: var TParser, definition: var PNode): PNode
proc parseTypeDesc(p: var TParser): PNode =
  var empty: PNode = nil
  return parseTypeDesc(p, empty)
proc parseTypeDesc(p: var TParser, definition: PNode): PNode =
  var definition = definition
  return parseTypeDesc(p, definition)
  

proc parseEmit(p: var TParser, definition: PNode): PNode = 
  getTok(p)                   # skip 'emit'
  result = emptyNode
  if p.tok.xkind != pxCurlyDirRi: 
    case p.context
    of conExpr: 
      result = parseExpr(p)
    of conStmt: 
      result = parseStmt(p)
      if p.tok.xkind != pxCurlyDirRi: 
        var a = result
        result = newNodeP(nkStmtList, p)
        addSon(result, a)
        while p.tok.xkind != pxCurlyDirRi: 
          addSon(result, parseStmt(p))
    of conTypeDesc: 
      result = parseTypeDesc(p, definition)
  eat(p, pxCurlyDirRi)

proc parseCommand(p: var TParser, definition: PNode = nil): PNode = 
  result = emptyNode
  getTok(p)
  if p.tok.ident.id == getIdent(identCache, "discard").id: 
    result = newNodeP(nkDiscardStmt, p)
    getTok(p)
    eat(p, pxCurlyDirRi)
    addSon(result, parseExpr(p))
  elif p.tok.ident.id == getIdent(identCache, "set").id: 
    getTok(p)
    eat(p, pxCurlyDirRi)
    result = parseExpr(p)
    if result.kind == nkEmpty: internalError(gConfig, "emptyNode modified")
    result.kind = nkCurly
  elif p.tok.ident.id == getIdent(identCache, "cast").id: 
    getTok(p)
    eat(p, pxCurlyDirRi)
    var a = parseExpr(p)
    if (a.kind == nkCall) and (sonsLen(a) == 2): 
      result = newNodeP(nkCast, p)
      addSon(result, a.sons[0])
      addSon(result, a.sons[1])
    else: 
      parMessage(p, errInvalidDirectiveX, $p.tok)
      result = a
  elif p.tok.ident.id == getIdent(identCache, "emit").id: 
    result = parseEmit(p, definition)
  elif p.tok.ident.id == getIdent(identCache, "ignore").id: 
    getTok(p)
    eat(p, pxCurlyDirRi)
    while true: 
      case p.tok.xkind
      of pxEof: 
        parMessage(p, errXExpected, "{@emit}")
      of pxCommand: 
        getTok(p)
        if p.tok.ident.id == getIdent(identCache, "emit").id: 
          result = parseEmit(p, definition)
          break 
        else: 
          while (p.tok.xkind != pxCurlyDirRi) and (p.tok.xkind != pxEof): 
            getTok(p)
          eat(p, pxCurlyDirRi)
      else: 
        getTok(p)             # skip token
  elif p.tok.ident.id == getIdent(identCache, "ptr").id: 
    result = newNodeP(nkPtrTy, p)
    getTok(p)
    eat(p, pxCurlyDirRi)
  elif p.tok.ident.id == getIdent(identCache, "tuple").id: 
    result = newNodeP(nkTupleTy, p)
    getTok(p)
    eat(p, pxCurlyDirRi)
  elif p.tok.ident.id == getIdent(identCache, "acyclic").id: 
    result = newIdentNodeP(p.tok.ident, p)
    getTok(p)
    eat(p, pxCurlyDirRi)
  else: 
    parMessage(p, errInvalidDirectiveX, $p.tok)
    while true: 
      getTok(p)
      if p.tok.xkind == pxCurlyDirRi or p.tok.xkind == pxEof: break 
    eat(p, pxCurlyDirRi)
    result = emptyNode

proc getPrecedence(kind: TTokKind): int = 
  case kind
  of pxDiv, pxMod, pxStar, pxSlash, pxShl, pxShr, pxAnd: result = 5
  of pxPlus, pxMinus, pxOr, pxXor: result = 4
  of pxIn, pxEquals, pxLe, pxLt, pxGe, pxGt, pxNeq, pxIs: result = 3
  else: result = -1
  
proc rangeExpr(p: var TParser): PNode = 
  var a = parseExpr(p)
  if p.tok.xkind == pxDotDot: 
    result = newNodeP(nkRange, p)
    addSon(result, a)
    getTok(p)
    skipCom(p, result)
    addSon(result, parseExpr(p))
  else: 
    result = a
  
proc bracketExprList(p: var TParser, first: PNode): PNode = 
  result = newNodeP(nkBracketExpr, p)
  addSon(result, first)
  getTok(p)
  skipCom(p, result)
  while true: 
    if p.tok.xkind == pxBracketRi: 
      getTok(p)
      break 
    if p.tok.xkind == pxEof: 
      parMessage(p, errXExpected, "token " & tokKindToStr(pxBracketRi))
      break 
    var a = rangeExpr(p)
    skipCom(p, a)
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)

proc exprColonEqExpr(p: var TParser, kind: TNodeKind, 
                     tok: TTokKind): PNode = 
  var a = parseExpr(p)
  if p.tok.xkind == tok: 
    result = newNodeP(kind, p)
    getTok(p)
    skipCom(p, result)
    addSon(result, a)
    addSon(result, parseExpr(p))
  else: 
    result = a
  
proc exprListAux(p: var TParser, elemKind: TNodeKind, 
                 endTok, sepTok: TTokKind, result: PNode) = 
  getTok(p)
  skipCom(p, result)
  while true: 
    if p.tok.xkind == endTok: 
      getTok(p)
      break 
    if p.tok.xkind == pxEof: 
      parMessage(p, errXExpected, "token " & tokKindToStr(endTok))
      break 
    var a = exprColonEqExpr(p, elemKind, sepTok)
    skipCom(p, a)
    if (p.tok.xkind == pxComma) or (p.tok.xkind == pxSemicolon): 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)

proc qualifiedIdent(p: var TParser): PNode = 
  if p.tok.xkind == pxSymbol: 
    result = createIdentNodeP(p.tok.ident, p)
  else: 
    parMessage(p, errXExpected, $p.tok)
    return emptyNode
  getTok(p)
  skipCom(p, result)
  if p.tok.xkind == pxDot: 
    getTok(p)
    skipCom(p, result)
    if p.tok.xkind == pxSymbol: 
      var a = result
      result = newNodeI(nkDotExpr, a.info)
      addSon(result, a)
      addSon(result, createIdentNodeP(p.tok.ident, p))
      getTok(p)
    else: 
      parMessage(p, errXExpected, "identifier " & $p.tok)
  
proc qualifiedIdentListAux(p: var TParser, endTok: TTokKind, 
                           result: PNode) = 
  getTok(p)
  skipCom(p, result)
  while true: 
    if p.tok.xkind == endTok: 
      getTok(p)
      break 
    if p.tok.xkind == pxEof: 
      parMessage(p, errXExpected, "token " & tokKindToStr(endTok))
      break 
    var a = qualifiedIdent(p)
    skipCom(p, a)
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)

proc exprColonEqExprList(p: var TParser, kind, elemKind: TNodeKind, 
                         endTok, sepTok: TTokKind): PNode = 
  result = newNodeP(kind, p)
  exprListAux(p, elemKind, endTok, sepTok, result)

proc setBaseFlags(n: PNode, base: TNumericalBase) = 
  case base
  of base10: discard
  of base2: incl(n.flags, nfBase2)
  of base8: incl(n.flags, nfBase8)
  of base16: incl(n.flags, nfBase16)
  
proc identOrLiteral(p: var TParser): PNode = 
  case p.tok.xkind
  of pxSymbol: 
    result = createIdentNodeP(p.tok.ident, p)
    getTok(p)
  of pxIntLit: 
    result = newIntNodeP(nkIntLit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of pxInt64Lit: 
    result = newIntNodeP(nkInt64Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of pxFloatLit: 
    result = newFloatNodeP(nkFloatLit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p)
  of pxStrLit: 
    if len(p.tok.literal) != 1: result = newStrNodeP(nkStrLit, p.tok.literal, p)
    else: result = newIntNodeP(nkCharLit, ord(p.tok.literal[0]), p)
    getTok(p)
  of pxNil: 
    result = newNodeP(nkNilLit, p)
    getTok(p)
  of pxParLe: 
    # () constructor
    result = exprColonEqExprList(p, nkPar, nkExprColonExpr, pxParRi, pxColon)
    #if hasSonWith(result, nkExprColonExpr) then
    #  replaceSons(result, nkExprColonExpr, nkExprEqExpr)
    if (sonsLen(result) > 1) and not hasSonWith(result, nkExprColonExpr): 
      result.kind = nkBracket # is an array constructor
  of pxBracketLe: 
    # [] constructor
    result = newNodeP(nkBracket, p)
    getTok(p)
    skipCom(p, result)
    while (p.tok.xkind != pxBracketRi) and (p.tok.xkind != pxEof): 
      var a = rangeExpr(p)
      if a.kind == nkRange: 
        result.kind = nkCurly # it is definitely a set literal
      opt(p, pxComma)
      skipCom(p, a)
      assert(a != nil)
      addSon(result, a)
    eat(p, pxBracketRi)
  of pxCommand: 
    result = parseCommand(p)
  else: 
    parMessage(p, errXExpected, "expression " & $(p.tok))
    getTok(p) # we must consume a token here to prevend endless loops!
    result = emptyNode
  if result.kind != nkEmpty: skipCom(p, result)
  
proc primary(p: var TParser): PNode = 
  # prefix operator?
  if (p.tok.xkind == pxNot) or (p.tok.xkind == pxMinus) or
      (p.tok.xkind == pxPlus): 
    result = newNodeP(nkPrefix, p)
    var a = newIdentNodeP(getIdent(identCache, $p.tok), p)
    addSon(result, a)
    getTok(p)
    skipCom(p, a)
    addSon(result, primary(p))
    return 
  elif p.tok.xkind == pxAt: 
    result = newNodeP(nkAddr, p)
    var a = newIdentNodeP(getIdent(identCache, $p.tok), p)
    getTok(p)
    if p.tok.xkind == pxBracketLe: 
      result = newNodeP(nkPrefix, p)
      addSon(result, a)
      addSon(result, identOrLiteral(p))
    else: 
      addSon(result, primary(p))
    return 
  result = identOrLiteral(p)
  while true: 
    case p.tok.xkind
    of pxParLe: 
      var a = result
      result = newNodeP(nkCall, p)
      addSon(result, a)
      exprListAux(p, nkExprEqExpr, pxParRi, pxEquals, result)
    of pxDot: 
      var a = result
      result = newNodeP(nkDotExpr, p)
      addSon(result, a)
      getTok(p)               # skip '.'
      skipCom(p, result)
      if p.tok.xkind == pxSymbol: 
        addSon(result, createIdentNodeP(p.tok.ident, p))
        getTok(p)
      else: 
        parMessage(p, errXExpected, "identifier " & $p.tok)
    of pxHat: 
      var a = result
      result = newNodeP(nkBracketExpr, p)
      addSon(result, a)
      getTok(p)
    of pxBracketLe: 
      result = bracketExprList(p, result)
    else: break 
  
proc lowestExprAux(p: var TParser, v: var PNode, limit: int): TTokKind = 
  var 
    nextop: TTokKind
    v2, node, opNode: PNode
  v = primary(p) # expand while operators have priorities higher than 'limit'
  var op = p.tok.xkind
  var opPred = getPrecedence(op)
  while (opPred > limit): 
    node = newNodeP(nkInfix, p)
    opNode = newIdentNodeP(getIdent(identCache, $(p.tok)), p) # skip operator:
    getTok(p)
    case op
    of pxPlus: 
      case p.tok.xkind
      of pxPer: 
        getTok(p)
        eat(p, pxCurlyDirRi)
        opNode.ident = getIdent(identCache, "+%")
      of pxAmp: 
        getTok(p)
        eat(p, pxCurlyDirRi)
        opNode.ident = getIdent(identCache, "&")
      else: 
        discard
    of pxMinus: 
      if p.tok.xkind == pxPer: 
        getTok(p)
        eat(p, pxCurlyDirRi)
        opNode.ident = getIdent(identCache, "-%")
    of pxEquals: 
      opNode.ident = getIdent(identCache, "==")
    of pxNeq: 
      opNode.ident = getIdent(identCache, "!=")
    else: 
      discard
    skipCom(p, opNode)        # read sub-expression with higher priority
    nextop = lowestExprAux(p, v2, opPred)
    addSon(node, opNode)
    addSon(node, v)
    addSon(node, v2)
    v = node
    op = nextop
    opPred = getPrecedence(nextop)
  result = op                 # return first untreated operator
  
proc fixExpr(n: PNode): PNode = 
  result = n
  case n.kind
  of nkInfix: 
    if n.sons[1].kind == nkBracket: n.sons[1].kind = nkCurly
    if n.sons[2].kind == nkBracket: n.sons[2].kind = nkCurly
    if (n.sons[0].kind == nkIdent): 
      if (n.sons[0].ident.id == getIdent("+").id): 
        if (n.sons[1].kind == nkCharLit) and (n.sons[2].kind == nkStrLit) and
            (n.sons[2].strVal == ""): 
          result = newStrNode(nkStrLit, chr(int(n.sons[1].intVal)) & "")
          result.info = n.info
          return              # do not process sons as they don't exist anymore
        elif (n.sons[1].kind in {nkCharLit, nkStrLit}) or
            (n.sons[2].kind in {nkCharLit, nkStrLit}): 
          n.sons[0].ident = getIdent("&") # fix operator
  else: 
    discard
  if not (n.kind in {nkEmpty..nkNilLit}): 
    for i in countup(0, sonsLen(n) - 1): result.sons[i] = fixExpr(n.sons[i])
  
proc parseExpr(p: var TParser): PNode = 
  var oldcontext = p.context
  p.context = conExpr
  if p.tok.xkind == pxCommand: 
    result = parseCommand(p)
  else: 
    discard lowestExprAux(p, result, - 1)
    result = fixExpr(result)
  p.context = oldcontext

proc parseExprStmt(p: var TParser): PNode = 
  var info = parLineInfo(p)
  var a = parseExpr(p)
  if p.tok.xkind == pxAsgn: 
    getTok(p)
    skipCom(p, a)
    var b = parseExpr(p)
    result = newNodeI(nkAsgn, info)
    addSon(result, a)
    addSon(result, b)
  else: 
    result = a
  
proc inImportBlackList(cache: IdentCache, ident: PIdent): bool = 
  for i in countup(low(ImportBlackList), high(ImportBlackList)): 
    if ident.id == getIdent(cache, ImportBlackList[i]).id: 
      return true

proc parseUsesStmt(p: var TParser): PNode = 
  var a: PNode
  result = newNodeP(nkImportStmt, p)
  getTok(p)                   # skip `import`
  skipCom(p, result)
  while true: 
    case p.tok.xkind
    of pxEof: break 
    of pxSymbol: a = newIdentNodeP(p.tok.ident, p)
    else: 
      parMessage(p, errXExpected, "identifier " & $(p.tok))
      break 
    getTok(p)                 # skip identifier, string
    skipCom(p, a)
    if pfImportBlackList notin p.flags or not inImportBlackList(identCache, a.ident): 
      addSon(result, createIdentNodeP(a.ident, p))
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    else: 
      break 
  if sonsLen(result) == 0: result = emptyNode
  
proc parseIncludeDir(p: var TParser): PNode = 
  result = newNodeP(nkIncludeStmt, p)
  getTok(p)                   # skip `include`
  var filename = ""
  while true: 
    case p.tok.xkind
    of pxSymbol, pxDot, pxDotDot, pxSlash: 
      add(filename, $p.tok)
      getTok(p)
    of pxStrLit: 
      filename = p.tok.literal
      getTok(p)
      break 
    of pxCurlyDirRi: 
      break 
    else: 
      parMessage(p, errXExpected, "identifier " & $p.tok)
      break 
  addSon(result, newStrNodeP(nkStrLit, changeFileExt(filename, "nim"), p))
  if filename == "config.inc": result = emptyNode
  
proc definedExprAux(p: var TParser): PNode = 
  result = newNodeP(nkCall, p)
  addSon(result, newIdentNodeP(getIdent(identCache, "defined"), p))
  expectIdent(p)
  addSon(result, createIdentNodeP(p.tok.ident, p))
  getTok(p)

proc isHandledDirective(p: TParser): bool = 
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: 
    case toLower(p.tok.ident.s)
    of "else", "endif": result = false
    else: result = true
  
proc parseStmtList(p: var TParser): PNode = 
  result = newNodeP(nkStmtList, p)
  while true: 
    case p.tok.xkind
    of pxEof: 
      break 
    of pxCurlyDirLe, pxStarDirLe: 
      if not isHandledDirective(p): break 
    else: 
      discard
    addSon(result, parseStmt(p))
  if sonsLen(result) == 1: result = result.sons[0]
  
proc parseIfDirAux(p: var TParser, result: PNode) = 
  addSon(result.sons[0], parseStmtList(p))
  if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: 
    var endMarker = succ(p.tok.xkind)
    if toLower(p.tok.ident.s) == "else": 
      var s = newNodeP(nkElse, p)
      while p.tok.xkind != pxEof and p.tok.xkind != endMarker: getTok(p)
      eat(p, endMarker)
      addSon(s, parseStmtList(p))
      addSon(result, s)
    if p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}: 
      endMarker = succ(p.tok.xkind)
      if toLower(p.tok.ident.s) == "endif": 
        while p.tok.xkind != pxEof and p.tok.xkind != endMarker: getTok(p)
        eat(p, endMarker)
      else: 
        parMessage(p, errXExpected, "{$endif}")
  else: 
    parMessage(p, errXExpected, "{$endif}")
  
proc parseIfdefDir(p: var TParser, endMarker: TTokKind): PNode = 
  result = newNodeP(nkWhenStmt, p)
  addSon(result, newNodeP(nkElifBranch, p))
  getTok(p)
  addSon(result.sons[0], definedExprAux(p))
  eat(p, endMarker)
  parseIfDirAux(p, result)

proc parseIfndefDir(p: var TParser, endMarker: TTokKind): PNode = 
  result = newNodeP(nkWhenStmt, p)
  addSon(result, newNodeP(nkElifBranch, p))
  getTok(p)
  var e = newNodeP(nkCall, p)
  addSon(e, newIdentNodeP(getIdent(identCache, "not"), p))
  addSon(e, definedExprAux(p))
  eat(p, endMarker)
  addSon(result.sons[0], e)
  parseIfDirAux(p, result)

proc parseIfDir(p: var TParser, endMarker: TTokKind): PNode = 
  result = newNodeP(nkWhenStmt, p)
  addSon(result, newNodeP(nkElifBranch, p))
  getTok(p)
  addSon(result.sons[0], parseExpr(p))
  eat(p, endMarker)
  parseIfDirAux(p, result)

proc parseDirective(p: var TParser): PNode = 
  result = emptyNode
  if not (p.tok.xkind in {pxCurlyDirLe, pxStarDirLe}): return 
  var endMarker = succ(p.tok.xkind)
  if p.tok.ident != nil: 
    case toLower(p.tok.ident.s)
    of "include": 
      result = parseIncludeDir(p)
      eat(p, endMarker)
    of "if": result = parseIfDir(p, endMarker)
    of "ifdef": result = parseIfdefDir(p, endMarker)
    of "ifndef": result = parseIfndefDir(p, endMarker)
    else: 
      # skip unknown compiler directive
      while p.tok.xkind != pxEof and p.tok.xkind != endMarker: getTok(p)
      eat(p, endMarker)
  else: 
    eat(p, endMarker)
  
proc parseRaise(p: var TParser): PNode = 
  result = newNodeP(nkRaiseStmt, p)
  getTok(p)
  skipCom(p, result)
  if p.tok.xkind != pxSemicolon: addSon(result, parseExpr(p))
  else: addSon(result, emptyNode)
  
proc parseIf(p: var TParser): PNode = 
  result = newNodeP(nkIfStmt, p)
  while true: 
    getTok(p)                 # skip ``if``
    var branch = newNodeP(nkElifBranch, p)
    skipCom(p, branch)
    addSon(branch, parseExpr(p))
    eat(p, pxThen)
    skipCom(p, branch)
    addSon(branch, parseStmt(p))
    skipCom(p, branch)
    addSon(result, branch)
    if p.tok.xkind == pxElse: 
      getTok(p)
      if p.tok.xkind != pxIf: 
        # ordinary else part:
        branch = newNodeP(nkElse, p)
        skipCom(p, result)    # BUGFIX
        addSon(branch, parseStmt(p))
        addSon(result, branch)
        break 
    else: 
      break 
  
proc parseWhile(p: var TParser): PNode = 
  result = newNodeP(nkWhileStmt, p)
  getTok(p)
  skipCom(p, result)
  addSon(result, parseExpr(p))
  eat(p, pxDo)
  skipCom(p, result)
  addSon(result, parseStmt(p))

proc parseRepeat(p: var TParser): PNode = 
  result = newNodeP(nkWhileStmt, p)
  getTok(p)
  skipCom(p, result)
  addSon(result, newIdentNodeP(getIdent(identCache, "true"), p))
  var s = newNodeP(nkStmtList, p)
  while p.tok.xkind != pxEof and p.tok.xkind != pxUntil: 
    addSon(s, parseStmt(p))
  eat(p, pxUntil)
  var a = newNodeP(nkIfStmt, p)
  skipCom(p, a)
  var b = newNodeP(nkElifBranch, p)
  var c = newNodeP(nkBreakStmt, p)
  addSon(c, emptyNode)
  addSon(b, parseExpr(p))
  skipCom(p, a)
  addSon(b, c)
  addSon(a, b)
  if b.sons[0].kind == nkIdent and b.sons[0].ident.id == getIdent(identCache, "false").id: 
    discard
  else: 
    addSon(s, a)
  addSon(result, s)

proc parseCase(p: var TParser): PNode = 
  var b: PNode
  result = newNodeP(nkCaseStmt, p)
  getTok(p)
  addSon(result, parseExpr(p))
  eat(p, pxOf)
  skipCom(p, result)
  while (p.tok.xkind != pxEnd) and (p.tok.xkind != pxEof): 
    if p.tok.xkind == pxElse: 
      b = newNodeP(nkElse, p)
      getTok(p)
    else: 
      b = newNodeP(nkOfBranch, p)
      while (p.tok.xkind != pxEof) and (p.tok.xkind != pxColon): 
        addSon(b, rangeExpr(p))
        opt(p, pxComma)
        skipCom(p, b)
      eat(p, pxColon)
    skipCom(p, b)
    addSon(b, parseStmt(p))
    addSon(result, b)
    if b.kind == nkElse: break 
  eat(p, pxEnd)

proc parseTry(p: var TParser): PNode = 
  result = newNodeP(nkTryStmt, p)
  getTok(p)
  skipCom(p, result)
  var b = newNodeP(nkStmtList, p)
  while not (p.tok.xkind in {pxFinally, pxExcept, pxEof, pxEnd}): 
    addSon(b, parseStmt(p))
  addSon(result, b)
  if p.tok.xkind == pxExcept: 
    getTok(p)
    while p.tok.ident.id == getIdent(identCache, "on").id: 
      b = newNodeP(nkExceptBranch, p)
      getTok(p)
      var e = qualifiedIdent(p)
      if p.tok.xkind == pxColon: 
        getTok(p)
        e = qualifiedIdent(p)
      addSon(b, e)
      eat(p, pxDo)
      addSon(b, parseStmt(p))
      addSon(result, b)
      if p.tok.xkind == pxCommand: discard parseCommand(p)
    if p.tok.xkind == pxElse: 
      b = newNodeP(nkExceptBranch, p)
      getTok(p)
      addSon(b, parseStmt(p))
      addSon(result, b)
  if p.tok.xkind == pxFinally: 
    b = newNodeP(nkFinally, p)
    getTok(p)
    var e = newNodeP(nkStmtList, p)
    while (p.tok.xkind != pxEof) and (p.tok.xkind != pxEnd): 
      addSon(e, parseStmt(p))
    if sonsLen(e) == 0: addSon(e, newNodeP(nkNilLit, p))
    addSon(result, e)
  eat(p, pxEnd)

proc parseFor(p: var TParser): PNode = 
  result = newNodeP(nkForStmt, p)
  getTok(p)
  skipCom(p, result)
  expectIdent(p)
  addSon(result, createIdentNodeP(p.tok.ident, p))
  getTok(p)
  eat(p, pxAsgn)
  var a = parseExpr(p)
  var b = emptyNode
  var c = newNodeP(nkCall, p)
  if p.tok.xkind == pxTo: 
    addSon(c, newIdentNodeP(getIdent(identCache, "countup"), p))
    getTok(p)
    b = parseExpr(p)
  elif p.tok.xkind == pxDownto: 
    addSon(c, newIdentNodeP(getIdent(identCache, "countdown"), p))
    getTok(p)
    b = parseExpr(p)
  else: 
    parMessage(p, errXExpected, "token " & tokKindToStr(pxTo))
  addSon(c, a)
  addSon(c, b)
  eat(p, pxDo)
  skipCom(p, result)
  addSon(result, c)
  addSon(result, parseStmt(p))

template parseParamImpl(p: var TParser; right: TTokKind; result): typed = 
  var a: PNode
  result = newNodeP(nkIdentDefs, p)
  var v = emptyNode
  case p.tok.xkind
  of pxConst: 
    getTok(p)
  of pxVar: 
    getTok(p)
    v = newNodeP(nkVarTy, p)
  of pxOut: 
    getTok(p)
    v = newNodeP(nkVarTy, p)
  else: 
    discard
  while true: 
    case p.tok.xkind
    of pxSymbol: a = createIdentNodeP(p.tok.ident, p)
    of pxColon, pxEof, right, pxEquals: break 
    else: 
      parMessage(p, errXExpected, "Identifier " & $p.tok)
      return 
    getTok(p)                 # skip identifier
    skipCom(p, a)
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)
  if p.tok.xkind == pxColon: 
    getTok(p)
    skipCom(p, result)
    if v.kind != nkEmpty: addSon(v, parseTypeDesc(p))
    else: v = parseTypeDesc(p)
    addSon(result, v)
  else: 
    addSon(result, emptyNode())
    if p.tok.xkind != pxEquals: 
      parMessage(p, errXExpected, $p.tok)
  if p.tok.xkind == pxEquals: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseExpr(p))
  else: 
    addSon(result, emptyNode())

proc genSelfType(p: var TParser): PNode =
  result = createIdentNodeP(p.selfClass, p)
  var info = p.classes.getOrDefault(p.selfClass.id)
  if info == nil:
    echo "warning not found ", p.selfClass.s
  if info != nil and not info.isRefTy:
    result = newNodeP(nkVarTy, p).add(result)
  
proc genSelfParam(p: var TParser): PNode =
  result = newNodeP(nkIdentDefs, p).add(
    newIdentNameNodeP("self", p),
    genSelfType(p),
    emptyNode)

template parseParamListImpl(p: var TParser; right: TTokKind): typed = 
  proc parseParam(px: var TParser): PNode = 
    parseParamImpl(px, right, result)
  var a: PNode
  result = newNodeP(nkFormalParams, p)
  addSon(result, emptyNode())         # return type
  if isNil(p.outerProc) and not isNil(p.selfClass):
    addSon(result, genSelfParam(p))
  if (right == pxParRi and p.tok.xkind == pxParLe) or 
    (right == pxBracketRi and p.tok.xkind == pxBracketLe): 
      p.inParamList = true
      getTok(p)
      skipCom(p, result)
      while true: 
        case p.tok.xkind
        of pxSymbol, pxConst, pxVar, pxOut: 
          a = parseParam(p)
        of right: 
          getTok(p)
          break 
        else: 
          parMessage(p, errXExpected, "token " & $right)
          break 
        skipCom(p, a)
        if p.tok.xkind == pxSemicolon: 
          getTok(p)
          skipCom(p, a)
        addSon(result, a)
      p.inParamList = false
  if p.tok.xkind == pxColon: 
    getTok(p)
    skipCom(p, result)
    result.sons[0] = parseTypeDesc(p)

proc parseParamList(p: var TParser): PNode = 
  parseParamListImpl(p, pxParRi)

proc parseArrayPropParams(p: var TParser): PNode = 
  parseParamListImpl(p, pxBracketRi)
  
proc parseCallingConvention(p: var TParser): PNode = 
  result = emptyNode
  case p.tok.xkind 
  of pxSymbol, pxInline: 
    case toLower(p.tok.ident.s)
    of "stdcall", "cdecl", "safecall", "syscall", "inline", "fastcall": 
      result = newNodeP(nkPragma, p)
      addSon(result, newIdentNodeP(p.tok.ident, p))
      getTok(p)
      opt(p, pxSemicolon)
    of "register": 
      result = newNodeP(nkPragma, p)
      addSon(result, newIdentNameNodeP("fastcall", p))
      getTok(p)
      opt(p, pxSemicolon)
    else: 
      discard
  of pxOf:
    if p.peekTok.xkind == pxObject:
      result = newNodeP(nkPragma, p)
      addSon(result, newIdentNameNodeP("closure", p))
      p.eat(pxOf)
      p.eat(pxObject)
  else: discard

proc parseRoutineSpecifiers(p: var TParser, noBody: var bool, isVirtual: var bool): PNode = 
  var e: PNode
  result = parseCallingConvention(p)
  while p.tok.xkind in {pxSymbol, pxInline}: 
    case toLower(p.tok.ident.s)
    of "assembler", "overload", "far": 
      getTok(p)
      opt(p, pxSemicolon)
    of "forward": 
      noBody = true
      getTok(p)
      opt(p, pxSemicolon)
    of "importc": 
      # This is a fake for platform module. There is no ``importc``
      # directive in Pascal.
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      addSon(result, newIdentNodeP(getIdent(identCache, "importc"), p))
      noBody = true
      getTok(p)
      opt(p, pxSemicolon)
    of "noconv": 
      # This is a fake for platform module. There is no ``noconv``
      # directive in Pascal.
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      addSon(result, newIdentNodeP(getIdent(identCache, "noconv"), p))
      noBody = true
      getTok(p)
      opt(p, pxSemicolon)
    of "procvar": 
      # This is a fake for the Nim compiler. There is no ``procvar``
      # directive in Pascal.
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      addSon(result, newIdentNodeP(getIdent(identCache, "procvar"), p))
      getTok(p)
      opt(p, pxSemicolon)
    of "varargs": 
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      addSon(result, newIdentNodeP(getIdent(identCache, "varargs"), p))
      getTok(p)
      opt(p, pxSemicolon)
    of "external": 
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      getTok(p)
      noBody = true
      e = newNodeP(nkExprColonExpr, p)
      addSon(e, newIdentNodeP(getIdent(identCache, "dynlib"), p))
      addSon(e, parseExpr(p))
      addSon(result, e)
      opt(p, pxSemicolon)
      if (p.tok.xkind == pxSymbol) and
          (p.tok.ident.id == getIdent(identCache, "name").id): 
        e = newNodeP(nkExprColonExpr, p)
        getTok(p)
        addSon(e, newIdentNodeP(getIdent(identCache, "importc"), p))
        addSon(e, parseExpr(p))
        addSon(result, e)
      else: 
        addSon(result, newIdentNodeP(getIdent(identCache, "importc"), p))
      opt(p, pxSemicolon)
    of "virtual":
      isVirtual = true
      getTok(p)
      opt(p, pxSemicolon)
    of "override":
      isVirtual = true
      if p.keepOverride:
        if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
        addSon(result, newIdentNodeP(getIdent(identCache, "override"), p))
      getTok(p)
      opt(p, pxSemicolon)
        
    else: 
      e = parseCallingConvention(p)
      if e.kind == nkEmpty: break 
      if result.kind == nkEmpty: result = newNodeP(nkPragma, p)
      addSon(result, e.sons[0])

proc parseRoutineType(p: var TParser): PNode = 
  result = newNodeP(nkProcTy, p)
  getTok(p)
  skipCom(p, result)
  addSon(result, parseParamList(p))
  opt(p, pxSemicolon)
  addSon(result, parseCallingConvention(p))
  skipCom(p, result)

proc parseEnum(p: var TParser): PNode = 
  var a: PNode
  result = newNodeP(nkEnumTy, p)
  getTok(p)
  skipCom(p, result)
  addSon(result, emptyNode) # it does not inherit from any enumeration
  while true: 
    case p.tok.xkind
    of pxEof, pxParRi: break 
    of pxSymbol: a = newIdentNodeP(p.tok.ident, p)
    else: 
      parMessage(p, errXExpected, "Identifier " & $(p.tok))
      break 
    getTok(p)                 # skip identifier
    skipCom(p, a)
    if (p.tok.xkind == pxEquals) or (p.tok.xkind == pxAsgn): 
      getTok(p)
      skipCom(p, a)
      var b = a
      a = newNodeP(nkEnumFieldDef, p)
      addSon(a, b)
      addSon(a, parseExpr(p))
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)
  eat(p, pxParRi)

proc identVis(p: var TParser): PNode = 
  # identifier with visability
  var a = createIdentNodeP(p.tok.ident, p)
  if p.visibility != visPrivate: # section == seInterface: 
    result = newNodeP(nkPostfix, p)
    addSon(result, newIdentNodeP(getIdent(identCache, "*"), p))
    addSon(result, a)
  else: 
    result = a
  getTok(p)

type 
  TSymbolParser = proc (p: var TParser): PNode {.nimcall.}

proc rawIdent(p: var TParser): PNode = 
  result = createIdentNodeP(p.tok.ident, p)
  getTok(p)

proc parseIdentColonEquals(p: var TParser, 
                           identParser: TSymbolParser): PNode = 
  var a: PNode
  result = newNodeP(nkIdentDefs, p)
  while true: 
    case p.tok.xkind
    of pxSymbol: a = identParser(p)
    of pxColon, pxEof, pxParRi, pxEquals: break 
    else: 
      parMessage(p, errXExpected, "Identifier " & $(p.tok))
      return 
    skipCom(p, a)
    if p.tok.xkind == pxComma: 
      getTok(p)
      skipCom(p, a)
    addSon(result, a)
  if p.tok.xkind == pxColon: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseTypeDesc(p))
  else: 
    addSon(result, emptyNode)
    if p.tok.xkind != pxEquals: 
      parMessage(p, errXExpected, $(p.tok))
  if p.tok.xkind == pxEquals: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseExpr(p))
  else: 
    addSon(result, emptyNode)
  if p.tok.xkind == pxSemicolon: 
    getTok(p)
    skipCom(p, result)

proc parseRecordCase(p: var TParser): PNode = 
  var b, c: PNode
  result = newNodeP(nkRecCase, p)
  getTok(p)
  var a = newNodeP(nkIdentDefs, p)
  addSon(a, rawIdent(p))
  eat(p, pxColon)
  addSon(a, parseTypeDesc(p))
  addSon(a, emptyNode)
  addSon(result, a)
  eat(p, pxOf)
  skipCom(p, result)
  while true: 
    case p.tok.xkind
    of pxEof, pxEnd: 
      break 
    of pxElse: 
      b = newNodeP(nkElse, p)
      getTok(p)
    else: 
      b = newNodeP(nkOfBranch, p)
      while (p.tok.xkind != pxEof) and (p.tok.xkind != pxColon): 
        addSon(b, rangeExpr(p))
        opt(p, pxComma)
        skipCom(p, b)
      eat(p, pxColon)
    skipCom(p, b)
    c = newNodeP(nkRecList, p)
    eat(p, pxParLe)
    while (p.tok.xkind != pxParRi) and (p.tok.xkind != pxEof): 
      addSon(c, parseIdentColonEquals(p, rawIdent))
      opt(p, pxSemicolon)
      skipCom(p, lastSon(c))
    eat(p, pxParRi)
    opt(p, pxSemicolon)
    if sonsLen(c) > 0: skipCom(p, lastSon(c))
    else: addSon(c, newNodeP(nkNilLit, p))
    addSon(b, c)
    addSon(result, b)
    if b.kind == nkElse: break 

proc genArrayPropertyType(p: var TParser; propName: PIdent): PNode =
  result = newNodeP(nkTypeSection, p).add(
    newNodeP(nkTypeDef, p).add(
      newIdentNameNodeP("`" & p.selfClass.s & "*" & propName.s & "`", p),
      emptyNode,
      newNodeP(nkDistinctTy, p).add(
        newIdentNodeP(p.selfClass, p)
      )
    )
  )  

proc genArrayProperty(p: var TParser; arrPropTy, propTy: PNode; asReader: bool; 
                    callId: PIdent, params: PNode): PNode = 
  var name = if asReader: newIdentNameNodeP("`[]`", p) else: newIdentNameNodeP("`[]=`", p)
  result =  newNodeP(nkTemplateDef, p).add(
    name,
    emptyNode,
    emptyNode,
    newNodeP(nkFormalParams, p).add(
      if asReader: propTy else: emptyNode,
      newNodeP(nkIdentDefs, p).add(
        newIdentNameNodeP("self", p),
        arrPropTy,
        emptyNode
      )
    ),
    emptyNode,
    emptyNode,
    newNodeP(nkStmtList, p).add(
      newNodeP(nkCall, p).add(
        newIdentNodeP(callId, p),
        newNodeP(nkDotExpr, p).add(
          newIdentNameNodeP("self", p),
          newIdentNodeP(p.selfClass, p)
        )
      )
    )
  ) 
  if p.visibility != visPrivate:
    exSymbol(result.sons[0])
  for i in 2..< params.sons.len:
    result.sons[3].addSon(params.sons[i])
    result.sons[6].sons[0].addSon(params.sons[i][0])
  if not asReader:
    result.sons[3].addSon(newNodeP(nkIdentDefs, p).add(
      newIdentNameNodeP("v", p),
      propTy,
      emptyNode
    ))
    result.sons[6].sons[0].addSon(newIdentNameNodeP("v", p))

proc genPropertyReader(p: var TParser, propName: PIdent, propTy: PNode, readerId: PIdent): PNode =
  result =  newNodeP(nkTemplateDef, p).add(
    newIdentNodeP(propName, p),
    emptyNode,
    emptyNode,
    newNodeP(nkFormalParams, p).add(
      propTy,
      newNodeP(nkIdentDefs, p).add(
        newIdentNameNodeP("self", p),
        newIdentNodeP(p.selfClass, p),
        emptyNode
      )
    ),
    emptyNode,
    emptyNode,
    newNodeP(nkStmtList, p).add(
      newNodeP(nkDotExpr, p).add(
        newIdentNameNodeP("self", p),
        newIdentNodeP(readerId, p)
      )
    )
  ) 
  if p.visibility != visPrivate:
    exSymbol(result.sons[0])

proc genPropertyWriter(p: var TParser, propName: PIdent, propTy: PNode, writerId: PIdent): PNode =
  let value = newIdentNameNodeP("v", p)
  let asgn = newNodeP(nkAsgn, p).add(
      newNodeP(nkDotExpr, p).add(
        newIdentNameNodeP("self", p),
        newIdentNodeP(writerId, p)
      ),
      value
    )
  let call = newNodeP(nkCall, p).add(
      newIdentNodeP(writerId, p),
      newIdentNameNodeP("self", p),
      value
  )
  # identfiers starting with F are treated as variables, starting with set as setter calls
  # all other the nim compiler should decide
  var setter = if writerId.s[0..0].toLower == "f":
      asgn
    elif writerId.s[0..2].toLower == "set":
      call
    else:
      newNodeP(nkWhenStmt, p).add(
        newNodeP(nkElifBranch, p).add(
          newNodeP(nkCall, p).add(
            newIdentNameNodeP("compiles", p),
            call
          ),
          newNodeP(nkStmtList, p).add(call)
        ),
        newNodeP(nkElse, p).add(
          newNodeP(nkStmtList, p).add(asgn)
        )
      )
      
  result =  newNodeP(nkTemplateDef, p).add(
    newNodeP(nkAccQuoted, p).add(
      newIdentNodeP(propName, p),
      newIdentNameNodeP("=", p)
    ),
    emptyNode,
    emptyNode,
    newNodeP(nkFormalParams, p).add(
      emptyNode,
      genSelfParam(p),
      newNodeP(nkIdentDefs, p).add(
        value,
        propTy,
        emptyNode
      ) ),
    emptyNode,
    emptyNode,
    newNodeP(nkStmtList, p).add(
      setter
    )
  )

proc parseProperty(p: var TParser): PNode =
  result = emptyNode
  template nextIdent(): PIdent =
    getTok(p)
    if p.tok.xkind != pxSymbol: parMessage(p, errXExpected, "Identifier " & $p.tok)
    var res = p.tok.ident
    getTok(p)
    res

  var propName = nextIdent()
  var propTy: PNode
  var params: PNode
  if p.tok.xkind == pxBracketLe: # array property ?
    params = parseArrayPropParams(p)
    propTy = params.sons[0]
  else:
    p.eat(pxColon)
    propTy = parseTypeDesc(p)
  var readId, writeId: PIdent
  while (p.tok.xkind != pxEof) and (p.tok.xkind != pxSemicolon):
    case p.tok.xkind
    of pxSymbol:
      if p.tok.ident.id == getIdent(identCache, "read").id: 
        readId = nextIdent()
      elif p.tok.ident.id == getIdent(identCache, "write").id: 
        writeId = nextIdent()
      elif p.tok.ident.id == getIdent(identCache, "default").id: 
        discard nextIdent()
      else:
        parMessage(p, errXExpected, "Identifier " & $p.tok)
    else:
      parMessage(p, errXExpected, "Identifier " & $p.tok)
  # echo "property ", propName.s, ": ", propTy.treeRepr, " for class ", p.selfClass.s
  opt(p, pxSemiColon)
  var isDefault = false
  if params != nil:
    if p.tok.xkind == pxSymbol and p.tok.ident.id == getIdent(identCache, "default").id:
      isDefault = true;
      eat(p, pxSymbol)
      opt(p, pxSemiColon)
  var flags: set[TExtraInfo]
  if p.visibility != visPrivate:
    incl(flags, eiPublic)
  var arrPropTy: PNode
  if params != nil:
    arrPropTy = genArrayPropertyType(p, propName)
    p.pushExtraStmt(TExtraStmt(node: arrPropTy, flags: flags))
    var a = genPropertyReader(p, propName, arrPropTy[0][0], arrPropTy[0][0].ident)
    p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
  if readId != nil:
    if params == nil:
      var a = genPropertyReader(p, propName, propTy, readId)
      p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
    else:
      var a = genArrayProperty(p, arrPropTy[0][0], propTy, true, readId, params)
      p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
      if  isDefault:
        a = genArrayProperty(p, newIdentNodeP(p.selfClass, p), propTy, true, readId, params)
        p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
  if writeId != nil :
    if params == nil:
      var a = genPropertyWriter(p, propName, propTy, writeId)
      p.pushExtraStmt(TExtraStmt(node: a, flags: flags))    
    else :
      var a = genArrayProperty(p, arrPropTy[0][0], propTy, false, writeId, params)
      p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
      if  isDefault:
        a = genArrayProperty(p, newIdentNodeP(p.selfClass, p), propTy, false, writeId, params)
        p.pushExtraStmt(TExtraStmt(node: a, flags: flags))
      

proc parseRecordPart(p: var TParser): PNode = 
  result = emptyNode
  while (p.tok.xkind != pxEof) and (p.tok.xkind != pxEnd): 
    if result.kind == nkEmpty: result = newNodeP(nkRecList, p)
    case p.tok.xkind
    of pxSymbol: 
      var defs = parseIdentColonEquals(p, rawIdent)
      if p.visibility != visPrivate : 
        fixRecordDef(defs)
      addSon(result, defs)
      opt(p, pxSemicolon)
      skipCom(p, lastSon(result))
    of pxCase: 
      addSon(result, parseRecordCase(p))
    of pxPrivate: 
      p.visibility = visPrivate
      getTok(p)
    of pxProtected: 
      p.visibility = visProtected
      getTok(p)
    of pxPublic: 
      p.visibility = visPublic
      getTok(p)      
    of pxPublished: 
      p.visibility = visPublished
      getTok(p)
    of pxComment: 
      if result.sons.len > 0:
        skipCom(p, lastSon(result))
      else:
        skipCom(p, result)
    of pxFunction, pxProcedure, pxConstructor, pxDestructor:
      let xkind = p.tok.xkind
      var a = parseRoutine(p, true) # TODO: forward all methods defs and mark virtual methods as "method" and keep the calling conventions
      var flags: set[TExtraInfo]
      if xkind == pxConstructor: incl(flags, eiConstructor)
      elif xkind == pxDestructor: incl(flags, eiDestructor)
      if p.visibility != visPrivate: incl(flags, eiPublic)
      p.pushExtraStmt(TExtraStmt(node: a, flags: flags)) 
    of pxProperty:
      var a = parseProperty(p)
    else: 
      parMessage(p, errXExpected, "Identifier " & $p.tok)
      break

proc exSymbol(n: var PNode) = 
  case n.kind
  of nkPostfix: 
    discard
  of nkPragmaExpr: 
    exSymbol(n.sons[0])
  of nkIdent, nkAccQuoted: 
    var a = newNodeI(nkPostFix, n.info)
    addSon(a, newIdentNode(getIdent("*"), n.info))
    addSon(a, n)
    n = a
  else: internalError(gConfig, n.info, "exSymbol(): " & $n.kind)
  
proc fixRecordDef(n: var PNode) = 
  case n.kind
  of nkRecCase: 
    fixRecordDef(n.sons[0])
    for i in countup(1, sonsLen(n) - 1): 
      var length = sonsLen(n.sons[i])
      fixRecordDef(n.sons[i].sons[length - 1])
  of nkRecList, nkRecWhen, nkElse, nkOfBranch, nkElifBranch, nkObjectTy: 
    for i in countup(0, sonsLen(n) - 1): fixRecordDef(n.sons[i])
  of nkIdentDefs: 
    for i in countup(0, sonsLen(n) - 3): exSymbol(n.sons[i])
  of nkNilLit, nkEmpty: discard
  else: internalError(gConfig, n.info, "fixRecordDef(): " & $n.kind)
  
proc addPragmaToIdent(ident: var PNode, pragma: PNode) = 
  var pragmasNode: PNode
  if ident.kind != nkPragmaExpr: 
    pragmasNode = newNodeI(nkPragma, ident.info)
    var e = newNodeI(nkPragmaExpr, ident.info)
    addSon(e, ident)
    addSon(e, pragmasNode)
    ident = e
  else: 
    pragmasNode = ident.sons[1]
    if pragmasNode.kind != nkPragma: 
      internalError(gConfig, ident.info, "addPragmaToIdent")
  addSon(pragmasNode, pragma)

proc parseRecordBody(p: var TParser, result, definition: PNode) =
  let oldSelfClass =  p.selfClass
  let oldVisibilty = p.visibility
  p.selfClass = getIdentOfNode(definition)
  skipCom(p, result)
  p.visibility = if result.kind != nkTupleTy: visPublic else: visPrivate
  var a = parseRecordPart(p)
  # if result.kind != nkTupleTy: fixRecordDef(a)
  addSon(result, a)
  eat(p, pxEnd)
  case p.tok.xkind
  of pxSymbol: 
    if p.tok.ident.id == getIdent(identCache, "acyclic").id: 
      if definition != nil: 
        addPragmaToIdent(definition.sons[0], newIdentNodeP(p.tok.ident, p))
      else: 
        internalError(gConfig, result.info, "anonymous record is not supported")
      getTok(p)
    else: 
      internalError(gConfig, result.info, "parseRecordBody")
  of pxCommand: 
    if definition != nil: addPragmaToIdent(definition.sons[0], parseCommand(p))
    else: internalError(gConfig, result.info, "anonymous record is not supported")
  else: 
    discard
  opt(p, pxSemicolon)
  skipCom(p, result)
  p.visibility = oldVisibilty
  p.selfClass = oldSelfClass

proc genInherited(p: var TParser; child, root: PIdent): PNode =
  # echo "child ", child.s, " root ", root.s
  return newNodeP(nkTemplateDef, p).add(
      newIdentNameNodeP("inherited", p),
      emptyNode,
      emptyNode,
      newNodeP(nkFormalParams, p).add(
        newIdentNodeP(root, p),
        newNodeP(nkIdentDefs, p).add(
          newIdentNameNodeP("self", p),
          newIdentNodeP(child, p),
          emptyNode)),
      emptyNode,
      emptyNode,
      newNodeP(nkStmtList, p).add(
        newNodeP(nkCall, p).add(
          newIdentNodeP(root, p),
          newIdentNameNodeP("self", p))))

proc parseRecordOrObject(p: var TParser, kind: TNodeKind, 
                         definition: PNode): PNode = 
  var record: PNode
  result = newNodeP(kind, p)
  if kind == nkRefTy:
    record = newNodeP(nkObjectTy, p)
    result.addSon(record)
  else:
    record = result
  getTok(p)
  addSon(record, emptyNode)
  var defIdent = getIdentOfNode(definition)
  if p.tok.xkind == pxParLe: 
    p.classes[defIdent.id] = TObjectInfo(isRefTy: kind == nkRefTy)
    var a = newNodeP(nkOfInherit, p)
    getTok(p)
    var ty = parseTypeDesc(p)
    addSon(a, ty)
    # echo defIdent.s, " of ", ty.ident.s
    p.pushExtraStmt(TExtraStmt(node: genInherited(p, defIdent, ty.ident), flags: {}))
    addSon(record, a)
    eat(p, pxParRi)
  elif kind == nkRefTy:
    p.classes[defIdent.id] = TObjectInfo(isRefTy: true)
    var a = newNodeP(nkOfInherit, p)
    a.addSon(newIdentNameNodeP("RootRef", p))
    record.addSon(a)
  elif kind == nkObjectTy: 
    p.classes[defIdent.id] = TObjectInfo(isRefTy: false)
    addSon(record, newNodeP(nkPragma, p).add(newIdentNameNodeP("inheritable", p)) )
  else: 
    addSon(record, emptyNode)
  parseRecordBody(p, record, definition)

proc parseTypeDesc(p: var TParser, definition: var PNode): PNode = 
  var oldcontext = p.context
  p.context = conTypeDesc
  if p.tok.xkind == pxPacked: getTok(p)
  case p.tok.xkind
  of pxCommand: 
    result = parseCommand(p, definition)
  of pxProcedure, pxFunction: 
    result = parseRoutineType(p)
  of pxRecord: 
    getTok(p)
    if p.tok.xkind == pxCommand: 
      result = parseCommand(p)
      if result.kind != nkTupleTy: internalError(gConfig, result.info, "parseTypeDesc")
      parseRecordBody(p, result, definition)
      var a = lastSon(result)     # embed nkRecList directly into nkTupleTy
      for i in countup(0, sonsLen(a) - 1): 
        if i == 0: result.sons[sonsLen(result) - 1] = a.sons[0]
        else: addSon(result, a.sons[i])
    else: 
      result = newNodeP(nkObjectTy, p)
      addSon(result, emptyNode)
      addSon(result, emptyNode)
      parseRecordBody(p, result, definition)
      if definition != nil: 
        addPragmaToIdent(definition.sons[0], newIdentNodeP(getIdent(identCache, "final"), p))
      else: 
        internalError(gConfig, result.info, "anonymous record is not supported")
  of pxObject: result = parseRecordOrObject(p, nkObjectTy, definition)
  of pxClass: 
    let next = peekTok(p)
    if next.xkind == pxSemiColon:
      var n = newNodeP(nkCommentStmt, p)
      n.comment = definition.renderTree & " = class;"
      definition = n
      result = emptyNode
      p.eat(pxClass)
      p.eat(pxSemiColon)
      skipCom(p, result)
    else:
      result = parseRecordOrObject(p, nkRefTy, definition)
  of pxParLe: result = parseEnum(p)
  of pxArray: 
    result = newNodeP(nkBracketExpr, p)
    getTok(p)
    var isArrayOfConst = false
    if p.tok.xkind == pxBracketLe: 
      addSon(result, newIdentNodeP(getIdent(identCache, "array"), p))
      getTok(p)
      addSon(result, rangeExpr(p))
      eat(p, pxBracketRi)
    else: 
      if p.inParamList: 
        if p.peekTok.xkind != pxConst:
          addSon(result, newIdentNodeP(getIdent(identCache, "openarray"), p))
        else:
          isArrayOfConst = true
          result = newIdentNameNodeP("TArrayOfConst", p)
      else: addSon(result, newIdentNodeP(getIdent(identCache, "seq"), p))
    eat(p, pxOf)
    if not isArrayOfConst:
      addSon(result, parseTypeDesc(p))
    else:
      eat(p, pxConst)
  of pxSet: 
    result = newNodeP(nkBracketExpr, p)
    getTok(p)
    eat(p, pxOf)
    addSon(result, newIdentNodeP(getIdent(identCache, "set"), p))
    addSon(result, parseTypeDesc(p))
  of pxHat: 
    getTok(p)
    if p.tok.xkind == pxCommand: result = parseCommand(p)
    elif pfRefs in p.flags: result = newNodeP(nkRefTy, p)
    else: result = newNodeP(nkPtrTy, p)
    addSon(result, parseTypeDesc(p))
  of pxType: 
    getTok(p)
    result = parseTypeDesc(p)
  else: 
    var a = primary(p)
    if p.tok.xkind == pxDotDot: 
      result = newNodeP(nkBracketExpr, p)
      var r = newNodeP(nkRange, p)
      addSon(result, newIdentNodeP(getIdent(identCache, "range"), p))
      getTok(p)
      addSon(r, a)
      addSon(r, parseExpr(p))
      addSon(result, r)
    else: 
      result = a
  p.context = oldcontext

proc parseTypeDef(p: var TParser): PNode = 
  result = newNodeP(nkTypeDef, p)
  addSon(result, identVis(p))
  addSon(result, emptyNode)         # generic params
  if p.tok.xkind == pxEquals: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseTypeDesc(p, result))
  else: 
    addSon(result, emptyNode)
  if p.tok.xkind == pxSemicolon: 
    getTok(p)
    skipCom(p, result)

proc parseTypeSection(p: var TParser): PNode = 
  result = newNodeP(nkTypeSection, p)
  getTok(p)
  skipCom(p, result)
  while p.tok.xkind == pxSymbol: 
    addSon(result, parseTypeDef(p))

proc parseConstant(p: var TParser): PNode = 
  result = newNodeP(nkConstDef, p)
  addSon(result, identVis(p))
  if p.tok.xkind == pxColon: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseTypeDesc(p))
  else: 
    addSon(result, emptyNode)
    if p.tok.xkind != pxEquals: 
      parMessage(p, errXExpected, $(p.tok))
  if p.tok.xkind == pxEquals: 
    getTok(p)
    skipCom(p, result)
    addSon(result, parseExpr(p))
  else: 
    addSon(result, emptyNode)
  if p.tok.xkind == pxSemicolon: 
    getTok(p)
    skipCom(p, result)

proc parseConstSection(p: var TParser): PNode = 
  result = newNodeP(nkConstSection, p)
  getTok(p)
  skipCom(p, result)
  while p.tok.xkind == pxSymbol: 
    addSon(result, parseConstant(p))

proc parseVar(p: var TParser): PNode = 
  result = newNodeP(nkVarSection, p)
  getTok(p)
  skipCom(p, result)
  while p.tok.xkind == pxSymbol:
    addSon(result, parseIdentColonEquals(p, identVis))
  p.lastVarSection = result

proc fixConstructorReturnTy(p: var TParser, n: var PNode, isHeader: bool) =
  n.sons[3].sons[0] = newIdentNodeP(p.selfClass, p)
  if isHeader:
    var pragma = if n.sons[4].kind == nkPragma: n.sons[4] else: newNodeP(nkPragma, p)
    pragma.addSon(newIdentNameNodeP("discardable", p))
    n.sons[4] = pragma

proc parseRoutine(p: var TParser; noBody: bool): PNode = 
  var noBody = noBody
  var isVirtual: bool
  let kind = p.tok.xkind
  let oldOuter = p.outerProc
  let oldVisibilty = p.visibility
  p.visibility = visPrivate
  result = newNodeP(nkProcDef, p)
  getTok(p)
  skipCom(p, result)
  expectIdent(p)
  let oldSelfClass = p.selfClass
  var next = p.peekTok
  if next.xkind == pxDot: # is it method ?
    p.selfClass = p.tok.ident
    p.removeNextTok
    getTok(p)
    skipCom(p, result)
    expectIdent(p)
  else:
    # p.selfClass = nil
    discard
  addSon(result, identVis(p))
  # patterns, generic parameters:
  addSon(result, emptyNode)
  addSon(result, emptyNode)   
  addSon(result, parseParamList(p))
  opt(p, pxSemicolon)
  addSon(result, parseRoutineSpecifiers(p, noBody, isVirtual))
  addSon(result, emptyNode)
  var onlyHeader = (p.section == seInterface) or noBody
  if kind == pxConstructor and p.selfClass != nil:
    p.fixConstructorReturnTy(result, onlyHeader)
  if onlyHeader: 
    if isVirtual: p.methods[genSignature(result)] = true
    addSon(result, emptyNode)
  else: 
    if oldOuter == nil:
      p.outerProc = result
    var stmts = newNodeP(nkStmtList, p)
    if kind == pxConstructor and p.selfClass != nil:
      var info = p.classes.getOrDefault(p.selfClass.id)
      if info == nil or info.isRefTy : # class constructor should return its type
        stmts.addSon(
          newNodeP(nkAsgn, p).add(
            newIdentNameNodeP("result", p),
            newIdentNameNodeP("self", p) ) )
    while true: 
      case p.tok.xkind
      of pxVar: addSon(stmts, parseVar(p))
      of pxConst: addSon(stmts, parseConstSection(p))
      of pxType: addSon(stmts, parseTypeSection(p))
      of pxComment: skipCom(p, result)
      of pxBegin: break 
      of pxProcedure, pxFunction: addSon(stmts, parseRoutine(p, false))
      else: 
        parMessage(p, errXExpected, "begin")
        break 
    var a = parseStmt(p)
    for i in countup(0, sonsLen(a) - 1): addSon(stmts, a.sons[i])
    addSon(result, stmts)
  if isVirtual or p.methods.hasKey(genSignature(result)) :
    var resMethod = newNodeP(nkMethodDef, p)
    resMethod.sons = result.sons
    result = resMethod
  p.selfClass = oldSelfClass
  p.outerProc = oldOuter
  p.visibility = oldVisibilty

proc fixExit(p: var TParser, n: PNode): bool = 
  if (p.tok.ident.id == getIdent(identCache, "exit").id): 
    var length = sonsLen(n)
    if (length <= 0): return 
    var a = n.sons[length-1]
    if (a.kind == nkAsgn) and (a.sons[0].kind == nkIdent) and
        (a.sons[0].ident.id == getIdent(identCache, "result").id): 
      delSon(a, 0)
      a.kind = nkReturnStmt
      result = true
      getTok(p)
      opt(p, pxSemicolon)
      skipCom(p, a)

proc fixVarSection(p: var TParser, counter: PNode) = 
  if p.lastVarSection == nil: return 
  assert(counter.kind == nkIdent)
  for i in countup(0, sonsLen(p.lastVarSection) - 1): 
    var v = p.lastVarSection.sons[i]
    for j in countup(0, sonsLen(v) - 3): 
      if v.sons[j].ident.id == counter.ident.id: 
        delSon(v, j)
        if sonsLen(v) <= 2: 
          delSon(p.lastVarSection, i)
        return 

proc exSymbols(n: PNode) = 
  case n.kind
  of nkEmpty..nkNilLit: discard
  of nkProcDef..nkIteratorDef: exSymbol(n.sons[namePos])
  of nkWhenStmt, nkStmtList: 
    for i in countup(0, sonsLen(n) - 1): exSymbols(n.sons[i])
  of nkVarSection, nkConstSection: 
    for i in countup(0, sonsLen(n) - 1): exSymbol(n.sons[i].sons[0])
  of nkTypeSection: 
    for i in countup(0, sonsLen(n) - 1): 
      exSymbol(n.sons[i].sons[0])
      if n.sons[i].sons[2].kind == nkObjectTy: 
        fixRecordDef(n.sons[i].sons[2])
  else: discard

proc parseBegin(p: var TParser, result: PNode) = 
  getTok(p)
  while true: 
    case p.tok.xkind
    of pxComment: addSon(result, parseStmt(p))
    of pxSymbol: 
      if not fixExit(p, result): addSon(result, parseStmt(p))
    of pxEnd: 
      getTok(p)
      break 
    of pxSemicolon: getTok(p)
    of pxEof: parMessage(p, errXExpected, "expression")
    else: 
      var a = parseStmt(p)
      if a.kind != nkEmpty: addSon(result, a)
  if sonsLen(result) == 0: addSon(result, newNodeP(nkNilLit, p))

proc parseInherited(p: var TParser): PNode =
  # if p.outerProc != nil: echo p.outerProc.sons[0].ident.s, " ", p.outerProc.sons[3].treeRepr
  eat(p, pxInherited)
  var isMethod = p.methods.hasKey(genSignature(p.outerProc))
  var parent = newNodeP(nkCall, p).add(
      newIdentNameNodeP("inherited", p),
      newIdentNameNodeP("self", p)
    )
  if p.tok.xkind == pxSemiColon:
    result = newNodeP(nkCall, p).add(
        newNodeP(nkDotExpr, p).add(
          parent,
          newIdentNodeP(p.outerProc.sons[0].ident, p)
        )
      )
    let params = p.outerProc.sons[3]
    for i in 2..< params.len: # skip ReturnTy and self
      # echo params[i].treeRepr
      if params[i].kind == nkIdentDefs: 
        result.addSon(params[i][0])
  else:
    var call = parseStmt(p)
    result = newNodeP(nkCall, p).add(
      if call.kind == nkCall: call[0] else: call,
      parent,
    )
    if call.kind == nkCall:
      for i in 1..< call.len:
        result.addSon(call[i])
  if isMethod :
    result = newNodeP(nkCommand, p).add(
      newIdentNameNodeP("procCall", p),
      result
    )

proc parseStmt(p: var TParser): PNode = 
  var oldcontext = p.context
  p.context = conStmt
  result = emptyNode
  case p.tok.xkind
  of pxBegin: 
    result = newNodeP(nkStmtList, p)
    parseBegin(p, result)
  of pxCommand: result = parseCommand(p)
  of pxCurlyDirLe, pxStarDirLe: 
    if isHandledDirective(p): result = parseDirective(p)
  of pxIf: result = parseIf(p)
  of pxWhile: result = parseWhile(p)
  of pxRepeat: result = parseRepeat(p)
  of pxCase: result = parseCase(p)
  of pxTry: result = parseTry(p)
  of pxProcedure, pxFunction, pxConstructor, pxDestructor: 
    result = parseRoutine(p, false)
  of pxType: result = parseTypeSection(p)
  of pxConst: result = parseConstSection(p)
  of pxVar: result = parseVar(p)
  of pxFor: 
    result = parseFor(p)
    fixVarSection(p, result.sons[0])
  of pxRaise: result = parseRaise(p)
  of pxUses: result = parseUsesStmt(p)
  of pxProgram, pxUnit, pxLibrary: 
    # skip the pointless header
    while not (p.tok.xkind in {pxSemicolon, pxEof}): getTok(p)
    getTok(p)
  of pxInitialization: getTok(p) # just skip the token
  of pxImplementation: 
    p.section = seImplementation
    p.visibility = visPrivate
    result = newNodeP(nkCommentStmt, p)
    result.comment = "# implementation"
    getTok(p)
  of pxInterface: 
    p.section = seInterface
    p.visibility = visPublic
    getTok(p)
  of pxComment: 
    result = newNodeP(nkCommentStmt, p)
    skipCom(p, result)
  of pxSemicolon: getTok(p)
  of pxSymbol: 
    if p.tok.ident.id == getIdent(identCache, "break").id: 
      result = newNodeP(nkBreakStmt, p)
      getTok(p)
      skipCom(p, result)
      addSon(result, emptyNode)
    elif p.tok.ident.id == getIdent(identCache, "continue").id: 
      result = newNodeP(nkContinueStmt, p)
      getTok(p)
      skipCom(p, result)
      addSon(result, emptyNode)
    elif p.tok.ident.id == getIdent(identCache, "exit").id: 
      result = newNodeP(nkReturnStmt, p)
      getTok(p)
      skipCom(p, result)
      addSon(result, emptyNode)
    else: 
      result = parseExprStmt(p)
  of pxDot: getTok(p) # BUGFIX for ``end.`` in main program
  of pxInherited: result = parseInherited(p)
  else: result = parseExprStmt(p)
  opt(p, pxSemicolon)
  if result.kind != nkEmpty: skipCom(p, result)
  p.context = oldcontext

proc genConstructorTemplate(p: var TParser, n: PNode): PNode =
  var params = newNodeP(nkFormalParams, p).add(
      newIdentNameNodeP("untyped", p),
      newNodeP(nkIdentDefs, p).add(
        newIdentNameNodeP("T", p),
        newNodeP(nkBracketExpr, p).add(
          newIdentNameNodeP("typedesc", p),
          n[3][0]
        ),
        emptyNode
      )
    )
  var call = newNodeP(nkCall, p).add(
      newIdentNodeP(getIdentOfNode(n), p),
      newNodeP(nkCall, p).add(
        newIdentNameNodeP("new", p),
        newIdentNameNodeP("T", p)
      )
    )
  for i in 2..< n[3].len :
    params.addSon(n[3][i])
    call.addSon(newIdentNodeP(n[3][i][0].ident, p))
  result = newNodeP(nkTemplateDef, p).add(
      n[0],
      n[1],
      n[2],
      params,
      emptyNode,
      emptyNode,
      newNodeP(nkStmtList, p).add(
        newNodeP(nkCast, p).add(
          newNodeP(nkDotExpr, p).add(
            newIdentNameNodeP("T", p),
            newIdentNameNodeP("type", p)
          ),
          call
        )
      )
    )

proc handleExtra(p: var TParser, node: var PNode) =
  var later: seq[TExtraStmt] 
  for e in p.extra:
    if (p.section == seInterface and eiPublic in e.flags) or p.section == seImplementation:
      node.addSon(e.node)
      if eiConstructor in e.flags:
        node.addSon(genConstructorTemplate(p, e.node))
      # echo e.node.treeRepr
    else: 
      later.add(e)
  p.extra = later

proc parseUnit(p: var TParser): PNode = 
  result = newNodeP(nkStmtList, p)
  getTok(p)                   # read first token
  while true: 
    case p.tok.xkind
    of pxEof, pxEnd: break 
    of pxBegin: parseBegin(p, result)
    of pxCurlyDirLe, pxStarDirLe: 
      if isHandledDirective(p): addSon(result, parseDirective(p))
      else: parMessage(p, errGenerated, p.tok.ident.s & " not allowed here")
    else: 
      addSon(result, parseStmt(p))
      handleExtra(p, result)
  opt(p, pxEnd)
  opt(p, pxDot)
  if p.tok.xkind != pxEof: 
    addSon(result, parseStmt(p)) # comments after final 'end.'
  
