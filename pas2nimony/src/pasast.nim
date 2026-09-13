#
#           pas2nimony - Pascal to Nimony translator
#        (c) Copyright 2025 Adrian  (based on pas2nim by A. Rumpf)
#
# Compact AST owned by the pas2nimony tool. Unlike the old pas2nim, this
# AST is not the Nim compiler's PNode: it only models what the translator
# emits and is compiled by the nimony toolchain itself.

import std/strutils

type
  TLineInfo* = object
    line*: int
    col*: int
    file*: string

  TSection* = enum
    seInterface, seImplementation

  TVisibility* = enum
    visPublic, visPrivate, visProtected, visPublished

  TParserFlag* = enum
    pfRefs,             ## use "ref" instead of "ptr" for Pascal's ^typ
    pfNoSelfQualify,    ## disable the self-qualification pass
    pfNoCaseCanon,      ## disable case canonicalization
    pfNoInit,           ## do not add explicit initializers to Pascal locals
    pfV2                ## emit {.feature: "v2".} instead of lenientnils
    pfStrictDirectives  ## unfulfillable directives/calling conventions
                        ## become errors instead of warnings

  NodeKind* = enum
    # literals & atoms
    nkEmpty, nkIdent, nkIntLit, nkInt64Lit, nkFloatLit, nkStrLit, nkCharLit,
    nkNilLit, nkTrue, nkFalse,
    # expressions
    nkCall, nkCommand, nkInfix, nkPrefix, nkPostfix, nkDotExpr, nkIndexExpr,
    nkPar, nkBracket, nkCurly, nkRange, nkCast, nkAddr, nkDeref,
    nkWhenExpr, nkCompiles,
    # statements
    nkStmtList, nkAsgn, nkIfStmt, nkElifBranch, nkElse, nkWhileStmt,
    nkCaseStmt, nkOfBranch, nkTryStmt, nkExceptBranch, nkFinally,
    nkForStmt, nkRaiseStmt, nkBreakStmt, nkContinueStmt, nkReturnStmt,
    nkGotoStmt, nkLabeledStmt, nkBlockStmt,
    nkVarSection, nkConstSection, nkTypeSection, nkCommentStmt,
    nkDiscardStmt,
    # declarations
    nkProcDef, nkFuncDef, nkMethodDef, nkTemplateDef, nkFormalParams,
    nkParam, nkTypeDef, nkPragma, nkPragmaExpr, nkPostfixExport,
    nkEnumTy, nkEnumFieldDef, nkObjectTy, nkRefTy, nkPtrTy, nkProcTy,
    nkDistinctTy, nkSetTy, nkArrayTy, nkRangeTy, nkOpenArrayTy, nkVarTy,
    nkSeqTy,
    nkOfInherit, nkIdentDefs, nkRecList, nkRecCase, nkGenericParams,
    nkImportStmt, nkTypeOfTy, nkStaticTy,
    nkOconstr
    ## object/record constructor `TRec(Key: v; Link: w)`, parsed from a
    ## Pascal typed record constant. Kept distinct from `nkPar`: the NIF
    ## shape is `(oconstr TY (kv NAME VAL) ...)`, and rendering it as a
    ## plain parenthesized list loses the field bindings (`kv(Name, v)`
    ## is not a valid Nim constructor). Appended at the END of the enum
    ## so no existing ordinal shifts.

  Node* = ref object
    kind*: NodeKind
    info*: TLineInfo
    sons*: seq[Node]
    strVal*: string     ## identifier spelling / string literal content
    intVal*: int64
    floatVal*: float64
    ## pragma/visibility/extra payload for definitions:
    flags*: set[TVisibility]
    isCtor*: bool       ## routine is a Pascal constructor
    defClass*: string   ## spelling of the owning class (methods)
    noQualCallee*: bool ## do not self-qualify the callee (inherited calls)
    exported*: bool     ## interface-section name: emit an export marker
    isRecordType*: bool ## type came from the Pascal `record` keyword

proc `[]`*(n: Node; i: int): Node {.inline.} = n.sons[i]
proc `[]=`*(n: Node; i: int; c: Node) {.inline.} = n.sons[i] = c
proc len*(n: Node): int {.inline.} = n.sons.len

proc add*(father: Node; child: Node) {.inline.} =
  father.sons.add(child)

proc newNode*(kind: NodeKind; info: TLineInfo): Node =
  Node(kind: kind, info: info)

proc newIdentNode*(name: string; info: TLineInfo): Node =
  Node(kind: nkIdent, info: info, strVal: name)

proc newIntNode*(kind: NodeKind; v: int64; info: TLineInfo): Node =
  Node(kind: kind, info: info, intVal: v)

proc newFloatNode*(v: float64; info: TLineInfo): Node =
  Node(kind: nkFloatLit, info: info, floatVal: v)

proc newStrNode*(s: string; info: TLineInfo): Node =
  Node(kind: nkStrLit, info: info, strVal: s)

proc newCharNode*(s: string; info: TLineInfo): Node =
  Node(kind: nkCharLit, info: info, strVal: s)

proc emptyNode*(info: TLineInfo): Node =
  newNode(nkEmpty, info)

proc `$`*(n: Node): string =
  ## debugging helper (shallow)
  case n.kind
  of nkIdent: result = n.strVal
  of nkIntLit, nkInt64Lit: result = $n.intVal
  of nkFloatLit: result = $n.floatVal
  of nkStrLit: result = "\"" & n.strVal & "\""
  of nkCharLit: result = "'" & n.strVal & "'"
  else: result = $n.kind

proc renderInfo*(info: TLineInfo): string =
  result = info.file & "(" & $info.line & "," & $info.col & ")"