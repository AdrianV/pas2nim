# borrowed from macros.nim

import ast

proc treeRepr*(n: PNode): string =
  ## Convert the AST `n` to a human-readable tree-like string.
  ##
  ## See also `repr` and `lispRepr`.
  proc traverse(res: var string, level: int, n: PNode) =
    for i in 0..level-1: res.add "  "
    res.add(($n.kind).substr(2))

    case n.kind
    of nkEmpty: discard # same as nil node in this representation
    of nkNilLit: res.add(" nil")
    of nkCharLit..nkInt64Lit: res.add(" " & $n.intVal)
    of nkFloatLit..nkFloat64Lit: res.add(" " & $n.floatVal)
    of nkStrLit..nkTripleStrLit: res.add(" " & $n.strVal)
    of nkIdent: res.add(" !\"" & n.ident.s & '"')
    of nkSym: res.add(" \"" & $n.sym.id & '"')
    of nkNone: assert false
    else:
      for j in 0..n.len-1:
        res.add "\n"
        traverse(res, level + 1, n[j])

  result = ""
  traverse(result, 0, n)

