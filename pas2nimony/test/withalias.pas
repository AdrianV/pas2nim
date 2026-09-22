program WithAlias;
(* `with r[i] do` where `r` has a NAMED array type (`TArr =
  array[..] of TRec`). The parser records the element spelling of the
  alias in `arrayAliases` for properties, but a plain variable of that
  alias never reached `arrayVarElems`, so the with-base classified as
  an unknown class: the body's field names stayed unqualified and
  nimony rejected them ("undeclared identifier: Node/Value/Key" in
  a corpus include's `with n.Items[x] do`).

  The with-body now qualifies through a hidden ADDRESS temp
  (`var w = addr(r[1])`): the base is evaluated once and writes reach
  the original element (a plain value temp would swallow the write). *)

type
  TRec = record
    A: Integer;
    B: string;
  end;
  TArr = array[0..2] of TRec;

var
  r: TArr;

begin
  r[1].A := 5;
  r[1].B := 'xy';
  with r[1] do
  begin
    A := A + 1;
    B := B + 'yz';
  end;
  writeln('a=', r[1].A, ' b=', r[1].B);
end.
