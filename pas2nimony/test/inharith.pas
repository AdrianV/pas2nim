program InhArith;
(* `inherited Member` may be one OPERAND of a larger expression:

    result := inherited Row + ',' + FieldF.AsString;

  (uTest1/uTest1b). The member name is the LEFTMOST leaf of the parsed
  expression, not a call of its own - so only that leaf may be rebound to
  the parent call. Treating the whole expression as "not a call" dropped
  every operand after the member reference and produced a wrong result. *)

type
  TBase = class
    function Row: string;
  end;

  TChild = class(TBase)
    function Row: string;
  end;

function TBase.Row: string;
begin
  result := 'base';
end;

function TChild.Row: string;
begin
  result := inherited Row + ',child';
end;

var
  c: TChild;

begin
  c := TChild.Create;
  writeln(c.Row);
end.
