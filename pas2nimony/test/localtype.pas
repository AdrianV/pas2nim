program LocalType;
(* A routine-LOCAL `type` section. nimony allows `type` inside a proc,
  but pas2nimony's statement renderer used to discard nkTypeSection,
  silently dropping the declaration and leaving every use undeclared
  (tplbtree.inc declares `RPath` locally inside InternalDelete/
  InternalPut). Module-level type sections were always emitted; only
  the in-procedure case was lost. *)

type
  PRec = ^TRec;
  TRec = record
    A: Integer;
    Next: PRec;
  end;

function Make(A: Integer): TRec;
type
  TLocal = record
    X, Y: Integer;
  end;
var
  L: TLocal;
begin
  L.X := A;
  L.Y := A + 1;
  Result.A := L.X + L.Y;
  Result.Next := nil;
end;

var
  r: TRec;

begin
  r := Make(5);
  writeln('a=', r.A);
end.
