program DotAncestor;
(* A class may name its ancestor with the UNIT qualifier, and the corpus
  does exactly that inside the unit itself:

    TaStringList = class(vStrLst.TaTemplateList)

  The ancestor registry is keyed by the bare type name, so a qualified
  parent used to be recorded as NO parent. `inherited IndexOf(x)` then
  took the "no ancestor" path and emitted a cast with an empty target,
  which nifler rejects: `Find(cast[](self), s, Index)`.

  The qualifier is stripped here, so the parent call binds to TParent. *)

type
  TParent = class
    function Get: Integer;
  end;

  TChild = class(DotAncestor.TParent)
    function Get: Integer;
  end;

function TParent.Get: Integer;
begin
  result := 40;
end;

function TChild.Get: Integer;
begin
  result := inherited Get;
end;

var
  c: TChild;

begin
  c := TChild.Create;
  writeln('g=', c.Get);
end.
