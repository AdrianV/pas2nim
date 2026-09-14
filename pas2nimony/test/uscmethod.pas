program UscMethod;
(* A leading-underscore METHOD name reached through `inherited`:

    inherited _load(pItem);

  The inherited path deliberately keeps the member spelling instead of
  running the canon (so the RTL builtin map cannot rename `Insert` to the
  string shim `strInsert`) - but that also skipped the underscore escape,
  so the CALL SITE emitted `_load` while the DEFINITION was escaped to
  `pas_load`. nifler then rejected the unit with "invalid token: _". *)

type
  TBase = class
    procedure _load(x: Integer);
  end;

  TChild = class(TBase)
    procedure Run;
  end;

procedure TBase._load(x: Integer);
begin
  writeln('load ', x);
end;

procedure TChild.Run;
begin
  inherited _load(5);
end;

var
  c: TChild;

begin
  c := TChild.Create;
  c.Run;
end.
