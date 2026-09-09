program Shadow;
type
  TBase = class
    procedure Foo; virtual;
  end;
  TChild = class(TBase)
    procedure Foo; reintroduce; virtual;   { NEW virtual slot - hides }
  end;
  TChild2 = class(TBase)
    procedure Foo; override;               { same slot - overrides }
  end;
procedure TBase.Foo;
begin
  writeln('base');
end;
procedure TChild.Foo;
begin
  writeln('child-newslot');
end;
procedure TChild2.Foo;
begin
  writeln('child2');
end;
var
  b: TBase;
  c: TChild;
begin
  c := TChild.Create;
  c.Foo;   { child-typed: the NEW slot's method }
  b := TChild.Create;
  b.Foo;   { Delphi: 'base' - TChild's virtual is a NEW slot, not an override }
  b := TChild2.Create;
  b.Foo;   { Delphi: 'child2' - override }
end.
