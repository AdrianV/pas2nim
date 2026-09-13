program AbsoluteAlias;
{ A second name for an existing variable. Delphi's `absolute` on a
  variable (no address form is used by the corpus). Nim has no
  `absolute`, so the alias lowers to a template whose body is the
  target, which reads and writes through to it - at module and local
  scope alike. }
var
  Target: Integer;
  Alias: Integer absolute Target;

procedure Bump;
var
  Local: Integer;
  Other: Integer absolute Local;
begin
  Local := 10;
  Other := Other + 5;
  writeln('local=', Local);
end;

begin
  Target := 41;
  Alias := Alias + 1;
  writeln('alias=', Target);
  Bump;
end.
