program LocalIf;
(* A forwarded `{$if}` may also sit between a routine's LOCAL DECLARATIONS,
  before its `begin`: template bodies in the corpus guard a local constant
  or variable that way. The branch cannot be chosen by a front end
  (`declared()` needs semantics), so the group is forwarded as a Nim
  `when` whose arms hold the declarations themselves - and the routine
  body must not end early at the directive.

  Both spellings of the closer appear, and the `var` section after the
  group exercises the return to local declarations. *)

const
  BaseSpan = 3;

function Span: Integer;
{$if not declared(LocalExtra)}
const
  LocalExtra = 4;
{$ifend}
{$if not declared(LocalVar)}
var
  LocalVar: Integer;
{$ifend}
var
  i: Integer;
begin
  LocalVar := 5;
  i := LocalVar;
  result := i + LocalExtra + BaseSpan;
end;

begin
  writeln('span=', Span);
end.
