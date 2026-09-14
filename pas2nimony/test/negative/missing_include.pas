program MissingInclude;
// A REAL `{$I file}` / `{$INCLUDE file}` whose file ships nowhere must be
// a hard error. The old behaviour silently skipped it, which dropped every
// declaration the include carried and pushed the failure to the first use
// site - an "undeclared identifier" far away from the typo. This is the
// minimal reproduction that guards the refusal.
type
{$I negative_definitely_missing.inc}
  TAfter = Byte;
begin
  writeln(SizeOf(TAfter));
end.
