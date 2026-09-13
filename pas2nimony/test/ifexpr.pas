program IfExpr;
{ An IF directive may guard a real EXPRESSION, and a frontend cannot
  answer it: sizeof(Pointer) depends on the target's data model, and
  declared() needs semantics. pas2nimony therefore forwards the whole
  group to Nim as a `when` and lets a real compiler pick the branch -
  which is why the Pascal condition has to be a valid expression and not
  a source-level hack. Delphi 2007 closes such a group with IFEND;
  newer compilers accept ENDIF as well. Both spellings appear below. }

{$if sizeof(Pointer) = 8}
const PTR_BITS = 64;
{$else}
const PTR_BITS = 32;
{$ifend}

{$if sizeof(Pointer) = 4}
const PTR_WIDE = 0;
{$elseif sizeof(Pointer) = 8}
const PTR_WIDE = 1;
{$else}
const PTR_WIDE = 2;
{$endif}

begin
  writeln('bits=', PTR_BITS, ' wide=', PTR_WIDE);
end.
