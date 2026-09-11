program vlitint;
{$APPTYPE CONSOLE}
{ Variant conformance probe: how an *integer literal* is typed, and whether
  that typing survives into a variable and into an operation.

  Measured: dcc32 uses the narrowest type that holds the value, preferring
  the unsigned Byte/Word/LongWord (3 -> varByte, 300 -> varWord,
  70000 -> varLongWord, -70000 -> varInteger); FPC 3.2.2 uses
  ShortInt/SmallInt/Integer. We follow dcc32 - see variant/vlitint.expect. }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

procedure T(const tag: string; const x: Variant);
begin
  WriteLn(tag, ' type=', Hex(VarType(x)));
end;

var
  v, w: Variant;
  i: Integer;
begin
  DecimalSeparator := '.';

  { literals, to find where the boundary is }
  T('lit 3         ', 3);
  T('lit -3        ', -3);
  T('lit 300       ', 300);
  T('lit 32767     ', 32767);
  T('lit 32768     ', 32768);
  T('lit 70000     ', 70000);
  T('lit 3000000000', 3000000000);
  T('lit -70000    ', -70000);

  { typed operands at the same values }
  i := 70000;      T('int 70000     ', i);
  i := 32768;      T('int 32768     ', i);

  { does the literal's type survive into a variable, and into an operation? }
  v := 70000; w := v + 1;
  WriteLn('V(70000lit)+1 type=', Hex(VarType(w)), ' val=[', VarToStr(w), ']');
  v := 70000; w := v;
  WriteLn('assign lit to var type=', Hex(VarType(w)));
end.
