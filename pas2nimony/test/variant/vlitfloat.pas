program vlitfloat;
{$APPTYPE CONSOLE}
{ Variant conformance probe: how a *decimal literal* is typed.

  Measured divergence: dcc32 stores 1.5 / 3.0 as varCurrency (0006) and only
  falls back to varDouble for values Currency cannot hold (1e30); FPC 3.2.2
  stores both as varDouble (0005). We currently follow FPC, because nimony
  unifies `int` and `int64` and does not apply converters, so a Nim float64
  cannot tell an nkFloatLit from a typed Double - the emitter would have to
  route float literals through the shim to reproduce dcc32 here.
  See variant/vlitfloat.expect and the wiki's open decisions. }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

procedure T(const tag: string; const x: Variant);
begin
  WriteLn(tag, ' type=', Hex(VarType(x)), ' val=[', VarToStr(x), ']');
end;

var
  d: Double;
  v: Variant;
begin
  DecimalSeparator := '.';

  T('lit 1.5       ', 1.5);
  T('lit 3.0       ', 3.0);
  T('lit 1e30      ', 1e30);

  { the typed counterpart, which both oracles agree on }
  d := 1.5; T('dbl 1.5       ', d);
  d := 3.0; T('dbl 3.0       ', d);

  { a literal's tag seen through an assignment }
  v := 1.5;
  WriteLn('assign lit 1.5 type=', Hex(VarType(v)), ' val=[', VarToStr(v), ']');
end.
