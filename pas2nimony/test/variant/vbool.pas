program vbool;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 6: a WordBool payload in arithmetic.

  The union view of a `True` variant is 65535 (vcase.pas pins that), but
  *arithmetic* converts the WordBool to -1: `True + 1` is 0 in both oracles.
  The tag the promotion lands on diverges when a real is involved - FPC gives
  varDouble (0.5), dcc32 varCurrency (0) - so this sample follows FPC
  (vbool.expect) and the Delphi leg is reported as INFO.

  Reads use the payload, never VarToStr, for the real case: the text would be
  locale-formatted by both oracles and the separator is not comparable. }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

var
  b: Boolean; v: Variant;
begin
  DecimalSeparator := '.';
  b := True;

  v := b;
  WriteLn('bool view VInteger=', TVarData(v).VInteger);

  v := b;
  v := v + 1;
  WriteLn('bool+1=', VarToStr(v), ' type=', Hex(VarType(v)));

  v := b;
  v := v + 1.5;
  WriteLn('bool+1.5x100=', Round(TVarData(v).VDouble * 100),
          ' type=', Hex(VarType(v)));

  b := False;
  v := b;
  v := v + 1;
  WriteLn('false+1=', VarToStr(v), ' type=', Hex(VarType(v)));
end.
