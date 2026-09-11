program vops2;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 2: the *promotion* type and value of mixed
  arithmetic. Every operand is a typed variable, so both oracles agree on
  the inputs; what is measured is what the operation yields.

  Measured: int op int -> varInteger, Int64 involved -> varInt64, a real
  mixed in -> varDouble, `/` always -> varDouble, and a string operand
  raises (EVariantTypeCastError in dcc32, EVariantError in FPC). }
uses SysUtils, Variants;

var
  raised: Boolean;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

{ the raise checks report only *that* something was raised: lowering `on E: T`
  to a `case` over the caught ErrorCode is a nimony toolchain defect today (the
  same one that blocks the corpus unit except.pas), so the probes stay with a
  plain except. The error *class* is measured separately - the oracles differ
  (dcc32 EVariantTypeCastError is a subclass of EVariantError, FPC's is not) -
  and our own hierarchy is pinned by test/variant/vshim.nim, which the same
  toolchain does compile. }
procedure Report(const what: string; wasRaised: Boolean);
begin
  if wasRaised then WriteLn(what);
  raised := False;
end;

var
  v, w: Variant;
  i: Integer; i64: Int64; d: Double; s: Single; c: Currency;
begin
  DecimalSeparator := '.';

  i := 1; v := i;

  i := 2; w := i;
  WriteLn('V(1)+V(2)   type=', Hex(VarType(v + w)),
          ' val=[', VarToStr(v + w), ']');

  d := 2.5; w := d;
  WriteLn('V(1)+V(dbl) type=', Hex(VarType(v + w)),
          ' val=[', VarToStr(v + w), ']');
  WriteLn('V(1)-V(dbl) type=', Hex(VarType(v - w)),
          ' val=[', VarToStr(v - w), ']');
  WriteLn('V(1)*V(dbl) type=', Hex(VarType(v * w)),
          ' val=[', VarToStr(v * w), ']');
  WriteLn('V(1)/V(dbl) type=', Hex(VarType(v / w)),
          ' val=[', VarToStr(v / w), ']');
  WriteLn('V(1)<V(dbl) type=', Hex(VarType(v < w)),
          ' val=[', VarToStr(v < w), ']');

  s := 2.5; w := s;
  WriteLn('V(1)+V(sgl) type=', Hex(VarType(v + w)),
          ' sgltype=', Hex(VarType(w)), ' val=[', VarToStr(v + w), ']');

  { dcc32 renders a Currency variant with the *system locale* even when
    DecimalSeparator is pinned, so the payload (scaled by 10000) is printed
    instead of the text }
  c := 2.5; w := c;
  WriteLn('V(cur)      type=', Hex(VarType(w)),
          ' curx10000=', TVarData(w).VInt64);
  w := v + w;
  WriteLn('V(1)+V(cur) type=', Hex(VarType(w)),
          ' sumx10000=', TVarData(w).VInt64);

  i64 := 3; w := i64;
  WriteLn('V(1)+V(i64) type=', Hex(VarType(v + w)),
          ' val=[', VarToStr(v + w), ']');

  { a string operand raises in every implementation; the *class* differs
    between the oracles (dcc32 EVariantTypeCastError, a subclass of
    EVariantError; FPC plain EVariantError) - see the header of vcore.pas for
    why the class is not observable here and where it is pinned instead }
  try
    w := 'x';
    { no WriteLn inside the try: with a partial write the two oracles emit
      different fragments before the raise aborts the line }
    v := v + w;
  except raised := True; end;
  Report('V(1)+V(str) raised', raised);
end.
