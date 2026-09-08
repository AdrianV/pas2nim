program MathOracle;
{ M5 oracle: Math surface (floats only via FloatToStr/Format) }
uses SysUtils, Math;
begin
  writeln('rt=', FloatToStr(RoundTo(2.5, 0)), ' ', FloatToStr(RoundTo(3.5, 0)),
    ' ', FloatToStr(RoundTo(-2.5, 0)), ' ', FloatToStr(RoundTo(123.45, -1)));
  writeln('srt=', FloatToStr(SimpleRoundTo(2.675, -2)),
    ' ', FloatToStr(SimpleRoundTo(1.5, 0)));
  writeln('fc=', Floor(3.7), ' ', Floor(-3.7), ' ', Ceil(3.2), ' ', Ceil(-3.2));
  writeln('pow=', FloatToStr(Power(2.0, 10.0)), ' ', FloatToStr(IntPower(2, 10)));
  writeln('hyp=', FloatToStr(Hypot(3.0, 4.0)), ' rad=', FloatToStr(DegToRad(90.0)));
  writeln('cv=', CompareValue(2, 5), ' ', CompareValue(5, 2), ' ', CompareValue(3, 3));
  writeln('sign=', Sign(-3.5), ' ', Sign(0.0), ' ', Sign(7));
  writeln('same=', SameValue(0.1 + 0.2, 0.3), ' ', SameValue(1.0, 2.0));
end.
