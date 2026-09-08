program StrconvOracle;
{ M5-2 oracle: Str, Val, FormatFloat - FPC conversion semantics }
uses SysUtils;
var
  s: string;
  w, ec: Integer;
  d: Double;
begin
  Val('123', w, ec);        writeln('1=', w, ' ', ec);
  Val('12x', w, ec);        writeln('2=', w, ' ', ec);
  Val(' 12', w, ec);        writeln('3=', w, ' ', ec);
  Val('-7', w, ec);         writeln('4=', w, ' ', ec);
  Val('+7', w, ec);         writeln('5=', w, ' ', ec);
  Val('', w, ec);           writeln('6=', w, ' ', ec);
  Val('1.5e2', d, ec);      writeln('7=', Format('%g', [d]), ' ', ec);
  Val('2.5x', d, ec);       writeln('8=', Format('%g', [d]), ' ', ec);
  Val('$1F', w, ec);        writeln('9=', w, ' ', ec);
  Str(42, s);               writeln('A=', s);
  Str(-42:6, s);            writeln('B=[', s, ']');
  Str(3.25:5:2, s);         writeln('C=[', s, ']');
  Str(-3.25:0:3, s);        writeln('D=[', s, ']');
  writeln('F1=', FormatFloat('0.00', 2.5));
  writeln('F2=', FormatFloat('0.###', 2.5));
  writeln('F3=', FormatFloat('0', 2.5));
  writeln('F4=', FormatFloat('#.##', 0.5));
  writeln('F5=', FormatFloat('0.00', -0.005));
  writeln('F6=', FormatFloat('0.00', 0.0));
  writeln('F7=', FormatFloat('#,##0', 1234567.0));
  writeln('F8=', FormatFloat('0.00E+00', 12345.0));
  writeln('F9=', FormatFloat('0', -0.4));
  writeln('F9b=', FormatFloat('0', 0.5));
  writeln('F10=', FormatFloat('0.00', 1.0e15));
end.
