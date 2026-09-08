program OrdinalsOracle;
{ M5-2 oracle: ordinal ops, enums, bitwise, typed constants }
uses SysUtils;
type
  TColor = (clRed, clGreen, clBlue);
  TWeek = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
const
  MaxSize: Integer = 100;
  Answer: Integer = 42;
  PiSmall: Double = 3.14;
var
  c: TColor;
  t: TWeek;
  i, x: Integer;
  ch: Char;
begin
  writeln('ord=', Ord(clRed), ' ', Ord(clGreen), ' ', Ord(clBlue));
  writeln('succ=', Succ(clRed), ' pred=', Pred(clBlue), ' ', Ord(Succ(clRed)));
  writeln('high=', Ord(High(TColor)), ' low=', Ord(Low(TColor)));
  writeln('week=', Ord(Mon), ' ', Ord(Sun), ' high=', Ord(High(TWeek)));
  writeln('enum=', clGreen, ' ', Sat);
  c := clBlue;
  writeln('var-enum=', c);
  c := Pred(c);
  writeln('after-pred=', c);
  writeln('chr=', Chr(65), ' ', Chr(97), ' ord=', Ord('A'), ' ', Ord('z'));
  ch := 'm';
  writeln('ord-var=', Ord(ch), ' up=', UpCase(ch));
  writeln('bits=', 5 and 3, ' ', 5 or 2, ' ', 5 xor 3, ' ', not 0 and 255);
  writeln('shift=', 1 shl 10, ' ', 1024 shr 3, ' ', -16 shr 2);
  writeln('div2=', 7 div 2, ' ', 7 mod 2, ' ', (-7) mod 2);
  writeln('const=', MaxSize, ' ', Answer, ' ', Format('%.1f', [PiSmall]));
  x := MaxSize + Answer;
  writeln('calc=', x);
  writeln('bool=', true, ' ', false, ' ', true and false, ' ', not true);
  i := 10;
  inc(i);
  dec(i, 3);
  writeln('incdec=', i);
  writeln('odd-even=', Odd(7), ' ', Odd(8));
end.
