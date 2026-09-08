program ShimsTest;
{ M3-1: StrUtils + Math shim units. `uses StrUtils`/`uses Math`
  resolve to the pas2nimony shim units (passtrutils/pasmath), which
  layer the Delphi API over nimony's std plus small re-implementations
  (RoundTo banker's rounding, case-insensitive ops). Char overloads
  absorb nimony's missing char->string conversion for the idiomatic
  1-char Pascal literals. }
uses StrUtils, Math;
begin
  writeln('left=', LeftStr('Hello World', 5), ' right=', RightStr('Hello World', 5),
    ' mid=', MidStr('Hello World', 7, 5));
  writeln('posex=', PosEx('World', 'Hello World World', 7));
  writeln('repl=', AnsiReplaceStr('a-b-c', '-', '+'),
    ' repltext=', AnsiReplaceText('Abc abc ABC', 'abc', 'X'));
  writeln('rev=', ReverseString('abcdef'), ' dupe=', DupeString('ab', 3));
  writeln('sametext=', AnsiSameText('Hello', 'hello'),
    ' starts=', StartsText('He', 'Hello'), ' ends=', EndsText('lo', 'Hello'));
  writeln('ifthen=', IfThen(true, 'yes', 'no'));
  writeln('split0=', SplitString('a,b;c', ',;')[0], ' split2=', SplitString('a,b;c', ',;')[2]);
  writeln('floor=', Floor(3.7), ' ceil=', Ceil(3.2));
  writeln('power=', Power(2.0, 10.0), ' hypot=', Hypot(3.0, 4.0));
  writeln('roundto=', RoundTo(2.5, 0), ' ', RoundTo(3.5, 0), ' ', RoundTo(-2.5, 0),
    ' r123=', RoundTo(123.45, -1));
  writeln('simplert=', SimpleRoundTo(2.675, -2));
  writeln('compare=', CompareValue(2, 5), ' sign=', Sign(-3.5));
  writeln('degtorad=', DegToRad(90.0));
end.
