program StrUtilsOracle;
{ M5 oracle: StrUtils surface }
uses SysUtils, StrUtils;
var
  parts: TStringArray; { FPC SplitString returns TStringArray }
begin
  writeln('l=', LeftStr('Hello World', 5), ' r=', RightStr('Hello World', 5),
    ' m=', MidStr('Hello World', 7, 5));
  writeln('posex=', PosEx('World', 'Hello World World', 7), ' ',
    PosEx('World', 'Hello World', 12));
  writeln('rep=', AnsiReplaceStr('a-b-c', '-', '+'));
  writeln('rept=', AnsiReplaceText('Abc abc', 'abc', 'X'));
  writeln('rev=', ReverseString('abcdef'), ' dup=', DupeString('ab', 3));
  writeln('same=', AnsiSameText('Hello', 'hello'), ' starts=',
    StartsText('He', 'Hello'), ' ends=', EndsText('lo', 'Hello'));
  parts := SplitString('a,b,c', ',');
  writeln('split=', parts[0], ' ', parts[1], ' ', parts[2]);
  writeln('ifthen=', IfThen(true, 'yes', 'no'));
end.
