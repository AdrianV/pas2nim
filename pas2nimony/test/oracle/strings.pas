program StringsOracle;
{ M5 oracle: SysUtils string/conversion surface, deterministic }
uses SysUtils;
var
  s: string;
begin
  writeln('i2s=', IntToStr(42), ' ', IntToStr(-7), ' ', IntToStr(1234567));
  writeln('s2i=', StrToInt('123'), ' ', StrToIntDef('x', 9), ' ', StrToIntDef('-5', 0));
  writeln('hex=', IntToHex(255, 4), ' ', IntToHex(4096, 2), ' ', IntToHex(0, 1));
  writeln('trim=[', Trim('  x y  '), '][', TrimLeft('  x'), '][', TrimRight('x  '), ']');
  writeln('up=', UpperCase('miXeD'), ' low=', LowerCase('MiXeD'));
  writeln('cmp=', CompareText('Abc', 'aBd'), ' ', CompareStr('abc', 'abd'));
  s := 'Hello World';
  writeln('pos=', Pos('World', s), ' pos2=', Pos('o', s), ' pos0=', Pos('zz', s));
  writeln('copy=', Copy(s, 7, 5), '|', Copy(s, 1, 5), '|', Copy(s, 20, 5), '|');
  writeln('len=', Length(s), ' soc=', StringOfChar('a', 3));
  writeln('ansipos=', AnsiPos('iss', 'Mississippi'));
  writeln('del=', s, ' ', s + '!');
end.
