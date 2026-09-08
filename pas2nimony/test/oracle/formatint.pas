program FormatIntOracle;
{ M5 oracle: integer/string Format surface }
uses SysUtils;
var
  b: Boolean;
begin
  writeln(Format('b=%s d=%d', [BoolToStr(b, true), 3]));
  writeln(Format('%d %d %d', [0, -7, 2147483647]));
  writeln(Format('%5d|%-5d|%05d', [42, 42, 42]));
  writeln(Format('%x %X %x', [255, 255, 10]));
  writeln(Format('%s|%10s|%-10s|', ['ab', 'ab', 'ab']));
  writeln(Format('%1:s and %0:s', ['second', 'first']));
  writeln(Format('100%% and %s', ['done']));
end.
