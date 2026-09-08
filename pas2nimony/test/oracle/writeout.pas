program WriteoutOracle;
{ M5-2b oracle: writeln/write width syntax - :w and :w:p forms }
uses SysUtils;
var
  i: Integer;
  x, y: Double;
  s: string;
begin
  i := 42;
  x := 2.5;
  y := -1234.5678;
  s := 'hi';
  writeln('1=[', i:8, ']');
  writeln('2=[', x:8, ']');
  writeln('3=[', -2.5:10:3, ']');
  writeln('4=[', 255:5, ']');
  writeln('5=[', 7:3, 8:4, ']');
  writeln('6=[', y:12, ']');
  writeln('7=[', y:12:2, ']');
  write('8=[', s:6, ']');
  writeln('[', x:9:2, ']');
  writeln('9=[', x:14, ']');
  x := 1.0e15;
  writeln('B=[', x:14, ']');
  x := 1.0e-10;
  writeln('C=[', x:14, ']');
  x := 123.456;
  writeln('D=[', x:8, ']');
  Str(x:12, s);
  writeln('E=[', s, ']');
  x := 0.0;
  writeln('F=[', x:8, ']');
end.
