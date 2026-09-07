program Samples;
{ exercises: case-insensitive uses, sets, control flow, strings,
  records, enums, arrays, case statements, for/while/repeat }

type
  TColor = (clRed, clGreen, clBlue);
  TPoint = record
    X, Y: Integer;
  end;

var
  I, Total: Integer;
  S, Acc: String;
  C: Char;
  Col: TColor;
  Pt: TPoint;
  Arr: array[0..4] of Integer;
  MySet: set of Byte;

begin
  // for loops with case-different identifiers
  Total := 0;
  for i := 1 to 10 do
    Total := Total + I;
  write('total=', total);
  writeln('');

  // while
  I := 10;
  while I > 0 do
    I := I - 3;
  writeln('while left: ', I);

  // repeat
  I := 0;
  repeat
    I := I + 2;
  until I >= 7;
  writeln('repeat: ', I);

  // sets
  MySet := [1, 3, 5, 7, 9];
  if 5 in MySet then writeln('5 in set');
  if not (2 in MySet) then writeln('2 not in set');
  Include(MySet, 2);
  if 2 in myset then writeln('2 now in set');
  Exclude(MySet, 9);
  if not (9 in MySet) then writeln('9 removed');

  // case statement on enum and int
  Col := clGreen;
  case col of
    clRed: writeln('red');
    clGreen: writeln('green');
    clBlue: writeln('blue');
  end;
  I := 4;
  case I of
    0..3: writeln('low');
    4..7: writeln('mid');
  else
    writeln('high');
  end;

  // strings and chars (Delphi 1-based indexing)
  s := 'Hello';
  acc := s + ' ' + 'World';
  writeln(acc);
  c := acc[1];
  writeln('char: ', C);
  writeln('upper: ', Uppercase(S));
  writeln('len: ', Length(s));
  writeln('pos: ', Pos('World', acc));
  writeln('pos0: ', Pos('zzz', acc));
  writeln('copy: ', Copy(acc, 2, 5));
  s[1] := 'J';
  writeln('set1: ', s);
  Delete(acc, 1, 6);
  writeln('del: ', acc);
  Insert('Well ', acc, 1);
  writeln('ins: ', acc);

  // records
  pt.x := 3;
  PT.Y := Pt.X * 2;
  writeln('point: ', pt.x, ',', pt.y);

  // arrays
  for i := 0 to 4 do
    arr[i] := i * i;
  writeln('arr[3] = ', ARR[3]);

  // downto and char ranges
  for I := 5 downto 1 do
    write(I, ' ');
  writeln('');
  for c := 'a' to 'c' do
    write(C);
  writeln('');
  writeln('done');
end.
