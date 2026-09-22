program amove;
var
  s, t: AnsiString;
  buf: array[0..7] of Byte;
  p: Pointer;
  c: PAnsiChar;
  i: Integer;
begin
  s := 'foo bar';
  t := s;
  Move(s[2], t[1], 3);
  WriteLn('A:', s, '|', t);
  FillChar(s[1], 3, 'x');
  WriteLn('B:', s, '|', t);
  for i := 0 to 7 do buf[i] := Byte(65 + i);
  p := @buf[0];
  t := '........';
  Move(p^, t[1], 4);
  WriteLn('C:', t, '|', s);
  FillChar(p^, 2, 0);
  WriteLn('D:', Integer(buf[0]), ' ', Integer(buf[1]), ' ', Integer(buf[2]));
  s := 'abcdef';
  c := PAnsiChar(Pointer(s));
  Move(c^, t[1], 3);
  WriteLn('E:', t);
end.
