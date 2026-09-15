program acow;

{ Copy-on-write is not directly observable in Pascal, but mutation
  independence is: after `b := a` a write through b must not change a. }

var
  a, b: AnsiString;

begin
  a := 'original';
  b := a;
  b[1] := 'X';
  WriteLn(a);
  WriteLn(b);
  a[3] := 'Z';
  WriteLn(a);
  WriteLn(b);
  b := b + '!';
  WriteLn(a);
  WriteLn(b);
end.
