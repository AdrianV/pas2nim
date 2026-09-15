program aedge;

{ Empty/nil, SetLength growth and shrink, Delete/Insert/Copy. }

var
  s: AnsiString;

begin
  s := '';
  WriteLn(Length(s));
  s := s + 'a';
  WriteLn(s, ' ', Length(s));
  SetLength(s, 4);
  WriteLn(Length(s));
  SetLength(s, 1);
  WriteLn(s);
  s := 'abcdef';
  Delete(s, 2, 2);
  WriteLn(s);
  Insert('XY', s, 2);
  WriteLn(s);
  WriteLn(Copy(s, 2, 3));
  WriteLn(Pos('cd', s));
end.
