program aconst;

{ Typed const AnsiString: a read-only literal in static storage
  (StrRec.refCnt = -1). A write to a copy must detach, never corrupt
  the const. }

const
  Greeting: AnsiString = 'hello';
  Empty: AnsiString = '';
  OneChar: AnsiString = 'x';
  Concat: AnsiString = 'ab' + 'cd';

var
  s: AnsiString;

begin
  WriteLn(Greeting);
  WriteLn(Length(Greeting));
  WriteLn('[', Empty, ']');
  WriteLn(Length(Empty));
  WriteLn(OneChar);
  WriteLn(Concat);
  s := Greeting;
  s[1] := 'H';
  WriteLn(Greeting);
  WriteLn(s);
  WriteLn(Pos('ll', Greeting));
end.
