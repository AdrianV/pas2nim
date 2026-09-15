program acore;

{ Public AnsiString conformance: construction, indexing, assignment and
  length. Observable behaviour is identical in FPC -Mdelphi and Delphi 2007. }

var
  a, b, c: AnsiString;

begin
  a := 'hello';
  WriteLn(Length(a));
  WriteLn(a[1], a[5]);
  b := a;
  b[1] := 'H';
  WriteLn(a);
  WriteLn(b);
  c := a + ' ' + b;
  WriteLn(c);
  WriteLn(Length(c));
  SetLength(c, 5);
  WriteLn(c);
  WriteLn(Length(c));
end.
