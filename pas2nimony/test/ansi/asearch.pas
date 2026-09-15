program asearch;

{ Comparison and search: both oracles agree on these. }

uses SysUtils;

var
  a, b: AnsiString;

begin
  a := 'apple';
  b := 'banana';
  if a = b then WriteLn('eq') else WriteLn('ne');
  if a < b then WriteLn('lt') else WriteLn('ge');
  if a > b then WriteLn('gt') else WriteLn('le');
  WriteLn(CompareStr(a, b));
  WriteLn(Pos('pp', a));
  WriteLn(AnsiPos('na', b));
  WriteLn(Copy(a, 1, 3));
end.
