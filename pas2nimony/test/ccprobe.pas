program CcProbe;
function Add1(x: Integer): Integer; inline;
begin
  result := x + 1;
end;
function Add2(x: Integer): Integer; cdecl;
begin
  result := x + 2;
end;
function Add3(x: Integer): Integer; stdcall;
begin
  result := x + 3;
end;
function Add4(x: Integer): Integer; register;
begin
  result := x + 4;
end;
begin
  writeln(Add1(1), Add2(2), Add3(3), Add4(4));
end.
