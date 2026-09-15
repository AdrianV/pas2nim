program intliteral;

var
  which: Integer;

procedure Pick(X: Integer); overload;
begin
  which := 1;
end;

procedure Pick(X: Int64); overload;
begin
  which := 2;
end;

begin
  which := 0;
  Pick(2);
  WriteLn(which);
end.
