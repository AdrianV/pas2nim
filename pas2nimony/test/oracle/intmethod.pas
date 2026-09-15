program intmethod;

type
  TBox = class
    F: Integer;
    procedure Put(A, B: Integer);
  end;

procedure Put(Box: TBox; A, B: Int64); overload;
begin
  Box.F := 0;
end;

procedure TBox.Put(A, B: Integer);
begin
  F := A + B;
end;

var
  Box: TBox;
begin
  Box := TBox.Create;
  Box.Put(2, 3);
  WriteLn(Box.F);
end.
