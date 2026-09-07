program WithTest;
{ M4 `with`: hidden temporaries (`var pasW1: TShape = S`) plus
  parse-time member qualification against the with-classes;
  later expressions are evaluated in the scope of the earlier ones }

type
  TPoint = class
  public
    FX, FY: Integer;
    procedure Move(ADX, ADY: Integer);
  end;

  TShape = class
  public
    FOrigin: TPoint;
    FTag: String;
    constructor Create;
  end;

procedure TPoint.Move(ADX, ADY: Integer);
begin
  FX := FX + ADX;
  FY := FY + ADY;
end;

constructor TShape.Create;
begin
  FOrigin := TPoint.Create;
  FTag := 'shape';
end;

var
  S: TShape;
  P: TPoint;
begin
  S := TShape.Create;
  with S do
  begin
    FTag := 'moved';
    FOrigin.Move(2, 3);
  end;
  with S.FOrigin do          { member chain }
  begin
    FX := 7;
  end;
  P := TPoint.Create;
  with P do FX := 42;        { single-statement body }
  with S, FOrigin do         { E2 evaluated in E1's scope }
  begin
    FY := 9;
  end;
  writeln('tag=', S.FTag, ' fx=', S.FOrigin.FX, ' fy=', S.FOrigin.FY, ' p=', P.FX);
end.