program RecordsOracle;
{ M5-2 oracle: records - nesting, arrays, with, variants, assignment }
uses SysUtils;
type
  TPoint = record
    X, Y: Integer;
  end;
  TLine = record
    A, B: TPoint;
    Tag: string;
  end;
  TShape = record
    Kind: Integer;
    case Integer of
      0: (Radius: Double);
      1: (W, H: Integer);
  end;
  TArr = array[1..3] of TPoint;
var
  p, q: TPoint;
  ln: TLine;
  sh: TShape;
  arr: TArr;
  i, area: Integer;
begin
  p.X := 3;
  p.Y := 4;
  writeln('p=', p.X, ' ', p.Y);
  p := p;
  q := p;
  q.X := 10;
  writeln('copy=', p.X, ' ', q.X, ' ', q.Y);
  with q do
  begin
    X := 20;
    Y := 30;
  end;
  writeln('with=', q.X, ' ', q.Y);
  ln.A.X := 1;
  ln.A.Y := 2;
  ln.B.X := 3;
  ln.B.Y := 4;
  ln.Tag := 'seg';
  writeln('line=', ln.Tag, ' ', ln.A.X + ln.B.Y, ' ', ln.B.Y - ln.B.X);
  sh.Kind := 1;
  sh.W := 6;
  sh.H := 7;
  area := sh.W * sh.H;
  writeln('rect=', area, ' ', sh.Kind);
  for i := 1 to 3 do
  begin
    arr[i].X := i;
    arr[i].Y := i * i;
  end;
  writeln('arr=', arr[1].Y, ' ', arr[2].Y, ' ', arr[3].Y);
end.
