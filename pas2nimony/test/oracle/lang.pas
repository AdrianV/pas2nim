program LangOracle;
{ M5 oracle: core language semantics - sets, case, records, classes,
  exceptions, negative div/mod (FPC truncates toward zero) }
uses SysUtils;
type
  TColor = (clRed, clGreen, clBlue);
  TPoint = record
    X, Y: Integer;
  end;
  TAnimal = class
  public
    Name: string;
    constructor Create(AName: string); virtual;
    function Speak: string; virtual;
  end;
  TDog = class(TAnimal)
  public
    function Speak: string; override;
  end;
  EMyError = class(Exception)
  end;

constructor TAnimal.Create(AName: string);
begin
  Name := AName;
end;

function TAnimal.Speak: string;
begin
  result := '...';
end;

function TDog.Speak: string;
begin
  result := 'Woof';
end;

procedure Boom;
begin
  raise EMyError.Create('boom');
end;

var
  s: set of TColor;
  p: TPoint;
  a: TAnimal;
  arr: array[1..5] of Integer;
  i, q, r: Integer;
begin
  s := [clRed, clBlue];
  if clGreen in s then writeln('bad') else writeln('set-ok');
  include(s, clGreen);
  writeln('incl=', clGreen in s);
  case 3 of
    1, 2: writeln('low');
    3..5: writeln('mid');
  else
    writeln('high');
  end;
  p.X := 7; p.Y := 9;
  writeln('rec=', p.X + p.Y);
  for i := 1 to 5 do arr[i] := i * i;
  writeln('arr=', arr[3], ' ', arr[5]);
  a := TDog.Create('Rex');
  writeln('virt=', a.Name, ' says ', a.Speak);
  q := -7 div 2;
  r := -7 mod 2;
  writeln('divmod=', q, ' ', r, ' ', 7 div -2, ' ', 7 mod -2);
  try
    Boom;
  except
    on E: Exception do writeln('exc=', E.Message);
  end;
  writeln('after-exc');
end.
