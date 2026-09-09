program TestSHL_SHR;

uses
  SysUtils, Windows;

function shr_int8(i, shift: shortint): shortint;
begin
  Result := i div (1 shl shift);
end;

function shr_int16(i, shift: smallint): smallint;
begin
  Result := i div (1 shl shift);
end;

function shr_int32(i, shift: longint): longint; inline;
begin
  Result := i div (1 shl shift);
end;

var
  number, i, A, B1, B2: Integer;
  tick: Cardinal;
begin
  try
    number := 1;
    for i := 1 to 31 do begin
      number := number shl 1;
      Writeln(Format('n = %d', [number]));
    end;
    for i := 1 to 31 do begin
      number := shr_int32(number, 1);
      Writeln(Format('n = %d', [number]));
    end;
    number := 1;
    for i := 1 to 31 do begin
      number := number * 2;
      Writeln(Format('n = %d', [number]));
    end;
    for i := 1 to 31 do begin
      number := number div 2;
      Writeln(Format('n = %d', [number]));
    end;
    A := -512;
    B1 := A shr 1;
    B2 := A div 2;
    Writeln(Format('B1 = %d', [B1]));
    Writeln(Format('B2 = %d', [B2]));
    Writeln(Format('i8 = %d', [shr_int8(100, 1)]));
    Writeln(Format('i16 = %d', [shr_int16(1000, 1)]));

    tick := GetTickCount;
    for i := 1 to 10000000 do begin
      number := shr_int32(i, 2);
    end;
    Writeln(Format('time = %d', [GetTickCount - tick]));
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
