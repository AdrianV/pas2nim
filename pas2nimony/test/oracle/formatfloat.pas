program FormatFloatOracle;
{ M5 oracle: FPC-shaped float rendering through Format }
uses SysUtils;
var
  vals: array[0..8] of Double;
  i: Integer;
begin
  vals[0] := 2.675; vals[1] := 1234.5; vals[2] := 0.00025;
  vals[3] := 1234567.891; vals[4] := 1.0; vals[5] := -0.5;
  vals[6] := 1.0e20; vals[7] := 0.1; vals[8] := 100000.0;
  for i := 0 to 8 do begin
    writeln(Format('%g', [vals[i]]), ' | ', Format('%e', [vals[i]]),
      ' | ', Format('%f', [vals[i]]), ' | ', Format('%.3g', [vals[i]]));
  end;
  writeln(Format('%.2f', [vals[0]]), ' ', Format('%5.1f', [3.14]));
  writeln(Format('%g', [0.0]), ' ', Format('%e', [0.0]));
  writeln(Format('%.1f', [vals[5]]), ' ', Format('%.0f', [vals[4]]));
end.
