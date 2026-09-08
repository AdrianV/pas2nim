program Ops;
uses SysUtils;
type
  TMyInt = record
    Value: Integer;
    class operator Implicit(a: Integer): TMyInt;
    class operator Implicit(a: TMyInt): Integer;
    class operator Explicit(a: TMyInt): Double;
    class operator Inc(a: TMyInt): TMyInt;
    class operator Dec(a: TMyInt): TMyInt;
    class operator Add(a, b: TMyInt): TMyInt;
  end;
class operator TMyInt.Implicit(a: Integer): TMyInt;
begin
  result.Value := a;
end;
class operator TMyInt.Implicit(a: TMyInt): Integer;
begin
  result := a.Value;
end;
class operator TMyInt.Explicit(a: TMyInt): Double;
begin
  result := a.Value * 2;
end;
class operator TMyInt.Inc(a: TMyInt): TMyInt;
begin
  result := a.Value + 1;
end;
class operator TMyInt.Dec(a: TMyInt): TMyInt;
begin
  result := a.Value - 1;
end;
class operator TMyInt.Add(a, b: TMyInt): TMyInt;
begin
  result := a.Value + b.Value;
end;
var
  m, n: TMyInt;
  i: Integer;
  d: Double;
begin
  m := 5;
  writeln('m=', m.Value);
  n := m + m;
  writeln('n=', n.Value);
  Inc(m);
  writeln('inc=', m.Value);
  Dec(m);
  writeln('dec=', m.Value);
  i := m;
  writeln('i=', i);
  d := Double(m);
  writeln('d=', Format('%g', [d]));
  m := n;
  writeln('copy=', m.Value);
end.
