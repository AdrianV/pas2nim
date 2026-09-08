program FuncsOracle;
{ M5-2 oracle: var params, defaults, overloading, recursion, Exit }
uses SysUtils;
function Add(a, b: Integer): Integer; overload;
begin
  result := a + b;
end;

function Add(a, b: Double): Double; overload;
begin
  result := a + b;
end;

procedure SwapVal(var a, b: Integer);
var
  t: Integer;
begin
  t := a;
  a := b;
  b := t;
end;

procedure Fill(var out1: Integer);
begin
  out1 := 99;
end;

function Greet(name: string; punct: string = '!'): string;
begin
  result := 'Hi ' + name + punct;
end;

function Fact(n: Integer): Integer;
begin
  if n <= 1 then
    Exit(1);
  result := n * Fact(n - 1);
end;

function Fib(n: Integer): Integer;
begin
  if n < 2 then
    result := n
  else
    result := Fib(n - 1) + Fib(n - 2);
end;

function Outer(x: Integer): Integer;
  function Twice(v: Integer): Integer;
  begin
    result := v * 2;
  end;
begin
  result := x + Twice(x);
end;

var
  a, b, x, y: Integer;
  d: Double;
begin
  x := 2;
  y := 3;
  writeln('add=', Add(x, y), ' ', Format('%.1f', [Add(2.5, 1.25)]));
  a := 1;
  b := 2;
  SwapVal(a, b);
  writeln('swap=', a, ' ', b);
  Fill(a);
  writeln('fill=', a);
  writeln('greet=', Greet('bob'), ' ', Greet('ann', '?'));
  writeln('fact=', Fact(6), ' fib=', Fib(10));
  writeln('outer=', Outer(7));
end.
