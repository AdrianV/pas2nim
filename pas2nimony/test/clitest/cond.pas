program Cond;
uses SysUtils, MyMath;
{$define LOCAL}
const A: Integer = 1;
{$ifdef LOCAL}
const B: Integer = 2;
{$endif}
{$ifndef NEVER}
const C: Integer = 3;
{$else}
const C: Integer = 4;
{$endif}
{$ifdef CLI_FLAG}
const D: Integer = 5;
{$else}
const D: Integer = 6;
{$endif}
begin
  writeln(Triple(A), B, C, D);
  {$ifdef LOCAL}writeln('local on');{$endif}
end.
