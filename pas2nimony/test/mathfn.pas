program MathFn;
(* The Delphi `Math` unit maps to the `pasmath` shim, which is a thin
  layer over nimony's `std/math`. The shim imports `std/math` for its
  own wrappers but did not re-export it, so an importer saw only
  Floor/Ceil/RoundTo and `Sin`, `Cos`, `ArcSin`, `ArcCos`, `ArcTan`,
  `PI` were "undeclared identifier". `ArcSin`/`ArcCos` additionally
  need the Pascal->nimony spelling in the RTL name map
  (`arcsin`/`arccos`), like `ArcTan` already had.

  Only the wrappers that std/math cannot express live in the shim. *)

uses Math, SysUtils;

var
  x, y: Extended;

begin
  x := Sin(0.0) + Cos(0.0);
  y := ArcSin(0.0) + ArcCos(1.0) + ArcTan(0.0);
  writeln('x=', FloatToStr(x), ' y=', FloatToStr(y), ' pi=', FloatToStr(PI));
end.
