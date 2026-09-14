program MultiDim;
(* `array[a..b, c..d] of T` is Pascal's multi-dimensional array; nimony has
  no such type, so each dimension becomes one nesting level:
  `array[N, array[M, T]]`. The two spellings below must agree -
  `array[0..2, 0..3] of T` and `array[0..2] of array[0..3] of T`.

  The dimension list used to lose its FIRST entry: the loop started at the
  last dimension and put it in the outer node's index slot, emitting
  `array[<an array>, int32]` (nifler: "expected: ']', but got: ..."). *)

var
  m: array[0..2, 0..3] of Integer;
  n: array[0..2] of array[0..3] of Integer;
  s: array[0..1, 0..2] of string;

begin
  m[2, 3] := 7;
  n[2, 3] := 8;
  s[1, 2] := 'xy';
  writeln('m=', m[2, 3], ' n=', n[2, 3], ' s=', s[1, 2]);
end.
