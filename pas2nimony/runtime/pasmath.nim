{.feature: "lenientnils".}
#
# pasmath - Delphi Math unit shim for pas2nimony
#
# A thin layer over std/math; Min/Max/Sign are nimony compiler
# builtins. RoundTo implements Delphi's banker's rounding;
# SimpleRoundTo rounds half away from zero like C's round.
#
# Nimony notes baked in (same as systempas): `proc` (not `func`),
# no {.raises.}.

import std/math

proc Floor*(x: float64): int64 = int64(floor(x))
proc Floor*(x: float32): int64 = int64(floor(x))
proc Ceil*(x: float64): int64 = int64(ceil(x))
proc Ceil*(x: float32): int64 = int64(ceil(x))

proc Power*(base, exponent: float64): float64 = pow(base, exponent)
proc IntPower*(base: float64; exponent: int32): float64 =
  pow(base, float64(int(exponent)))

proc Hypot*(x, y: float64): float64 = sqrt(x * x + y * y)

proc DegToRad*(deg: float64): float64 = deg * (PI / 180.0)
proc RadToDeg*(rad: float64): float64 = rad * (180.0 / PI)

proc CompareValue*(a, b: int64): int = sgn(a - b)
proc CompareValue*(a, b: float64): int = sgn(a - b)

proc SameValue*(a, b: float64; epsilon: float64 = 0.0): bool =
  ## Delphi SameValue: equality with a tolerance; epsilon = 0 scales
  ## the tolerance from the magnitudes (1e-12)
  if a == b:
    result = true
  else:
    var eps = epsilon
    if eps == 0.0:
      var aa = a
      if aa < 0.0: aa = -aa
      var bb = b
      if bb < 0.0: bb = -bb
      if bb > aa: aa = bb
      eps = aa * 1e-12
    var diff = a - b
    if diff < 0.0: diff = -diff
    result = diff <= eps

proc Sign*(x: int64): int = sgn(x)
proc Sign*(x: float64): int = sgn(x)

proc IsZero*(x: float64): bool = x == 0.0

proc SimpleRoundTo*(x: float64; digits: int64 = -2): float64 =
  ## round half away from zero to `digits` decimal places
  ## (digits < 0 counts decimals, Delphi convention; int64 param:
  ## nimony does not narrow negative int literals)
  var m = pow(10.0, float64(-int(digits)))
  result = round(x * m) / m

proc RoundTo*(x: float64; digits: int64 = -2): float64 =
  ## Delphi RoundTo: banker's rounding (halves go to the even value)
  var m = pow(10.0, float64(-int(digits)))
  var y = x * m
  var r = floor(y)
  var frac = y - r
  if frac == 0.5:
    if int64(r) mod 2 == 0:
      result = r / m
    else:
      result = (r + 1.0) / m
  else:
    result = round(y) / m