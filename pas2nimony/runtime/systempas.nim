{.feature: "lenientnils".}
#
#           systempas - Delphi RTL compatibility shim for pas2nimony output
#
# Translated Pascal code imports this module. It provides the Delphi
# runtime helpers that have no direct nimony equivalent, plus a few
# operator overloads that Pascal source expects (`+` on strings).
#
# Nimony notes baked into this module:
#  - `proc` (not `func`): nimony `func` implies .noSideEffect and several
#    helpers here mutate locals
#  - no {.raises.} anywhere: the ErrorCode exception model makes freely
#    callable raising routines impossible, so error handling is lossy
#    (StrToInt etc. return a fallback instead of raising)

import std/[strutils, syncio]
import pasdatetime
export pasdatetime

# ---------------------------------------------------------------------------
# string operators (Pascal uses + for concatenation)

proc strDelete*(s: var string; idx, cnt: int32) =
  ## Delphi Delete(s, idx, cnt): remove cnt chars starting at 1-based idx.
  ## substr clamping keeps out-of-range calls well-defined.
  if cnt <= 0 or idx < 1: return
  s = substr(s, 0, idx - 2) & substr(s, idx - 1 + cnt)

proc strInsert*(src: string; s: var string; idx: int32) =
  ## Delphi Insert(src, s, idx): insert src before the 1-based idx
  if idx < 1: return
  s = substr(s, 0, idx - 2) & src & substr(s, idx - 1)

proc ChrToStr*(c: char): string =
  result = ""
  result.add(c)

proc `+`*(a, b: string): string = a & b
proc `+`*(a: string, b: char): string = a & ChrToStr(b)
proc `+`*(a: char, b: string): string = ChrToStr(a) & b

# ---------------------------------------------------------------------------
# conversions (Delphi SysUtils names, non-raising)

proc IntToStr*(i: int64): string = $i
proc IntToStr*(i: int32): string = $i
proc IntToStr*(i: uint32): string = $i
proc IntToStr*(i: uint64): string = $i

proc FloatToStr*(f: float): string = fpcFormatG(f, 15)
proc FloatToStr*(f: float32): string = fpcFormatG(f, 15)

proc StrToIntDef*(s: char; def: int64): int64 =
  ## 1-char Pascal literal: never parses as a number
  def

proc StrToIntDef*(s: string; def: int64): int64 =
  ## non-raising parse; returns `def` on failure
  var pos = 0
  var negative = false
  var i = 0
  while i < s.len and s[i] == ' ': inc i
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    negative = s[i] == '-'
    inc i
  while i < s.len and s[i] >= '0' and s[i] <= '9':
    pos = pos * 10 + ord(s[i]) - ord('0')
    inc i
  if i == 0 or i < s.len:
    result = def
  else:
    result = if negative: -pos else: pos

proc StrToInt*(s: string): int64 = StrToIntDef(s, 0)

proc StrToInt*(s: char): int64 = 0

# ---------------------------------------------------------------------------
# string helpers (Pascal names map onto strutils)

proc UpperCase*(s: string): string = toUpperAscii(s)
proc LowerCase*(s: string): string = toLowerAscii(s)
proc UpCase*(c: char): char = toUpperAscii(c)
proc Trim*(s: string): string = strip(s)
proc SameText*(a, b: string): bool = cmpIgnoreCase(a, b) == 0
proc CompareText*(a, b: string): int = cmpIgnoreCase(a, b)
proc StringOfChar*(c: char; count: int): string = repeat(c, count)

# ---------------------------------------------------------------------------
# I/O shims

proc WriteLn*() = echo ""

proc delphiBool*(b: bool): string =
  ## writeln(bool) renders TRUE/FALSE like Delphi/FPC
  if b: result = "TRUE" else: result = "FALSE"

proc BoolToStr*(b: bool; useBoolStrs: bool): string =
  ## Delphi/FPC: with bool strings 'True'/'False', otherwise '-1'/'0'
  if useBoolStrs:
    if b: result = "True" else: result = "False"
  else:
    if b: result = "-1" else: result = "0"

proc ReadLn*(): string =
  result = ""
  discard "readline support depends on syncio; extended in tests"
proc Read*(): char =
  result = '\x00'

# In the Delphi model `Free` destroys the object. Nimony instances are
# memory-managed, so Free is a no-op (semantics documented in
# doc/nimony-compat.md).
proc Free*(self: RootRef) =
  discard

# ---------------------------------------------------------------------------
# array of const (Delphi open array of TVarRec)

type
  TVarRecKind* = enum vrInt, vrFloat, vrChar, vrBool, vrStr, vrObj
  TVarRec* = object
    case k*: TVarRecKind
    of vrInt: i*: int64
    of vrFloat: f*: float64
    of vrChar: c*: char
    of vrBool: b*: bool
    of vrStr: s*: string
    of vrObj: o*: RootRef

  TArrayOfConst* = seq[TVarRec]

proc toVrec*(x: int64): TVarRec = TVarRec(k: vrInt, i: x)
proc toVrec*(x: int32): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: float64): TVarRec = TVarRec(k: vrFloat, f: x)
proc toVrec*(x: float32): TVarRec = TVarRec(k: vrFloat, f: float64(x))
proc toVrec*(x: char): TVarRec = TVarRec(k: vrChar, c: x)
proc toVrec*(x: bool): TVarRec = TVarRec(k: vrBool, b: x)
proc toVrec*(x: string): TVarRec = TVarRec(k: vrStr, s: x)
# ---------------------------------------------------------------------------
# Pascal for-loops
#
# Pascal iterates the *declared* loop variable; a Nim `for` always binds a
# fresh variable. These iterators drive the declared variable through its
# `var` parameter so the generated loop keeps Pascal semantics: the body
# reads the loop variable itself, `break` leaves it at the broken-out
# value, nested routines capture the live loop value, and after normal
# termination it holds b+1 (resp. b-1 for downto), Delphi's practical
# convention. The for-loop binds `_` as a throwaway.

iterator pforTo*[T: Ordinal](v: var T, a, b: T): T {.inline.} =
  ## `for i := a to b do body` -> `for _ in pforTo(i, a, b): body`
  v = a
  while v <= b:
    yield v
    inc v

iterator pforDownto*[T: Ordinal](v: var T, a, b: T): T {.inline.} =
  ## `for i := a downto b do body` -> `for _ in pforDownto(i, a, b): body`
  v = a
  while v >= b:
    yield v
    dec v

# ---------------------------------------------------------------------------
# exception instances (M4-2): ErrorCode remains the raise transport; the
# exception OBJECT rides a module-level current-exception slot that
# `raise SomeE.Create(msg)` populates and `on E: SomeE do` handlers read

type
  # spelled PasException: Nim's system module reserves `Exception`
  PasException* {.inheritable.} = ref object of RootRef
    Message*: string

var pasCurrentExc*: PasException = PasException(Message: "")

# Delphi `x as T`: nil stays nil, a failed checked cast yields nil.
# (The raising variant would mark every transitive caller {.raises.};
# nimony only allows calls to .raises routines inside try - a
# documented divergence, see doc/nimony-compat.md.)
proc pasAs*[T](x: RootRef): T =
  if x of T:
    cast[T](x)
  else:
    nil

# named pasExcCreate (not `create`): a user subclass's own `create`
# must not shadow the prelude constructor in nimony's name resolution
proc pasExcCreate*(self: PasException; msg: string): PasException =
  ## Delphi Exception.Create: stash the message, return the instance
  self.Message = msg
  result = self

# ---------------------------------------------------------------------------
# SysUtils core (M3-2): conversions, string helpers, path functions,
# Format. All our own code; float rendering follows C printf through
# nimony's formatBiggestFloat (rounding may differ from Delphi in the
# last digit).

import std/[os, dirs]

proc IntToHex*(value: int64; digits: int32): string =
  ## Delphi/FPC: pad to `digits`, but never truncate - a value that
  ## needs more digits prints in full (IntToHex(4096, 2) = "1000")
  var s = toHex(value, 16)
  var i = 0
  while i < s.len - 1 and s[i] == '0':
    inc i
  s = substr(s, i, s.len - 1)
  while s.len < digits:
    s = "0" & s
  result = s
proc IntToHex*(value: int32; digits: int32): string =
  IntToHex(int64(value), digits)

proc StrToFloatDef*(s: string; def: float64): float64 =
  ## non-raising parse of an optional-sign integer/fraction/exponent
  var i = 0
  var negative = false
  var val = 0.0
  while i < s.len and s[i] == ' ': inc i
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    negative = s[i] == '-'
    inc i
  var seenDigits = false
  while i < s.len and s[i] >= '0' and s[i] <= '9':
    val = val * 10.0 + float64(ord(s[i]) - ord('0'))
    seenDigits = true
    inc i
  if i < s.len and s[i] == '.':
    inc i
    var frac = 0.1
    while i < s.len and s[i] >= '0' and s[i] <= '9':
      val = val + float64(ord(s[i]) - ord('0')) * frac
      frac = frac / 10.0
      seenDigits = true
      inc i
  if i < s.len and (s[i] == 'e' or s[i] == 'E'):
    inc i
    var eneg = false
    if i < s.len and (s[i] == '-' or s[i] == '+'):
      eneg = s[i] == '-'
      inc i
    var e = 0
    while i < s.len and s[i] >= '0' and s[i] <= '9':
      e = e * 10 + ord(s[i]) - ord('0')
      inc i
    var mult = 1.0
    var k = 0
    while k < e:
      mult = mult * 10.0
      inc k
    if eneg: val = val / mult else: val = val * mult
  if not seenDigits:
    result = def
  else:
    if negative: val = -val
    result = val

proc StrToFloat*(s: string): float64 = StrToFloatDef(s, 0.0)

proc StrToFloatDef*(s: char; def: float64): float64 =
  ## a 1-char Pascal literal can never parse as a float (char overload
  ## for nimony's missing char->string conversion)
  def

proc TrimLeft*(s: string): string =
  result = ""
  var i = 0
  while i < s.len and (s[i] == ' ' or s[i] == '\t' or s[i] == '\r' or s[i] == '\n'):
    inc i
  if i < s.len: result = substr(s, i, s.len - 1)

proc TrimRight*(s: string): string =
  result = ""
  var e = s.len
  while e > 0 and (s[e - 1] == ' ' or s[e - 1] == '\t' or s[e - 1] == '\r' or
      s[e - 1] == '\n'):
    dec e
  if e > 0: result = substr(s, 0, e - 1)

proc CompareStr*(a, b: string): int =
  if a < b: -1 elif a > b: 1 else: 0

proc AnsiCompareStr*(a, b: string): int = CompareStr(a, b)
proc AnsiCompareText*(a, b: string): int = CompareText(a, b)
proc AnsiUpperCase*(s: string): string = UpperCase(s)
proc AnsiLowerCase*(s: string): string = LowerCase(s)

proc FileExists*(path: string): bool = fileExists(path)

proc DeleteFile*(path: string): bool =
  try:
    removeFile(path(path))
    result = true
  except Exception:
    result = false

proc DirectoryExists*(path: char): bool =
  ## 1-char Pascal literals are chars ('.' = the current dir)
  DirectoryExists($path)

proc DirectoryExists*(path: string): bool =
  try:
    result = dirExists(path)
  except Exception:
    result = false

proc CreateDir*(path: string): bool =
  try:
    createDir(path(path))
    result = true
  except Exception:
    result = false

proc ForceDirectories*(path: string): bool =
  ## CreateDir already creates the whole chain in nimony's dirs module
  if path.len == 0:
    result = false
  else:
    result = CreateDir(path)

# --- path functions (pure string re-implementation, both / and \) ---

proc LastPathDelimiter(path: string): int =
  result = -1
  var i = path.len - 1
  while i >= 0:
    if path[i] == '/' or path[i] == '\\':
      return i
    dec i

proc ExtractFilePath*(path: string): string =
  ## everything up to and including the last path delimiter
  result = ""
  let p = LastPathDelimiter(path)
  if p >= 0: result = substr(path, 0, p)

proc ExtractFileDir*(path: string): string =
  ## like ExtractFilePath without the trailing delimiter
  result = ""
  let p = LastPathDelimiter(path)
  if p >= 0: result = substr(path, 0, p - 1)

proc ExtractFileName*(path: string): string =
  result = path
  let p = LastPathDelimiter(path)
  if p >= 0 and p + 1 < path.len: result = substr(path, p + 1, path.len - 1)

proc FileExtensionPos(path: string): int =
  ## the last '.' before the last path delimiter (-1 when none)
  result = -1
  var i = path.len - 1
  while i >= 0:
    if path[i] == '/' or path[i] == '\\': break
    if path[i] == '.':
      return i
    dec i

proc ExtractFileExt*(path: string): string =
  ## the extension including the dot ("" when none)
  result = ""
  let p = FileExtensionPos(path)
  if p >= 0: result = substr(path, p, path.len - 1)

proc ChangeFileExt*(path, ext: string): string =
  result = path
  let p = FileExtensionPos(path)
  if p >= 0: result = substr(path, 0, p - 1)
  result = result & ext

proc IncludeTrailingPathDelimiter*(path: string): string =
  result = path
  if path.len > 0 and path[path.len - 1] != '/' and path[path.len - 1] != '\\':
    result = path & "/"

proc ExcludeTrailingPathDelimiter*(path: string): string =
  result = path
  if path.len > 0 and (path[path.len - 1] == '/' or path[path.len - 1] == '\\'):
    result = substr(path, 0, path.len - 2)

# --- Format: Delphi printf-style with array-of-const ---

proc vrecToBoolStr(v: TVarRec): string =
  result = ""
  if v.b: result = "True" else: result = "False"

proc vrecToStr(v: TVarRec): string =
  case v.k
  of vrInt: result = $v.i
  of vrFloat: result = $v.f
  of vrChar: result = ChrToStr(v.c)
  of vrBool: result = vrecToBoolStr(v)
  of vrStr: result = v.s
  of vrObj: result = ""

proc fpcSciMantissa(f: float64; afterPoint: int): string =
  ## the mantissa digits FPC prints: C printf rounding at afterPoint
  ## digits after the point (verified identical to FPC 3.2.2)
  formatBiggestFloat(f, ffScientific, int64(afterPoint), '.')

proc fpcFormatE*(f: float64; prec: int): string =
  ## FPC %e: mantissa + E with a signed 3-digit exponent (1.23E+003).
  ## prec < 0 = the default (16 after the point = 17 significant);
  ## an explicit N counts significant digits like Delphi/FPC.
  var ap = prec
  if ap < 0: ap = 16
  else:
    ap = ap - 1
    if ap < 1: ap = 1
  let s = fpcSciMantissa(f, ap)
  let ep = find(s, "e", 0)
  let mant = substr(s, 0, ep - 1)
  var expPart = substr(s, ep + 1, s.len - 1)     # "+00" / "-01"
  var digits = substr(expPart, 1, expPart.len - 1)
  while digits.len < 3: digits = "0" & digits
  result = mant & "E" & substr(expPart, 0, 0) & digits

proc fpcFormatG*(f: float64; prec: int): string =
  ## FPC %g: the exact value; positional unless exp >= precision
  ## (default 17), trailing zeros stripped, E-form without plus sign
  ## or padding (1E20, 1.23E3).
  var sig = prec
  if sig <= 0: sig = 17
  let s = fpcSciMantissa(f, sig - 1)
  let ep = find(s, "e", 0)
  var mant = substr(s, 0, ep - 1)
  # parse the exponent manually
  var expPart = substr(s, ep + 1, s.len - 1)
  var eneg = false
  var q = 0
  if expPart[q] == '-':
    eneg = true
    inc q
  elif expPart[q] == '+':
    inc q
  var exp = 0
  while q < expPart.len and expPart[q] >= '0' and expPart[q] <= '9':
    exp = exp * 10 + ord(expPart[q]) - ord('0')
    inc q
  if eneg: exp = -exp
  if exp >= sig:
    # scientific: strip trailing zeros (and a trailing dot)
    while mant.len > 0 and mant[mant.len - 1] == '0':
      mant = substr(mant, 0, mant.len - 2)
    if mant.len > 0 and mant[mant.len - 1] == '.':
      mant = substr(mant, 0, mant.len - 2)
    result = mant & "E" & $exp
  else:
    # positional: shift the decimal point from after digit 1
    var neg = false
    var digits = ""
    var k = 0
    while k < mant.len:
      if mant[k] == '-':
        neg = true
      elif mant[k] != '.':
        digits.add(mant[k])
      inc k
    let pt = 1 + exp
    var outp = ""
    if pt <= 0:
      outp = "0."
      var z = 0
      while z < -pt:
        outp.add('0')
        inc z
      outp.add(digits)
    else:
      if pt >= digits.len:
        outp = digits
        var z = digits.len
        while z < pt:
          outp.add('0')
          inc z
      else:
        outp = substr(digits, 0, pt - 1) & "." & substr(digits, pt, digits.len - 1)
    while outp.len > 1 and outp[outp.len - 1] == '0':
      outp = substr(outp, 0, outp.len - 2)
    if outp.len > 0 and outp[outp.len - 1] == '.':
      outp = substr(outp, 0, outp.len - 2)
    if neg and outp != "0": outp = "-" & outp
    result = outp

proc padLeftStr(s: string; width: int; leftAlign: bool): string =
  result = s
  if width > s.len:
    var pad = ""
    var k = s.len
    while k < width:
      pad.add(' ')
      inc k
    if leftAlign: result = s & pad else: result = pad & s

proc Format*(fmt: string; args: openArray[TVarRec]): string =
  ## Delphi Format: %[index:][-][width][.precision]type over
  ## array-of-const; the parser lowers `[a, b, c]` to toVrec calls.
  ## d/u decimal, s string, x/X hex, e/E/f/g/G/n/m floats (n/m = fixed
  ## 2 in v1, no thousands separator), p unsupported, %% literal.
  result = ""
  var argIndex = 0
  var p = 0
  while p < fmt.len:
    if fmt[p] != '%':
      result.add(fmt[p])
      inc p
      continue
    inc p
    if p >= fmt.len: break
    if fmt[p] == '%':
      result.add('%')
      inc p
      continue
    # optional index
    var idx = -1
    var q = p
    var num = 0
    var hasDigits = false
    while q < fmt.len and fmt[q] >= '0' and fmt[q] <= '9':
      num = num * 10 + ord(fmt[q]) - ord('0')
      hasDigits = true
      inc q
    if hasDigits and q < fmt.len and fmt[q] == ':':
      idx = num
      p = q + 1
    # '-' flag
    var leftAlign = false
    if p < fmt.len and fmt[p] == '-':
      leftAlign = true
      inc p
    # width
    var width = 0
    while p < fmt.len and fmt[p] >= '0' and fmt[p] <= '9':
      width = width * 10 + ord(fmt[p]) - ord('0')
      inc p
    # precision
    var prec = -1
    if p < fmt.len and fmt[p] == '.':
      inc p
      prec = 0
      while p < fmt.len and fmt[p] >= '0' and fmt[p] <= '9':
        prec = prec * 10 + ord(fmt[p]) - ord('0')
        inc p
    if p >= fmt.len: break
    let typ = fmt[p]
    inc p
    # fetch the argument
    var ai = argIndex
    if idx >= 0: ai = idx else: inc argIndex
    var piece = ""
    if ai >= 0 and ai < args.len:
      let v = args[ai]
      if typ == 'd' or typ == 'u':
        if v.k == vrInt: piece = $v.i
        elif v.k == vrBool: piece = vrecToBoolStr(v)
        elif v.k == vrFloat: piece = $v.f
        else: piece = "0"
      elif typ == 'x' or typ == 'X':
        if v.k == vrInt:
          let full = toHex(v.i, 16)
          var i = 0
          while i < full.len - 1 and full[i] == '0':
            inc i
          piece = substr(full, i, full.len - 1)
          if typ == 'X': piece = toUpperAscii(piece)
      elif typ == 'e' or typ == 'E' or typ == 'f' or typ == 'g' or typ == 'G' or typ == 'n' or typ == 'm':
        var f = 0.0
        if v.k == vrFloat: f = v.f
        elif v.k == vrInt: f = float64(v.i)
        if typ == 'f' or typ == 'n' or typ == 'm':
          # FPC's variable path (array-of-const holds Double) rounds like
          # C printf; FPC's *literal* path promotes to Extended instead -
          # a v1 divergence documented in doc/nimony-compat.md
          var prec2 = prec
          if prec2 < 0: prec2 = 2
          piece = formatBiggestFloat(f, ffDecimal, prec2, '.')
          if prec2 == 0 and piece.len > 0 and piece[piece.len - 1] == '.':
            piece = substr(piece, 0, piece.len - 2)
          if typ == 'n':
            # thousands separators in the integer part
            let dot = find(piece, ".", 0)
            if dot > 0:
              var grouped = ""
              var k = 0
              while k < dot:
                if k > 0 and (dot - k) mod 3 == 0: grouped.add(',')
                grouped.add(piece[k])
                inc k
              piece = grouped & substr(piece, dot, piece.len - 1)
        elif typ == 'g' or typ == 'G':
          piece = fpcFormatG(f, prec)
        else:
          piece = fpcFormatE(f, prec)
      elif typ == 's':
        if v.k == vrStr:
          piece = v.s
          if prec >= 0 and prec < piece.len:
            piece = substr(piece, 0, prec - 1)
        else:
          piece = vrecToStr(v)
      else:
        piece = ""
    result.add(padLeftStr(piece, width, leftAlign))

proc Format*(fmt: string): string =
  Format(fmt, [])
