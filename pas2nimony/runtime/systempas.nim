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

import std/[strutils, syncio, times]
import pasdatetime

proc GetTickCount*(): uint32 =
  ## Delphi Windows.GetTickCount - milliseconds since boot. Shimmed
  ## over the epoch clock (documented divergence: no boot-time origin,
  ## no 49.7-day wrap); translated programs use deltas.
  let t = getTime()
  result = uint32(t.seconds * 1000'i64 + int64(t.nanosecond) div 1_000_000)
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

proc delphiShr*(a: int64; b: int32): int64 =
  ## FPC's shr is LOGICAL (fills zeros); nimony's is arithmetic
  int64(uint64(a) shr uint32(b))

proc Odd*(i: int64): bool =
  (i and 1) != 0

proc Even*(i: int64): bool =
  (i and 1) == 0

proc padWidth(s: string; w: int32): string =
  ## right-align in `w` columns (Str/writeln width)
  result = s
  while result.len < w:
    result = " " & result

proc pasW*(v: int64; w: int32): string = padWidth($v, w)

proc fpcWidthSci*(f: float64; w: int32): string =
  ## FPC's :width-only float form (probed): sign slot (space for
  ## non-negatives), mantissa with max(width-8, 1) digits after the
  ## point, E+-ddd; no truncation below the width
  var ap = int(w) - 8
  if ap < 1: ap = 1
  let body = fpcFormatE(f, ap + 1)
  if f >= 0:
    result = padWidth(" " & body, w)
  else:
    result = padWidth(body, w)

proc pasW*(v: float64; w: int32): string = fpcWidthSci(v, w)

proc pasW*(v: string; w: int32): string = padWidth(v, w)

proc pasW*(v: char; w: int32): string = padWidth(ChrToStr(v), w)

proc pasW*(v: float64; w: int32; p: int32): string =
  padWidth(fpcFormatF(v, int(p)), w)

proc Str*(v: int64; s: var string) = s = $v

proc Str*(v: int32; s: var string) = s = $v

proc Str*(v: int64; w: int32; s: var string) = s = padWidth($v, w)

proc Str*(v: int32; w: int32; s: var string) = s = padWidth($v, w)

proc Str*(v: float64; w: int32; s: var string) = s = fpcWidthSci(v, w)

proc Str*(v: float64; w: int32; p: int32; s: var string) =
  s = padWidth(fpcFormatF(v, int(p)), w)

proc Str*(v: string; target: var string) = target = v

proc Val*(s: string; v: var int32; code: var int32) =
  ## Integer var (Pascal Integer = int32); nimony var params match
  ## exactly, no implicit widening
  var big: int64 = 0
  Val(s, big, code)
  v = int32(big)

proc Val*(s: string; v: var int64; code: var int32) =
  ## FPC semantics: skip leading blanks, optional sign, decimal or
  ## $hex digits; code = 1-based position of the first offending
  ## character (0 = ok); the out value is 0 on error
  var i = 0
  while i < s.len and s[i] == ' ':
    inc i
  var neg = false
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    neg = s[i] == '-'
    inc i
  var acc: int64 = 0
  var any = false
  var bad = s.len
  if i < s.len and s[i] == '$':
    inc i
    while i < s.len and ((s[i] >= '0' and s[i] <= '9') or
        (s[i] >= 'a' and s[i] <= 'f') or (s[i] >= 'A' and s[i] <= 'F')):
      let c = s[i]
      if c >= '0' and c <= '9':
        acc = acc * 16 + ord(c) - ord('0')
      elif c >= 'a' and c <= 'f':
        acc = acc * 16 + ord(c) - ord('a') + 10
      else:
        acc = acc * 16 + ord(c) - ord('A') + 10
      inc i
      any = true
  else:
    while i < s.len and s[i] >= '0' and s[i] <= '9':
      acc = acc * 10 + ord(s[i]) - ord('0')
      inc i
      any = true
  if any and i == s.len:
    if neg: acc = -acc
    v = acc
    code = 0
  else:
    bad = i
    v = 0
    code = int32(bad + 1)

proc Val*(s: string; v: var float64; code: var int32) =
  var i = 0
  while i < s.len and s[i] == ' ':
    inc i
  var j = i
  if j < s.len and (s[j] == '-' or s[j] == '+'):
    inc j
  var sawDigit = false
  while j < s.len and s[j] >= '0' and s[j] <= '9':
    inc j
    sawDigit = true
  if j < s.len and s[j] == '.':
    inc j
    while j < s.len and s[j] >= '0' and s[j] <= '9':
      inc j
      sawDigit = true
  if sawDigit and j < s.len and (s[j] == 'e' or s[j] == 'E'):
    var k = j + 1
    if k < s.len and (s[k] == '-' or s[k] == '+'):
      inc k
    var any = false
    while k < s.len and s[k] >= '0' and s[k] <= '9':
      inc k
      any = true
    if any: j = k
  if sawDigit and j == s.len:
    v = StrToFloatDef(substr(s, i, j - 1), 0.0)
    code = 0
  else:
    v = 0
    if sawDigit: code = int32(j + 1) else: code = int32(i + 1)

proc ffSection(pattern: string; idx: int): string =
  ## the idx-th ';'-separated section of a FormatFloat pattern
  var sec = 0
  var cur = ""
  var i = 0
  while i <= pattern.len:
    if i == pattern.len or pattern[i] == ';':
      if sec == idx: return cur
      inc sec
      cur = ""
    else:
      cur.add(pattern[i])
    inc i
  return cur

proc ffCount(s: string; chars: set[char]): int =
  result = 0
  var i = 0
  while i < s.len:
    if s[i] in chars: inc result
    inc i

proc FormatFloat*(pattern: char; value: float64): string =
  ## 1-char Pascal literal pattern ('0')
  FormatFloat(ChrToStr(pattern), value)

proc FormatFloat*(pattern: string; value: float64): string =
  ## Delphi/FPC FormatFloat: '0' mandatory digit, '#' optional,
  ## ',' thousands, 'E+xx' scientific, ';' positive/negative/zero
  ## sections. Rounding: shortest-repr half-away (2.5 '0' -> "3").
  var pat = pattern
  var v = value
  # section selection
  var secs = ffCount(pattern, {';'})
  if secs == 0: secs = 1
  if value < 0:
    if secs >= 2:
      pat = ffSection(pattern, 1)
      v = -value
    # else: keep the sign, fpcFormatF renders it
  elif value == 0 and secs >= 3:
    pat = ffSection(pattern, 2)
  if pat.len == 0:
    pat = "0"
  # scientific?
  var epos = find(pat, "E+", 0)
  var eNeg = false
  if epos < 0: epos = find(pat, "E-", 0)
  if epos >= 0:
    # mantissa: '0's after the point in the pattern, then E+dd
    var after = substr(pat, 0, epos - 1)
    var expDigits = ffCount(substr(pat, epos + 2, pat.len - 1), {'0', '#'})
    if expDigits < 1: expDigits = 1
    let dot = find(after, ".", 0)
    var mantDigits = 0
    if dot > 0:
      mantDigits = ffCount(substr(after, dot + 1, after.len - 1), {'0', '#'})
    let sci = fpcSciMantissa(abs(v), mantDigits)   # d.ddd e+xx
    let ep2 = find(sci, "e", 0)
    var m = substr(sci, 0, ep2 - 1)
    var ex = substr(sci, ep2 + 1, sci.len - 1)     # +04
    # pad the exponent to expDigits
    var digits = substr(ex, 1, ex.len - 1)
    while digits.len < expDigits: digits = "0" & digits
    var neg = v < 0
    if neg: m = "-" & m
    result = m & "E" & substr(ex, 0, 0) & digits
    return
  # fixed: decimals wanted = trailing '0's + '#'s after the point
  let dot = find(pat, ".", 0)
  var dec0 = 0
  var decHash = 0
  if dot > 0:
    let frac = substr(pat, dot + 1, pat.len - 1)
    dec0 = ffCount(frac, {'0'})
    decHash = ffCount(frac, {'#'})
  let prec = dec0 + decHash
  var outp = fpcFormatF(v, prec)
  # strip optional decimals: trailing zeros beyond the mandatory digits
  if decHash > 0:
    let dp = find(outp, ".", 0)
    if dp > 0:
      var frac = substr(outp, dp + 1, outp.len - 1)
      var cut = decHash
      while cut > 0 and frac.len > dec0 and frac[frac.len - 1] == '0':
        frac = substr(frac, 0, frac.len - 2)
        dec cut
      outp = substr(outp, 0, dp)   # keeps the dot
      if frac.len > 0: outp = outp & frac
      if outp[outp.len - 1] == '.' and dec0 == 0:
        outp = substr(outp, 0, outp.len - 2)
  # no leading zero when the integer pattern has only '#'
  var intPat = pat
  if dot > 0: intPat = substr(pat, 0, dot - 1)
  if ffCount(intPat, {'0'}) == 0 and ffCount(intPat, {'#'}) > 0:
    if outp.len > 0 and outp[0] == '0' and outp.len > 1 and outp[1] == '.':
      outp = substr(outp, 1, outp.len - 1)
  # thousands separators
  if find(pat, ",", 0) >= 0:
    let dp = find(outp, ".", 0)
    var ip: string
    var rest: string
    if dp > 0:
      ip = substr(outp, 0, dp - 1)
      rest = substr(outp, dp, outp.len - 1)
    else:
      ip = outp
      rest = ""
    var neg = false
    if ip.len > 0 and ip[0] == '-':
      neg = true
      ip = substr(ip, 1, ip.len - 1)
    var grouped = ""
    var cnt = 0
    var j = ip.len - 1
    while j >= 0:
      if cnt == 3:
        grouped = "," & grouped
        cnt = 0
      grouped = substr(ip, j, j) & grouped
      inc cnt
      dec j
    if neg: grouped = "-" & grouped
    outp = grouped & rest
  result = outp

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

  # Delphi System-unit pointer aliases (Types.pas re-exports these)
  PLongint* = ptr int32
  PInteger* = ptr int32
  PSmallInt* = ptr int16
  PDouble* = ptr float64
  PByte* = ptr uint8

  # SysUtils multi-byte classification (MaskUtils uses ByteType); the
  # v1 model treats every byte as a single byte (ASCII divergence)
  TMbcsByteType* = enum mbSingleByte, mbLeadByte, mbTrailByte

proc toVrec*(x: int64): TVarRec = TVarRec(k: vrInt, i: x)
proc toVrec*(x: int32): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: int8): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: int16): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: uint8): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: uint16): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: uint32): TVarRec = TVarRec(k: vrInt, i: int64(x))
proc toVrec*(x: float64): TVarRec = TVarRec(k: vrFloat, f: x)
proc toVrec*(x: float32): TVarRec = TVarRec(k: vrFloat, f: float64(x))
proc toVrec*(x: char): TVarRec = TVarRec(k: vrChar, c: x)
proc toVrec*(x: bool): TVarRec = TVarRec(k: vrBool, b: x)
proc toVrec*(x: string): TVarRec = TVarRec(k: vrStr, s: x)

proc ByteType*(s: string; index: int32): TMbcsByteType =
  ## SysUtils multi-byte byte classification; v1 ASCII model
  result = mbSingleByte

proc Len32*(s: string): int32 =
  ## Pascal Length(string) = Integer (int32); nimony's len is int64
  result = int32(len(s))

proc Len32*[T](x: openArray[T]): int32 =
  ## Pascal Length(array) = Integer (int32)
  result = int32(len(x))

# SysUtils locale separators (v1 constants - locale read/write is a
# documented divergence)
var TimeSeparator*: char = ':'
var DateSeparator*: char = '/'
var DecimalSeparator*: char = '.'
var ThousandSeparator*: char = ','
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

# Delphi TObject.Destroy: the root destructor. Classes whose parent
# chain declares no destructor (`inherited Destroy` from
# TSynchroObject) dispatch through this root method.
method Destroy*(self: RootRef) =
  discard

# TObject.ClassType: the v1 shell hands back the instance (the corpus
# only compares class references)
proc ClassType*(self: RootRef): RootRef =
  result = self

# TObject.InheritsFrom: v1 accepts everything (single-threaded
# front-end, no RTTI dispatch exercised)
proc InheritsFrom*(self: RootRef; cls: RootRef): bool =
  result = true

# SysUtils.RaiseLastOSError: the v1 shim is single-threaded and every
# WaitFor path returns a real result, so the v1 stub is a no-op
# (documented divergence - the error is neither captured nor raised).
proc RaiseLastOSError*() =
  discard

# Delphi Variant (v1): a boxed-any placeholder. The corpus only moves
# Variant values between declarations (TypInfo property accessors,
# fcCustomFormat's format callback) - no arithmetic or conversion is
# exercised, so one concrete shape keeps every signature type-checking.
# A real VarData model is a documented next tier.
type
  Variant* = TVarRec

  # System's TMethod (Delphi's method-pointer pair): the v1 shell keeps
  # the field names; Code stays a pointer (no nimony model for a
  # proc-typed record field shared across signatures)
  TMethod* = object
    Code*: pointer
    Data*: RootRef

  # System's TPoint
  TPoint* = object
    X*: int32
    Y*: int32

  # System's COM interface root: the v1 shell keeps the spelling
  # (TypInfo's interface accessors only move values)
  IInterface* = RootRef

  # SysUtils's conversion exception
  EConvertError* = ref object of PasException
  TVarData* {.inheritable.} = ref object of RootRef
    VType*: int32
    VString*: string
    VInteger*: int32
    VDouble*: float64
    VBoolean*: bool

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

proc Classname*(self: PasException): string =
  ## Delphi TObject.ClassName (a class function). The exception shim
  ## keeps one canonical runtime type, so the spelling is a constant -
  ## documented divergence: subclasses do not report their own name.
  result = "Exception"

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

proc fpcFormatF*(f: float64; prec: int): string =
  ## FPC's Str(:w:p) fixed-point form: FPC rounds the SHORTEST
  ## round-trip decimal representation half-away-from-zero
  ## (2.675 -> "2.68"), unlike Format's %f variable path
  var p2 = prec
  if p2 < 0: p2 = 2
  var s = $f
  var neg = false
  if s.len > 0 and s[0] == '-':
    neg = true
    s = substr(s, 1, s.len - 1)
  var digits = ""
  var point = 0
  var ep = find(s, "e", 0)
  if ep < 0: ep = find(s, "E", 0)
  var mant: string
  var expPart: string
  if ep >= 0:
    mant = substr(s, 0, ep - 1)
    expPart = substr(s, ep + 1, s.len - 1)
  else:
    mant = s
    expPart = "0"
  var seenDot = false
  var k = 0
  while k < mant.len:
    if mant[k] == '.':
      seenDot = true
      point = k
    else:
      digits.add(mant[k])
    inc k
  if not seenDot: point = mant.len
  var exp = 0
  var q = 0
  var eneg = false
  if q < expPart.len and expPart[q] in {'+', '-'}:
    eneg = expPart[q] == '-'
    inc q
  while q < expPart.len and expPart[q] >= '0' and expPart[q] <= '9':
    exp = exp * 10 + ord(expPart[q]) - ord('0')
    inc q
  if eneg: exp = -exp
  point = point + exp
  while digits.len > point and digits[digits.len - 1] == '0':
    digits = substr(digits, 0, digits.len - 2)
  let keep = point + p2
  if keep <= 0:
    digits = "0"
    point = 1
  else:
    var kept = ""
    if digits.len <= keep:
      kept = digits
      var z = digits.len
      while z < keep:
        kept.add('0')
        inc z
    else:
      kept = substr(digits, 0, keep - 1)
      if digits[keep] >= '5':
        var i = keep - 1
        var done = false
        while i >= 0 and not done:
          if kept[i] == '9':
            kept[i] = '0'
            dec i
          else:
            kept[i] = chr(ord(kept[i]) + 1)
            done = true
        if not done:
          kept = "1" & kept
          point = point + 1
    digits = kept
  var outp = ""
  if point <= 0:
    outp = "0."
    var z = 0
    while z < -point:
      outp.add('0')
      inc z
    outp.add(digits)
  else:
    if point >= digits.len:
      outp = digits
      var z = digits.len
      while z < point:
        outp.add('0')
        inc z
      if p2 > 0: outp.add('.')
      var z2 = 0
      while z2 < p2:
        outp.add('0')
        inc z2
    else:
      outp = substr(digits, 0, point - 1) & "." & substr(digits, point, digits.len - 1)
  if neg: outp = "-" & outp
  result = outp

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

const
  Maxint* = 0x7FFFFFFF'i32

# EConvertError.Create (the raise sites' only ctor)
proc Create*(self: typedesc[EConvertError]; msg: string): EConvertError =
  var r = EConvertError(Message: msg)
  result = r

# SysUtils date parsing: the corpus only exercises the `on
# EConvertError` path, so the v1 stub raises (the handler swallows it)
proc StrToDate*(s: string): TDateTime {.raises.} =
  pasCurrentExc = EConvertError.Create("StrToDate: v1 unimplemented")
  raise ValueError

proc StrToDateTime*(s: string): TDateTime {.raises.} =
  result = StrToDate(s)

proc StrToTime*(s: string): TDateTime {.raises.} =
  pasCurrentExc = EConvertError.Create("StrToTime: v1 unimplemented")
  raise ValueError

# Variants' custom-variant base (the corpus's TypInfo only inherits
# the spelling; no variant dispatch is exercised)
type
  TInvokeableVariantType* {.inheritable.} = ref object of RootRef
  EVariantError* = ref object of PasException
