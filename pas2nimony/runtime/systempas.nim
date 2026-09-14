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

proc FloatToStr*(f: float): string = applyDecSep(fpcFormatG(f, 15))
proc FloatToStr*(f: float32): string = applyDecSep(fpcFormatG(f, 15))

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

proc pasW*(v: float64; w: int32): string = applyDecSep(fpcWidthSci(v, w))

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

# Delphi TObject.InheritsFrom: v1 accepts everything (single-threaded
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

# New/Dispose: the corpus's pointer-record pools (THashedStringList's
# PHashItem) — v1 allocates nothing (the buckets stay exercise-only)
proc New*[T](p: var ptr T) =
  discard

proc Dispose*[T](p: ptr T) =
  discard

proc Dispose*(p: pointer) =
  discard

# pasCStr: Delphi's `PChar(Integer(P) + N)` pointer arithmetic has no
# nimony spelling (int<->cstring casts are rejected); the v1 shim
# answers nil and the corpus's binary-stream paths stay compile-only
proc pasCStr*(a: int32): cstring =
  result = nil

# pasLineEnding: the platform's native line terminator. Probed: nimony's
# syncio is a pure-Nimony layer with NO text-mode translation, so even a
# Windows build emits bare \n; the RTL spellings route through this
# constant, mirroring FPC's LineEnding (CRLF on Windows, LF on Unix/macOS)
when defined(MSWINDOWS):
  const pasLineEnding* = "\r\n"
else:
  const pasLineEnding* = "\n"

# TFormatSettings: the FPC-shaped locale record. v1 defaults are the
# locale-independent '.'/','/ISO-ish forms; a per-platform locale query
# (Win32 API / clocale) can fill the same fields later - Delphi reads the
# Win32 locale on Windows, FPC reads clocale on Unix, so the shim matches
# both once the values are populated.
type
  TFormatSettings* = object
    DecimalSeparator*: char
    ThousandSeparator*: char
    DateSeparator*: char
    TimeSeparator*: char
    ShortDateFormat*: string
    LongDateFormat*: string
    ShortTimeFormat*: string
    LongTimeFormat*: string

var pasFormatSettings* = TFormatSettings(
  DecimalSeparator: '.', ThousandSeparator: ',', DateSeparator: '/',
  TimeSeparator: ':', ShortDateFormat: "yyyy-mm-dd",
  LongDateFormat: "yyyy-mm-dd hh:nn:ss", ShortTimeFormat: "hh:nn",
  LongTimeFormat: "hh:nn:ss")

proc applyDecSep*(s: string): string =
  ## swap the formatter's '.' for the configured decimal separator
  let ds = pasFormatSettings.DecimalSeparator
  if ds == '.': result = s
  else:
    result = s
    var k = 0
    while k < result.len:
      if result[k] == '.': result[k] = ds
      inc k

proc pasRound*(f: float64): int64 =
  ## Delphi/FPC banker's rounding: halves go to the even neighbor
  ## (nim's round is half-away-from-zero). Truncation toward zero
  ## (nimony has no floor) plus the signed-fraction handling
  var t = int64(f)
  var frac = f - float64(t)
  if f < 0.0: frac = -frac
  if frac > 0.5:
    if f < 0.0: result = t - 1 else: result = t + 1
  elif frac < 0.5:
    result = t
  elif (t mod 2) == 0:
    result = t
  elif f < 0.0:
    result = t - 1
  else:
    result = t + 1

# sameRef: nimony's `==`/`!=` refuse ref-object operands; class
# comparisons rewrite to pointer identity (Delphi's `a = b` on objects)
proc sameRef*(a, b: RootRef): bool =
  result = cast[pointer](a) == cast[pointer](b)

type
  # Delphi Variant: TVarData's tag plus a payload wide enough to keep the
  # Delphi union views (VInteger/VLongWord/VInt64/VDouble/VCurrency/...)
  # synchronised on every store, so corpus code reading
  # `TVarData(v).VInteger` sees what Delphi sees. `TVarData` is the same
  # type, which makes Delphi's `TVarData(v)` cast an identity in Nim.
  #
  # The semantics below are *measured*, not recalled: type codes, the
  # Null/Unassigned predicates, the conversions and the operators were
  # pinned against Delphi 2007 (dcc32/Win32 under wine) and FPC 3.2.2 by
  # test/variant/vcore.pas + test/variant-oracle.sh; see
  # .dsh/wiki/pas2nimony-variant-semantics.md. Divergences are marked.
  TVarType* = uint16

  Variant* = object
    VType*: TVarType
    # integer views, kept in sync on store (a union's low-32 view of an
    # unsigned value is a two's-complement reinterpretation, as in Delphi)
    VInteger*: int32
    VLongWord*: uint32
    VSmallint*: int16
    VShortInt*: int8
    VByte*: uint8
    VWord*: uint16
    VInt64*: int64
    VUInt64*: uint64
    # real views
    VSingle*: float32
    VDouble*: float64
    VCurrency*: int64      # Delphi Currency: scaled by 10000
    VDate*: float64        # TDateTime
    # misc scalars
    VBoolean*: uint16      # WordBool
    VChar*: char
    VWideChar*: uint16
    VError*: int32
    # payloads without a scalar view
    VString*: string       # varString / varOleStr / varUString
    VObject*: RootRef      # varUnknown / varDispatch
    VArray*: RootRef       # variant array (VariantArrayObj)

  # a variant array: per-dimension bounds plus flat storage
  VariantArrayObj* = object
    dims*: seq[tuple[lo, hi: int32]]
    values*: seq[Variant]
  VariantArray* = ref VariantArrayObj

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
  # `Variant` and `TVarData` are one type: Delphi's TVarData(v) cast is
  # the identity, and the field names are the Delphi ones.
  TVarData* = Variant

# Delphi's variant type codes; measured identical in dcc32 and FPC
const
  varEmpty* = TVarType(0x0000)
  varNull* = TVarType(0x0001)
  varSmallint* = TVarType(0x0002)
  varInteger* = TVarType(0x0003)
  varSingle* = TVarType(0x0004)
  varDouble* = TVarType(0x0005)
  varCurrency* = TVarType(0x0006)
  varDate* = TVarType(0x0007)
  varOleStr* = TVarType(0x0008)
  varDispatch* = TVarType(0x0009)
  varError* = TVarType(0x000A)
  varBoolean* = TVarType(0x000B)
  varVariant* = TVarType(0x000C)
  varUnknown* = TVarType(0x000D)
  varShortInt* = TVarType(0x0010)
  varByte* = TVarType(0x0011)
  varWord* = TVarType(0x0012)
  varLongWord* = TVarType(0x0013)
  varInt64* = TVarType(0x0014)
  varUInt64* = TVarType(0x0015)   # FPC only: D2007 stores UInt64 as varInt64
  varString* = TVarType(0x0100)   # Delphi's AnsiString variant
  varAny* = TVarType(0x0101)      # FPC only (D2007: E2003)
  varUString* = TVarType(0x0102)  # FPC / Delphi 2009+
  varArray* = TVarType(0x2000)    # OR'd into the tag of a variant array
  varParam* = TVarType(0x4000)    # OR'd in for an untyped `var` parameter

type
  PVariant* = ptr Variant

# The three special values. Measured: Null is Null but NOT clear; Unassigned
# is both Empty and Clear; EmptyParam is a varError variant carrying
# DISP_E_PARAMNOTFOUND. They are module-level `let`s rather than `const`s:
# nimony cannot fold a Variant construction (its ref field would have to be
# defaulted) at compile time.
let
  Null* = Variant(VType: varNull)
  Unassigned* = Variant(VType: varEmpty)
  EmptyParam* = Variant(VType: varError, VError: -2147352572'i32)

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
proc IntToHex*(value: uint16; digits: int32): string =
  ## TVarType is a Word; Delphi's Hex(VarType) helper passes it directly
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

# SysUtils const tier (the corpus's string/char surface; FPC/Delphi
# spellings with the shared defaults)
const
  NameValueSeparator* = '='
  PathDelim* = '/'
  DriveDelim* = ':'
  ExtensionSeparator* = '.'

proc pasFind*(s: string; sub: string): int32 = int32(find(s, sub))
proc pasFind*(s: string; sub: char): int32 = int32(find(s, sub))

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
  # Delphi's hierarchy: the cast failure is a *subclass*, so
  # `except on E: EVariantError` catches it (measured: dcc32 raises
  # EVariantTypeCastError where FPC raises plain EVariantError)
  EVariantTypeCastError* = ref object of EVariantError

proc Create*(self: typedesc[EVariantError]; msg: string): EVariantError =
  result = EVariantError(Message: msg)

proc Create*(self: typedesc[EVariantTypeCastError];
             msg: string): EVariantTypeCastError =
  result = EVariantTypeCastError(Message: msg)

# ---------------------------------------------------------------------------
# Variant: construction
#
# Measured: dcc32 stores a *literal* by its value (3 -> varByte,
# -3 -> varShortInt, 70000 -> varLongWord) but a typed Integer/Int64 keeps
# its own tag, and Single widens to varDouble. nimony unifies `int` and
# `int64` AND does not apply converters at all (measured: `f(3)` fails with
# a matching converter in scope), so the emitter emits the construction
# explicitly: toVariant(...) for a typed value, pasVarLit(...) for nkIntLit.
# The converters below are kept for tooling that honours them and to
# document the mapping.

proc vInt(tag: TVarType; x: int64): Variant {.noSideEffect.} =
  ## every integer view of the union is written, so a read through any of
  ## them matches Delphi's bit pattern
  result = Variant(VType: tag)
  result.VInt64 = x
  result.VUInt64 = cast[uint64](x)
  result.VInteger = int32(x and 0xFFFFFFFF'i64)
  result.VLongWord = cast[uint32](result.VInteger)
  result.VWord = uint16(result.VLongWord and 0xFFFF'u32)
  result.VByte = uint8(uint32(result.VWord) and 0xFF'u32)
  result.VSmallint = cast[int16](result.VWord)
  result.VShortInt = cast[int8](result.VByte)

proc vReal(tag: TVarType; x: float64): Variant {.noSideEffect.} =
  result = Variant(VType: tag)
  result.VDouble = x
  result.VSingle = float32(x)
  if tag == varDate:
    result.VDate = x

proc vCurrency(cu: int64): Variant {.noSideEffect.} =
  ## Delphi Currency: an int64 scaled by 10000 (the same 8 bytes as VInt64)
  result = Variant(VType: varCurrency)
  result.VCurrency = cu
  result.VInt64 = cu
  result.VUInt64 = cast[uint64](cu)
  result.VDouble = float64(cu) / 10000.0
  result.VSingle = float32(result.VDouble)

proc pasVarCurrF*(x: float64): Variant =
  ## a Pascal Currency-typed expression reaches the shim as float64
  ## (rtlSpelling maps Currency to float64), but its Variant tag must be
  ## varCurrency (measured: 0006 in both oracles)
  vCurrency(int64(pasRound(x * 10000.0)))

proc pasVarCurr*(cu: int64): Variant {.noSideEffect.} =
  ## a Pascal Currency value (already scaled by 10000) stored as varCurrency;
  ## the emitter routes Currency-typed expressions here, since neither the
  ## int64 nor the float64 constructor would produce the right tag
  vCurrency(cu)

proc pasVarDateF*(x: TDateTime): Variant {.noSideEffect.} =
  ## TDateTime is a float64 alias here, but its Variant tag is varDate
  ## (measured: 0007 in both oracles)
  vReal(varDate, x)

proc pasVarWStr*(c: char): Variant {.noSideEffect.} =
  ## a Char element of `VarArrayOf([...])` is varOleStr as well (measured);
  ## nimony has no `$char` - concatenation is the only conversion
  vStr(varOleStr, "" & c)

proc pasVarWStr*(s: string): Variant {.noSideEffect.} =
  ## a WideString arrives as a plain string; both oracles tag it varOleStr
  ## (measured: 0008)
  vStr(varOleStr, s)

proc vStr(tag: TVarType; s: string): Variant {.noSideEffect.} =
  result = Variant(VType: tag)
  result.VString = s

proc vBool(b: bool): Variant {.noSideEffect.} =
  result = Variant(VType: varBoolean)
  result.VBoolean = if b: 1'u16 else: 0'u16
  result.VInteger = if b: 1'i32 else: 0'i32

proc pasVarLit*(x: int64): Variant {.noSideEffect.} =
  ## Pascal integer *literals*, as dcc32 types them: the narrowest type that
  ## holds the value, preferring the unsigned Byte/Word/LongWord for
  ## non-negative literals (measured: 300 -> varWord, 32768 -> varWord,
  ## 70000 -> varLongWord, -70000 -> varInteger).
  result = Variant(VType: varEmpty)
  if x >= 0:
    if x <= 255: result = vInt(varByte, x)
    elif x <= 65535: result = vInt(varWord, x)
    elif x <= 4294967295'i64: result = vInt(varLongWord, x)
    else: result = vInt(varInt64, x)
  else:
    if x >= -128: result = vInt(varShortInt, x)
    elif x >= -32768: result = vInt(varSmallint, x)
    elif x >= -2147483648'i64: result = vInt(varInteger, x)
    else: result = vInt(varInt64, x)

converter toVariant*(x: int32): Variant = vInt(varInteger, int64(x))
converter toVariant*(x: int64): Variant = vInt(varInt64, x)
converter toVariant*(x: int16): Variant = vInt(varSmallint, int64(x))
converter toVariant*(x: int8): Variant = vInt(varShortInt, int64(x))
converter toVariant*(x: uint32): Variant = vInt(varLongWord, int64(x))
converter toVariant*(x: uint16): Variant = vInt(varWord, int64(x))
converter toVariant*(x: uint8): Variant = vInt(varByte, int64(x))
converter toVariant*(x: uint64): Variant = vInt(varInt64, cast[int64](x))
  ## measured divergence: Delphi has no unsigned 64-bit variant and stores
  ## UInt64 as varInt64; FPC 3.2.2 stores varUInt64 (0x0015)
converter toVariant*(x: float64): Variant = vReal(varDouble, x)
  ## measured divergence: dcc32 stores a decimal *literal* as varCurrency,
  ## FPC as varDouble. A Nim float64 cannot tell a literal from a typed
  ## Double, so this follows FPC; the emitter can fix it later by routing
  ## nkFloatLit through the shim (see the wiki's open decisions).
converter toVariant*(x: float32): Variant = vReal(varDouble, float64(x))
  ## measured: Single and Double both land in varDouble in dcc32 and FPC
converter toVariant*(x: string): Variant = vStr(varString, x)
converter toVariant*(x: bool): Variant = vBool(x)
converter toVariant*(x: char): Variant = vStr(varString, $x)
  ## measured: a Char is stored as a one-character varString in both
converter toVariant*(x: RootRef): Variant =
  result = Variant(VType: varUnknown)
  result.VObject = x

# ---------------------------------------------------------------------------
# Variant: predicates and inspection

proc VarType*(v: Variant): TVarType = v.VType
proc VarIsNull*(v: Variant): bool = v.VType == varNull
proc VarIsEmpty*(v: Variant): bool = v.VType == varEmpty
proc VarIsClear*(v: Variant): bool = v.VType == varEmpty
  ## measured: Null is NOT clear (Clear == Unassigned/varEmpty)
proc VarIsArray*(v: Variant): bool = (v.VType and varArray) != TVarType(0)
proc VarArrayDimCount*(v: Variant): int32 =
  let a = cast[VariantArray](v.VArray)
  if a == nil: result = 0 else: result = int32(a.dims.len)
proc VarArrayLowBound*(v: Variant; dim: int32): int32 =
  let a = cast[VariantArray](v.VArray)
  if a == nil or dim < 1 or dim > a.dims.len: result = 0
  else: result = a.dims[dim - 1].lo
proc VarArrayHighBound*(v: Variant; dim: int32): int32 =
  let a = cast[VariantArray](v.VArray)
  if a == nil or dim < 1 or dim > a.dims.len: result = -1
  else: result = a.dims[dim - 1].hi

proc isIntTag(t: TVarType): bool =
  t in {varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord,
        varInt64, varUInt64}
proc isRealTag(t: TVarType): bool =
  t in {varSingle, varDouble, varDate, varCurrency}
proc isStrTag(t: TVarType): bool =
  t in {varString, varOleStr, varUString}

proc asInt64(v: Variant): int64 =
  ## the *signed* reading Delphi's arithmetic uses per tag
  if v.VType in {varInt64, varUInt64}: result = v.VInt64
  elif v.VType == varLongWord: result = int64(v.VLongWord)
  elif v.VType == varWord: result = int64(v.VWord)
  elif v.VType == varByte: result = int64(v.VByte)
  elif v.VType == varSmallint: result = int64(v.VSmallint)
  elif v.VType == varShortInt: result = int64(v.VShortInt)
  else: result = int64(v.VInteger)

proc asFloat64(v: Variant): float64 =
  case v.VType
  of varSingle: result = float64(v.VSingle)
  of varDouble, varDate: result = v.VDouble
  of varCurrency: result = float64(v.VCurrency) / 10000.0
  of varBoolean: result = float64(v.VBoolean)
  else: result = float64(asInt64(v))

proc variantToStr(v: Variant): string =
  ## measured: Null and Unassigned both render as ''; Boolean as
  ## 'True'/'False'; a varLongWord renders *unsigned* (4000000000) even
  ## though VInteger reads back signed; a real uses the locale separator in
  ## dcc32 but '.' in FPC (we follow FPC - the pinned DecimalSeparator)
  case v.VType
  of varEmpty, varNull: result = ""
  of varBoolean: result = if v.VBoolean != 0'u16: "True" else: "False"
  of varCurrency:
    let n = v.VCurrency
    let neg = n < 0
    let m = if neg: -n else: n
    var s = $(m div 10000)
    var frac = m mod 10000
    if frac != 0:
      var fs = ""
      for i in 0 ..< 4:
        fs = $(frac mod 10) & fs
        frac = frac div 10
      while fs.len > 0 and fs[^1] == '0': fs.setLen(fs.len - 1)
      s = s & "." & fs
    result = (if neg: "-" else: "") & s
  of varSingle: result = FloatToStr(v.VSingle)
  of varDouble, varDate: result = FloatToStr(v.VDouble)
  of varString, varOleStr, varUString: result = v.VString
  of varLongWord: result = $v.VLongWord
  of varWord: result = $v.VWord
  of varByte: result = $v.VByte
  else: result = $asInt64(v)

proc parseCurr(s: string): int64 {.raises.} =
  ## '4.25' -> 42500 (Currency is scaled by 10000)
  var i = 0
  var neg = false
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    neg = s[i] == '-'
    inc i
  var whole = 0'i64
  var seen = false
  while i < s.len and s[i] in {'0'..'9'}:
    whole = whole * 10 + int64(ord(s[i]) - ord('0'))
    seen = true
    inc i
  var frac = 0'i64
  var scale = 1000'i64
  if i < s.len and s[i] == '.':
    inc i
    while i < s.len and s[i] in {'0'..'9'}:
      if scale > 0:
        frac = frac + int64(ord(s[i]) - ord('0')) * scale
        scale = scale div 10
      seen = true
      inc i
  if not seen:
    pasCurrentExc = EVariantTypeCastError.Create("Invalid variant type cast")
    raise ValueError
  result = whole * 10000 + frac
  if neg: result = -result

proc parseInt64(s: string): int64 {.raises.} =
  var i = 0
  while i < s.len and s[i] in {' ', '\t'}: inc i
  var neg = false
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    neg = s[i] == '-'
    inc i
  var n = 0'i64
  var seen = false
  while i < s.len and s[i] in {'0'..'9'}:
    n = n * 10 + int64(ord(s[i]) - ord('0'))
    seen = true
    inc i
  if not seen:
    pasCurrentExc = EVariantTypeCastError.Create("Invalid variant type cast")
    raise ValueError
  result = if neg: -n else: n

proc parseFloat64(s: string): float64 {.raises.} =
  var i = 0
  while i < s.len and s[i] in {' ', '\t'}: inc i
  var neg = false
  if i < s.len and (s[i] == '-' or s[i] == '+'):
    neg = s[i] == '-'
    inc i
  var whole = 0.0
  var seen = false
  while i < s.len and s[i] in {'0'..'9'}:
    whole = whole * 10.0 + float64(ord(s[i]) - ord('0'))
    seen = true
    inc i
  if i < s.len and s[i] == '.':
    inc i
    var scale = 0.1
    while i < s.len and s[i] in {'0'..'9'}:
      whole = whole + float64(ord(s[i]) - ord('0')) * scale
      scale = scale / 10.0
      seen = true
      inc i
  if not seen:
    pasCurrentExc = EVariantTypeCastError.Create("Invalid variant type cast")
    raise ValueError
  result = if neg: -whole else: whole

proc numericOf(v: Variant): float64 {.raises.} =
  if isStrTag(v.VType): result = parseFloat64(v.VString)
  else: result = asFloat64(v)

proc intOf(v: Variant): int64 {.raises.} =
  ## measured: a real rounds half to even (1.9 -> 2, 2.5 -> 2); a numeric
  ## string is parsed ('42' -> 42); a non-numeric string raises
  if isStrTag(v.VType): result = parseInt64(v.VString)
  else: result = pasRound(asFloat64(v))

proc VarAsType*(v: Variant; t: TVarType): Variant {.raises.} =
  if v.VType == t: return v
  if v.VType in {varEmpty, varNull}: return v
  case t
  of varSmallint, varShortInt, varByte, varWord, varLongWord, varInteger,
     varInt64, varUInt64:
    result = vInt(t, intOf(v))
  of varSingle, varDouble, varDate:
    result = vReal(if t == varDate: varDate else: varDouble, numericOf(v))
  of varCurrency:
    if isStrTag(v.VType): result = vCurrency(parseCurr(v.VString))
    else: result = vCurrency(int64(asFloat64(v) * 10000.0))
  of varBoolean:
    if isStrTag(v.VType): result = vBool(v.VString == "True")
    else: result = vBool(asFloat64(v) != 0.0)
  of varString, varOleStr, varUString:
    result = vStr(t, variantToStr(v))
  else:
    pasCurrentExc = EVariantTypeCastError.Create("Invalid variant conversion")
    raise ValueError

proc VarToStr*(v: Variant): string = variantToStr(v)
proc VarToWideStr*(v: Variant): string = variantToStr(v)
proc VarToStrDef*(v: Variant; default: string): string =
  if v.VType in {varEmpty, varNull}: result = default
  else: result = variantToStr(v)

# ---------------------------------------------------------------------------
# Variant: operators
#
# Promotion measured in dcc32 and FPC: int op int -> varInteger (varInt64
# when either side is 64-bit), a real mixed in -> varDouble, str + str ->
# varString, `/` always -> varDouble, and a type mismatch raises. Null
# swallows the operation (measured: Null + int -> Null).

proc vNullOr(a, b: Variant): bool = a.VType == varNull or b.VType == varNull

# Measured promotion (dcc32 + FPC agree):
#   int op int            -> varInteger
#   LongWord/Int64/UInt64 -> varInt64   (measured: varLongWord + varByte = 0014)
#   Currency with integer -> varCurrency (measured: varInteger + varCurrency = 0006)
#   Double/Single mixed in-> varDouble   (`/` always)
type
  VPromoKind = enum vpkInt, vpkInt64, vpkDouble, vpkCurrency

proc promoKind(a, b: Variant): VPromoKind =
  let aCur = a.VType == varCurrency
  let bCur = b.VType == varCurrency
  let aReal = a.VType in {varSingle, varDouble}
  let bReal = b.VType in {varSingle, varDouble}
  if aCur or bCur:
    result = if aReal or bReal: vpkDouble else: vpkCurrency
  elif aReal or bReal or a.VType == varDate or b.VType == varDate:
    result = vpkDouble
  elif a.VType in {varInt64, varUInt64, varLongWord} or
       b.VType in {varInt64, varUInt64, varLongWord}:
    result = vpkInt64
  else:
    result = vpkInt

proc promoAdd(a, b: Variant): float64 = asFloat64(a) + asFloat64(b)
proc promoSub(a, b: Variant): float64 = asFloat64(a) - asFloat64(b)
proc promoMul(a, b: Variant): float64 = asFloat64(a) * asFloat64(b)

proc mismatched(): Variant {.raises.} =
  pasCurrentExc = EVariantTypeCastError.Create("Invalid variant operation")
  raise ValueError

proc pasVarAdd*(a, b: Variant): Variant {.raises.} =
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) and isStrTag(b.VType):
    return vStr(varString, a.VString & b.VString)
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  case promoKind(a, b)
  of vpkCurrency: result = vCurrency(int64(pasRound(promoAdd(a, b) * 10000.0)))
  of vpkDouble: result = vReal(varDouble, promoAdd(a, b))
  of vpkInt64: result = vInt(varInt64, asInt64(a) + asInt64(b))
  of vpkInt: result = vInt(varInteger, asInt64(a) + asInt64(b))

proc pasVarSub*(a, b: Variant): Variant {.raises.} =
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  case promoKind(a, b)
  of vpkCurrency: result = vCurrency(int64(pasRound(promoSub(a, b) * 10000.0)))
  of vpkDouble: result = vReal(varDouble, promoSub(a, b))
  of vpkInt64: result = vInt(varInt64, asInt64(a) - asInt64(b))
  of vpkInt: result = vInt(varInteger, asInt64(a) - asInt64(b))

proc pasVarMul*(a, b: Variant): Variant {.raises.} =
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  case promoKind(a, b)
  of vpkCurrency: result = vCurrency(int64(pasRound(promoMul(a, b) * 10000.0)))
  of vpkDouble: result = vReal(varDouble, promoMul(a, b))
  of vpkInt64: result = vInt(varInt64, asInt64(a) * asInt64(b))
  of vpkInt: result = vInt(varInteger, asInt64(a) * asInt64(b))

proc pasVarDiv*(a, b: Variant): Variant {.raises.} =
  ## `/` always yields varDouble (measured)
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  result = vReal(varDouble, asFloat64(a) / asFloat64(b))

proc intWidth64(a, b: Variant): bool =
  ## the measured widening rule: LongWord/Int64/UInt64 force a 64-bit result
  a.VType in {varInt64, varUInt64, varLongWord} or
    b.VType in {varInt64, varUInt64, varLongWord}

proc pasVarIDiv*(a, b: Variant): Variant {.raises.} =
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  if intWidth64(a, b): return vInt(varInt64, asInt64(a) div asInt64(b))
  result = vInt(varInteger, asInt64(a) div asInt64(b))

proc pasVarMod*(a, b: Variant): Variant {.raises.} =
  if vNullOr(a, b): return Null
  if isStrTag(a.VType) or isStrTag(b.VType): return mismatched()
  if intWidth64(a, b): return vInt(varInt64, asInt64(a) mod asInt64(b))
  result = vInt(varInteger, asInt64(a) mod asInt64(b))

proc pasVarNeg*(a: Variant): Variant {.raises.} =
  if a.VType == varNull: return Null
  if isRealTag(a.VType): return vReal(varDouble, -asFloat64(a))
  result = vInt(varInteger, -asInt64(a))

proc pasVarCmp*(a, b: Variant): int =
  ## Delphi's VarCmp ordering. Measured: Null = Null is True, Null =
  ## Unassigned is False (no exception), Unassigned < Null; a Null against
  ## a value orders first, as COM does.
  if a.VType == varNull or b.VType == varNull:
    if a.VType == varNull and b.VType == varNull: return 0
    if a.VType == varNull: return -1
    return 1
  if a.VType == varEmpty or b.VType == varEmpty:
    if a.VType == varEmpty and b.VType == varEmpty: return 0
    if a.VType == varEmpty: return -1
    return 1
  if isStrTag(a.VType) or isStrTag(b.VType):
    let sa = variantToStr(a)
    let sb = variantToStr(b)
    if sa < sb: return -1
    if sa > sb: return 1
    return 0
  if isRealTag(a.VType) or isRealTag(b.VType):
    let x = asFloat64(a)
    let y = asFloat64(b)
    if x < y: return -1
    if x > y: return 1
    return 0
  let x = asInt64(a)
  let y = asInt64(b)
  if x < y: return -1
  if x > y: return 1
  result = 0

proc pasVarEq*(a, b: Variant): bool = pasVarCmp(a, b) == 0
proc pasVarNe*(a, b: Variant): bool = pasVarCmp(a, b) != 0
proc pasVarLt*(a, b: Variant): bool = pasVarCmp(a, b) < 0
proc pasVarLe*(a, b: Variant): bool = pasVarCmp(a, b) <= 0
proc pasVarGt*(a, b: Variant): bool = pasVarCmp(a, b) > 0
proc pasVarGe*(a, b: Variant): bool = pasVarCmp(a, b) >= 0

# Pascal's operators (`v + w`), routed to the named implementations above so
# the emitter has one spelling per operation. They raise on a type mismatch
# exactly as Delphi does, which means a call site must sit in a try/except or
# in a {.raises.} routine - nimony's rule for any raising call.
proc `+`*(a, b: Variant): Variant {.raises.} = pasVarAdd(a, b)
proc `-`*(a, b: Variant): Variant {.raises.} = pasVarSub(a, b)
proc `*`*(a, b: Variant): Variant {.raises.} = pasVarMul(a, b)
proc `/`*(a, b: Variant): Variant {.raises.} = pasVarDiv(a, b)
proc `div`*(a, b: Variant): Variant {.raises.} = pasVarIDiv(a, b)
proc `mod`*(a, b: Variant): Variant {.raises.} = pasVarMod(a, b)
proc `-`*(a: Variant): Variant {.raises.} = pasVarNeg(a)
proc `==`*(a, b: Variant): bool = pasVarEq(a, b)
proc `<`*(a, b: Variant): bool = pasVarLt(a, b)
proc `<=`*(a, b: Variant): bool = pasVarLe(a, b)
proc `>`*(a, b: Variant): bool = pasVarGt(a, b)
proc `>=`*(a, b: Variant): bool = pasVarGe(a, b)

# ---------------------------------------------------------------------------
# Variant arrays
#
# Delphi's tag carries the varArray bit plus the element type; the bounds
# arrive as (lo, hi) pairs. Storage is flat with per-dimension strides.

proc VarArrayCreate*(bounds: openArray[int32]; elemType: TVarType): Variant =
  var a = VariantArray(dims: @[])
  var i = 0
  var total = 1
  while i + 1 < bounds.len:
    let lo = bounds[i]
    let hi = bounds[i + 1]
    a.dims.add((lo: lo, hi: hi))
    total = total * int(hi - lo + 1)
    i = i + 2
  a.values = newSeq[Variant](total)
  for j in 0 ..< total:
    a.values[j] = Variant(VType: elemType)
  result = Variant(VType: varArray or elemType)
  result.VArray = cast[RootRef](a)

proc VarArrayOf*(values: openArray[Variant]): Variant =
  var a = VariantArray(dims: @[(lo: 0'i32, hi: int32(values.len) - 1)],
                       values: @[])
  for v in values: a.values.add(v)
  result = Variant(VType: varArray or varVariant)
  result.VArray = cast[RootRef](a)

proc arrayIndexOf(a: VariantArray; indices: openArray[int32]): int =
  result = 0
  var stride = 1
  var i = a.dims.len - 1
  while i >= 0:
    let idx = int(indices[i]) - int(a.dims[i].lo)
    result = result + idx * stride
    stride = stride * int(a.dims[i].hi - a.dims[i].lo + 1)
    dec i

proc `[]`*(v: Variant; i: int32): Variant {.raises.} =
  let a = cast[VariantArray](v.VArray)
  if a == nil:
    pasCurrentExc = EVariantError.Create("Variant is not an array")
    raise ValueError
  result = a.values[arrayIndexOf(a, [i])]

proc `[]=`*(v: var Variant; i: int32; x: Variant) {.raises.} =
  let a = cast[VariantArray](v.VArray)
  if a == nil:
    pasCurrentExc = EVariantError.Create("Variant is not an array")
    raise ValueError
  a.values[arrayIndexOf(a, [i])] = x

proc VarArrayGet*(v: Variant; indices: openArray[int32]): Variant =
  let a = cast[VariantArray](v.VArray)
  if a == nil: result = Unassigned
  else: result = a.values[arrayIndexOf(a, indices)]

proc VarArrayPut*(v: var Variant; x: Variant; indices: openArray[int32]) =
  let a = cast[VariantArray](v.VArray)
  if a != nil: a.values[arrayIndexOf(a, indices)] = x

proc VarArrayRedim*(v: var Variant; highBound: int32) =
  let a = cast[VariantArray](v.VArray)
  if a != nil and a.dims.len == 1:
    let lo = a.dims[0].lo
    a.dims[0] = (lo: lo, hi: highBound)
    var total = int(highBound - lo + 1)
    if total < 0: total = 0
    let elem = if a.values.len > 0: a.values[0].VType else: varVariant
    let old = a.values.len
    a.values.setLen(total)
    for j in old ..< total:
      a.values[j] = Variant(VType: elem)

# ---------------------------------------------------------------------------
# Delphi memory management
#
# Delphi's `AllocMem`/`FreeMem`/`ReallocMem` family maps onto nimony's
# allocator (`alloc0`/`realloc`/`dealloc`); the Pascal spellings take a
# size in any Integer width, and `AllocMem` is the zero-filling one.
# `Move`/`FillChar` are the untyped pair - they take the *value* (not an
# address) and are emitted generically, with `var` on the destination so
# a dereferenced pointer (`p[]`) or an array element binds directly. They
# are spelled `pasMove`/`pasFillChar` (see RtlNames) so the generic
# `var`-parameter overloads cannot hijack a user method named `Move`.

proc AllocMem*(size: int32): pointer = alloc0(int(size))
proc AllocMem*(size: uint32): pointer = alloc0(int(size))
proc AllocMem*(size: int64): pointer = alloc0(int(size))
proc AllocMem*(size: uint64): pointer = alloc0(int(size))

proc FreeMem*(p: pointer) = dealloc(p)
proc FreeMem*(p: pointer; size: int32) = dealloc(p)
proc FreeMem*(p: pointer; size: uint32) = dealloc(p)
proc FreeMem*(p: pointer; size: int64) = dealloc(p)
proc FreeMem*(p: pointer; size: uint64) = dealloc(p)
proc FreeMemory*(p: pointer) = dealloc(p)

proc ReallocMem*(p: var pointer; size: int32) = p = realloc(p, int(size))
proc ReallocMem*(p: var pointer; size: uint32) = p = realloc(p, int(size))
proc ReallocMemory*(p: pointer; size: int32): pointer = realloc(p, int(size))
proc ReallocMemory*(p: pointer; size: uint32): pointer = realloc(p, int(size))
proc ReallocMemory*(p: pointer; size: int64): pointer = realloc(p, int(size))
proc ReallocMemory*(p: pointer; size: uint64): pointer = realloc(p, int(size))

proc pasMove*[S, D](src: var S; dest: var D; count: int32) =
  moveMem(addr dest, addr src, int(count))
proc pasMove*[S, D](src: var S; dest: var D; count: int64) =
  moveMem(addr dest, addr src, int(count))
proc pasMove*[S, D](src: var S; dest: var D; count: uint32) =
  moveMem(addr dest, addr src, int(count))

proc pasFillChar*[D](dest: var D; count: int32; value: uint8) =
  var p = addr dest
  var i: int32 = 0
  while i < count:
    cast[ptr uint8](cast[uint](p) + uint(i))[] = value
    i = i + 1
proc pasFillChar*[D](dest: var D; count: int64; value: uint8) =
  pasFillChar(dest, int32(count), value)
proc pasFillChar*[D](dest: var D; count: uint32; value: uint8) =
  pasFillChar(dest, int32(count), value)
proc pasFillChar*[D](dest: var D; count: int32; value: string) =
  # Delphi accepts a 1-char string where a Byte is expected
  let v = if value.len > 0: uint8(value[0]) else: uint8(0)
  pasFillChar(dest, count, v)
