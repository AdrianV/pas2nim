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
proc IntToStr*(i: int): string = $i
proc IntToStr*(i: uint32): string = $i
proc IntToStr*(i: uint64): string = $i

proc FloatToStr*(f: float): string = $f
proc FloatToStr*(f: float32): string = $f

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
