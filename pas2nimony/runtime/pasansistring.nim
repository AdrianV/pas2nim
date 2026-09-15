## Delphi AnsiString model for the nimony chain.
##
## Ported from /home/adrian/dev/nimbackend/delphi32.nim (Nim 1, 32-bit)
## to nimony/64-bit:
##   - getMemory/freeMemory/reallocMemory -> alloc0/dealloc/realloc
##   - system.atomicInc/atomicDec        -> std/atomics atomicFetchAdd/Sub
##   - ByteAddress                       -> uint64
##   - {.bycopy.} dropped
##   - union helpers (SomeDelphiString, AnsiString|AnsiStringData) split
##     into concrete overloads: nimony does not resolve fields/methods
##     through an object union.
##   - addr s[0] -> cast[pointer](toCString(s)): nimony rejects an
##     address of a not-yet-written string element.
##
## Layout: an AnsiString is a pointer to the character data; the StrRec
## (refCnt, length) sits immediately before it. refCnt > 0 is a heap
## string; refCnt < 0 marks a string literal (never freed, never mutated
## in place). Assignment shares (copy-on-write); any mutation goes through
## uniqueString/uniqueStringOfLen first.

import std/atomics

type
  StrRec* {.pure, final.} = object
    refCnt*: int32
    length*: int32
  StrRecPtr* = ptr StrRec
  AnsiStringData = ptr UncheckedArray[char]
  AnsiString* {.pure, final.} = object
    data: AnsiStringData
  WeakPartialString* {.pure, final.} = object
    data: AnsiStringData
    delta*: int32
    length*: int32

template needLength(len: int32): int32 =
  let length = len
  int32(length + int32(sizeof(StrRec)) + 1'i32 + ((length + 1'i32) and 1'i32))

proc newAnsiString(length: int32): AnsiStringData =
  result = cast[AnsiStringData](0)
  if length > 0:
    var p = cast[StrRecPtr](alloc0(int(needLength(length))))
    result = cast[AnsiStringData](cast[uint64](p) + uint64(sizeof(StrRec)))
    p.length = length
    p.refCnt = 1
    cast[ptr int16](cast[uint64](result) +
        uint64(length and not 1'i32))[] = 0'i16

template raw*(s: AnsiString): pointer = cast[pointer](s.data)

template rec*(s: AnsiStringData): StrRecPtr =
  cast[StrRecPtr](cast[uint64](s) - uint64(sizeof(StrRec)))

template rec*(s: AnsiString): StrRecPtr = s.data.rec

template rec*(s: WeakPartialString): StrRecPtr =
  cast[StrRecPtr](cast[uint64](s.data) - uint64(sizeof(StrRec)) -
      uint64(s.delta))

template strData(r: StrRecPtr): AnsiStringData =
  cast[AnsiStringData](cast[uint64](r) + uint64(sizeof(StrRec)))

template isNil*(s: AnsiString): bool = cast[uint64](s.data) == 0
template isNil*(s: WeakPartialString): bool = cast[uint64](s.data) == 0

proc refCount*(s: AnsiString): int {.inline.} =
  result = 0
  if not isNil(s):
    result = int(s.rec.refCnt)

proc len*(s: AnsiString): int32 {.inline.} =
  result = 0'i32
  if not isNil(s):
    result = s.rec.length

proc len*(s: WeakPartialString): int32 {.inline.} = s.length

template low*(s: AnsiString): int32 = 0'i32

proc high*(s: AnsiString): int32 =
  result = 0'i32
  result = s.len - 1'i32

proc toString*(s: AnsiString): string =
  ## Build the nimony string from the AnsiString buffer as a C string:
  ## every AnsiString carries a terminating zero just past its data (set
  ## in newAnsiString), and nimony's fromCString copies from a cstring
  ## without us assuming anything about the nimony string layout.
  ## Tradeoff: like every cstring round-trip, an embedded \0 truncates;
  ## Delphi lengths an AnsiString by its header, so a string with an
  ## embedded \0 must not be routed through this conversion.
  if isNil(s):
    result = ""
  else:
    result = fromCString(cast[cstring](s.data))

template `$`*(s: AnsiString): string = toString(s)

template incRef(s: AnsiString) =
  let p = s.rec
  if p[].refCnt > 0:
    discard atomicFetchAdd(p[].refCnt, 1'i32)

template decRef(s: AnsiString) =
  let p = s.rec
  if p[].refCnt > 0:
    if atomicFetchSub(p[].refCnt, 1'i32) == 1'i32:
      dealloc(cast[pointer](p))

template decRefD(s: AnsiStringData) =
  let p = s.rec
  if p[].refCnt > 0:
    if atomicFetchSub(p[].refCnt, 1'i32) == 1'i32:
      dealloc(cast[pointer](p))

proc `=destroy`*(s: var AnsiString) =
  if not isNil(s):
    decRef(s)
    s.data = nil

proc `=sink`*(a: var AnsiString; b: AnsiString) =
  if not isNil(a):
    decRef(a)
  a.data = b.data

proc `=dup`*(s: AnsiString): AnsiString =
  ## nimony's initialization/copy hook (Nim 1 called this `=`): share
  ## the buffer and take a reference. A string-literal record
  ## (refCnt < 0) is immutable, so sharing is safe there too.
  result = default(AnsiString)
  if not isNil(s):
    result.data = s.data
    let p = s.rec
    discard atomicFetchAdd(p[].refCnt, 1'i32)

proc `=copy`*(dest: var AnsiString; source: AnsiString) =
  ## explicit assignment hook (used where the compiler does not choose
  ## the `=dup` path): same sharing, old value released.
  if dest.data == source.data: return
  var s = source.data
  if not isNil(source):
    var p = source.rec
    if p[].refCnt < 0:
      let length = p.length
      s = newAnsiString(length)
      copyMem(cast[pointer](s), cast[pointer](source.data), int(length))
      p = s.rec
    discard atomicFetchAdd(p[].refCnt, 1'i32)
  let d = dest.data
  dest.data = s
  if cast[uint64](d) != 0:
    decRefD(d)

proc uniqueStringOfLen*(s: var AnsiString; wantedLen: int32) =
  let data = s.data
  let minLen = if wantedLen > s.len: s.len else: wantedLen
  if data == nil or data.rec.refCnt > 1:
    s.data = newAnsiString(wantedLen)
    if data != nil:
      for i in 0'i32 ..< minLen:
        s.data[int(i)] = data[int(i)]
      decRefD(data)
  else:
    let p = cast[StrRecPtr](realloc(cast[pointer](data.rec), int(needLength(wantedLen))))
    s.data = cast[AnsiStringData](cast[uint64](p) + uint64(sizeof(StrRec)))
    p.refCnt = 1
    p.length = wantedLen
    cast[ptr int16](cast[uint64](s.data) +
        uint64(wantedLen and not 1'i32))[] = 0'i16

proc uniqueStringImpl(s: var AnsiString; r: StrRecPtr) =
  let wantedLen = r.length
  s.data = newAnsiString(wantedLen)
  copyMem(cast[pointer](s.data), cast[pointer](r.strData), int(wantedLen))
  discard atomicFetchSub(r.refCnt, 1'i32)

template uniqueString*(s: var AnsiString) =
  if not s.isNil:
    let res = s.rec
    if res.refCnt > 1:
      uniqueStringImpl(s, res)

template `[]`*(s: AnsiString; x: int): char = s.data[x]
template `[]`*(s: AnsiString; x: int32): char = s.data[int(x)]
template `[]`*(s: WeakPartialString; x: int): char = s.data[x]
template `[]`*(s: WeakPartialString; x: int32): char = s.data[int(x)]

proc toAnsiString*(s: string): AnsiString =
  result = default(AnsiString)
  let length = int32(s.len)
  if length > 0:
    result.data = newAnsiString(length)
    var v = s
    copyMem(cast[pointer](result.data), cast[pointer](toCString(v)), int(length))

proc toAnsiString*(s: cstring): AnsiString =
  result = default(AnsiString)
  let p = cast[ptr UncheckedArray[char]](s)
  var n = 0'i32
  while p[int(n)] != '\0':
    inc n
  if n > 0:
    result.data = newAnsiString(n)
    copyMem(cast[pointer](result.data), cast[pointer](p), int(n))

# --- varString slot bridge --------------------------------------------------
# Delphi/FPC keep a variant's string in a *pointer* slot, hard-cast to an
# AnsiString only where the tag says varString (varianth.inc:
# "varstring : (vstring : pointer)"; variants.pp: "AnsiString(vString) :=
# Source"). The slot must never be a managed AnsiString field: the Variant's
# generated hooks run unconditionally, and the raising-return convention
# destroys a Variant slot that was never initialized, which would free
# whatever bits were there. These helpers are the explicit hard casts.

proc ansiToPtr*(a: AnsiString): pointer =
  ## share `a` and hand out its char pointer with one extra reference; the
  ## caller owns that reference.
  result = cast[pointer](a.data)
  if not isNil(a):
    let p = a.rec
    if p[].refCnt > 0:
      discard atomicFetchAdd(p[].refCnt, 1'i32)

proc ptrToNimString*(p: uint64): string =
  ## the text of a varString slot as a *pure borrow*: no AnsiString reference
  ## is created, so there is nothing to release. Use this on the read path -
  ## a `ptrToAnsiString` temporary in an argument position is not destroyed
  ## by nimony, which would leak one reference per read.
  result = ""
  if p != 0:
    result = fromCString(cast[cstring](p))

proc ptrToAnsiString*(p: uint64): AnsiString =
  ## reinterpret a varString slot pointer as an AnsiString and take a
  ## reference; the caller owns (and destroys) the result. This is the
  ## `AnsiString(vString)` hard cast of Delphi/FPC. The slot travels as a
  ## uint64 so nimony's non-nil `pointer` parameter rule cannot reject it.
  result = default(AnsiString)
  if p != 0:
    result.data = cast[AnsiStringData](p)
    let r = result.rec
    if r[].refCnt > 0:
      discard atomicFetchAdd(r[].refCnt, 1'i32)

proc concat(a, b: AnsiString): AnsiStringData =
  result = cast[AnsiStringData](0)
  let la = a.rec.length
  let lb = b.rec.length
  result = newAnsiString(la + lb)
  copyMem(cast[pointer](result), cast[pointer](a.data), int(la))
  copyMem(cast[pointer](cast[uint64](result) + uint64(la)),
      cast[pointer](b.data), int(lb))

proc `&`*(a, b: AnsiString): AnsiString {.inline.} =
  result = default(AnsiString)
  if not isNil(a):
    if not isNil(b):
      result.data = concat(a, b)
    else:
      incRef(a)
      result.data = a.data
  elif not isNil(b):
    incRef(b)
    result.data = b.data

proc setLen*(dest: var AnsiString; newLen: int32) =
  uniqueStringOfLen(dest, newLen)

proc add*(dest: var AnsiString; b: AnsiString) =
  if not isNil(dest):
    if not isNil(b):
      let lena = dest.len
      uniqueStringOfLen(dest, lena + b.len)
      for i in 0'i32 ..< b.len:
        dest.data[int(lena + i)] = b.data[int(i)]
  elif not isNil(b):
    incRef(b)
    dest.data = b.data

proc cmp*(a, b: AnsiString): int =
  result = 0
  let m = if a.len < b.len: int(a.len) else: int(b.len)
  var i = 0
  while i < m:
    if a[i] != b[i]:
      return int(cast[uint8](a[i])) - int(cast[uint8](b[i]))
    inc i
  return int(a.len) - int(b.len)

proc `==`*(a, b: AnsiString): bool = cmp(a, b) == 0
proc `<`*(a, b: AnsiString): bool = cmp(a, b) < 0

proc weakSlice*(source: AnsiString; start: int32): WeakPartialString {.inline.} =
  result = default(WeakPartialString)
  let max = source.len
  if start >= 0'i32 and start < max:
    result.data = cast[AnsiStringData](addr source.data[int(start)])
    result.delta = start
    result.length = max - start

proc startsWith*(s: AnsiString; sub: AnsiString): bool =
  if sub.len > s.len: return false
  for i in 0'i32 ..< sub.len:
    if s[int(i)] != sub[int(i)]: return false
  return true

proc endsWith*(s: AnsiString; sub: AnsiString): bool =
  if sub.len > s.len: return false
  let m = s.len
  for i in 1'i32 .. sub.len:
    if s[int(m - i)] != sub[int(sub.len - i)]: return false
  return true

proc indexOf*(s: AnsiString; sub: AnsiString): int32 =
  result = -1'i32
  if sub.len == 0'i32: return 0'i32
  let n = s.len - sub.len
  for i in 0'i32 .. n:
    var k = 0'i32
    while k < sub.len and s[int(i + k)] == sub[int(k)]:
      inc k
    if k == sub.len: return i
  return -1'i32
