{.feature: "lenientnils".}
## Delphi AnsiString model for the nimony chain.
##
## Ported from /home/adrian/dev/nimbackend/delphi32.nim (Nim 1, 32-bit)
## to nimony/64-bit:
##   - getMemory/freeMemory/reallocMemory -> alloc0/dealloc/realloc
##   - system.atomicInc/atomicDec        -> std/atomics atomicFetchAdd/Sub
##   - ByteAddress                       -> uint
##   - {.bycopy.} dropped
##   - union helpers (SomeDelphiString, AnsiString|AnsiStringData) split
##     into concrete overloads: nimony does not resolve fields/methods
##     through an object union.
##   - the buffer is reached through the exported `data` field (a
##     `ptr UncheckedArray[char]`): `s[i]` is the read/write element
##     accessor and `s.data[i]` the un-detached raw read. nimony's own
##     string `readRawData`/`beginStore` API is not involved: AnsiString
##     carries its own \0, so `toCString` is a pure reinterpretation and
##     never an extra copy.
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
    data*: AnsiStringData
  WeakPartialString* {.pure, final.} = object
    data: AnsiStringData
    delta*: int32
    length*: int32
  ConstAnsiLit*[N: static[int]] = object
    ## Backing storage for a Delphi const string literal. The StrRec sits
    ## immediately before the data and carries refCnt = -1: never freed,
    ## never mutated in place (uniqueString detaches on the first write).
    rec*: StrRec
    buf*: array[N + 1, char]

# Byte-correctness guard. Delphi's StrRec is a *packed* record of two
# Longint -> 8 bytes, fields at 0 and 4, data at 8. `packed` is omitted
# because GCC warns -Waddress-of-packed-member on every refcount atomic (and
# the harness captured the warning as program output). That is safe only
# while the layout still matches, so fail the build if it ever does not.
when sizeof(StrRec) != 8:
  {.error: "StrRec must be 8 bytes to stay byte-compatible with Delphi".}

template needLength(len: int32): int32 =
  let length = len
  int32(length + int32(sizeof(StrRec)) + 1'i32 + ((length + 1'i32) and 1'i32))

proc newAnsiString(length: int32): AnsiStringData =
  result = cast[AnsiStringData](0)
  if length > 0:
    var p = cast[StrRecPtr](alloc0(int(needLength(length))))
    result = cast[AnsiStringData](cast[uint](p) + uint(sizeof(StrRec)))
    p.length = length
    p.refCnt = 1
    cast[ptr int16](cast[uint](result) +
        uint(length and not 1'i32))[] = 0'i16

template raw*(s: AnsiString): pointer = cast[pointer](s.data)

template rec*(s: AnsiStringData): StrRecPtr =
  cast[StrRecPtr](cast[uint](s) - uint(sizeof(StrRec)))

template rec*(s: AnsiString): StrRecPtr = s.data.rec

template rec*(s: WeakPartialString): StrRecPtr =
  cast[StrRecPtr](cast[uint](s.data) - uint(sizeof(StrRec)) -
      uint(s.delta))

template strData(r: StrRecPtr): AnsiStringData =
  cast[AnsiStringData](cast[uint](r) + uint(sizeof(StrRec)))

template isNil*(s: AnsiString): bool = cast[uint](s.data) == 0
template isNil*(s: WeakPartialString): bool = cast[uint](s.data) == 0

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
  ## Build the nimony string by the AnsiString's *counted* length, exactly
  ## like Delphi: an embedded \0 is data, not a terminator. Writes through
  ## nimony's documented bulk-write pair (beginStore/endStore); toCString is
  ## the mutating null-terminating accessor and must not be used here.
  result = ""
  let length = s.len
  if length > 0'i32:
    let dst = beginStore(result, int(length))
    copyMem(cast[pointer](dst), cast[pointer](s.data), int(length))
    endStore(result)

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
    if p[].refCnt > 0:
      discard atomicFetchAdd(p[].refCnt, 1'i32)

proc `=copy`*(dest: var AnsiString; source: AnsiString) =
  ## explicit assignment hook (used where the compiler does not choose
  ## the `=dup` path): same sharing, old value released.
  if dest.data == source.data: return
  var s = source.data
  if not isNil(source):
    var p = source.rec
    if p[].refCnt > 0:
      discard atomicFetchAdd(p[].refCnt, 1'i32)
  let d = dest.data
  dest.data = s
  if cast[uint](d) != 0:
    decRefD(d)

proc moveInto*(dest: var AnsiString; src: var AnsiString) =
  ## hand src's buffer to dest and nil src. Assigning to a `var AnsiString`
  ## parameter can run the parameter's destroy at proc exit, which would
  ## release the buffer the caller now holds; a true move avoids that.
  let d = dest.data
  dest.data = src.data
  src.data = nil
  if cast[uint](d) != 0:
    decRefD(d)

proc uniqueStringOfLen*(s: var AnsiString; wantedLen: int32) =
  let data = s.data
  let minLen = if wantedLen > s.len: s.len else: wantedLen
  if data == nil or data.rec.refCnt != 1:
    s.data = newAnsiString(wantedLen)
    if data != nil:
      for i in 0'i32 ..< minLen:
        s.data[int(i)] = data[int(i)]
      decRefD(data)
  else:
    let p = cast[StrRecPtr](realloc(cast[pointer](data.rec), int(needLength(wantedLen))))
    s.data = cast[AnsiStringData](cast[uint](p) + uint(sizeof(StrRec)))
    p.refCnt = 1
    p.length = wantedLen
    # Delphi zero-fills newly exposed bytes on growth; without this the
    # realloc path leaks stale bytes, unlike newAnsiString's fresh buffer.
    var z = minLen
    while z < wantedLen:
      s.data[int(z)] = '\0'
      inc z
    # truncation/growth on a live buffer: terminate at the new length. The
    # allocation-time `and not 1` double-NUL trick is for fresh buffers only;
    # here it would clobber the last byte when wantedLen is odd.
    s.data[int(wantedLen)] = '\0'

proc uniqueStringImpl(s: var AnsiString; r: StrRecPtr) =
  let wantedLen = r.length
  s.data = newAnsiString(wantedLen)
  copyMem(cast[pointer](s.data), cast[pointer](r.strData), int(wantedLen))
  if r.refCnt > 0:
    discard atomicFetchSub(r.refCnt, 1'i32)

template uniqueString*(s: var AnsiString) =
  if not s.isNil:
    let res = s.rec
    if res.refCnt != 1:
      # shared (>1) or a const literal (<0): detach before any mutation
      uniqueStringImpl(s, res)

func `[]`*(s: AnsiString; x: int): var char {.inline.} = s.data[x]
template `[]`*(s: AnsiString; x: int32): var char = s[int(x)]

proc `[]=`*(s: var AnsiString; x: int; c: char) {.inline.} =
  ## write access: detach first (copy-on-write), so a copy never observes it.
  uniqueString(s)
  s.data[x] = c

template `[]=`*(s: var AnsiString; x: int32; c: char) =
  `[]=`(s, int(x), c)

template `[]`*(s: WeakPartialString; x: int): char = s.data[x]
template `[]`*(s: WeakPartialString; x: int32): char = s.data[int(x)]

proc toAnsiString*(s: string): AnsiString =
  result = default(AnsiString)
  let length = int32(s.len)
  if length > 0:
    result.data = newAnsiString(length)
    copyMem(cast[pointer](result.data), cast[pointer](readRawData(s)),
        int(length))

proc toAnsiString*(s: cstring): AnsiString =
  result = default(AnsiString)
  let p = cast[ptr UncheckedArray[char]](s)
  var n = 0'i32
  while p[int(n)] != '\0':
    inc n
  if n > 0:
    result.data = newAnsiString(n)
    copyMem(cast[pointer](result.data), cast[pointer](p), int(n))

proc toAnsiString*(c: char): AnsiString =
  ## a single-character literal assigned to an AnsiString
  result = default(AnsiString)
  result.data = newAnsiString(1'i32)
  result.data[0] = c

template toAnsiStringLit*[N: static[int]](lit: ConstAnsiLit[N]): AnsiString =
  ## View a module-level ConstAnsiLit (rec.refCnt = -1) as an AnsiString.
  ## `lit` is substituted as a name, so unsafeAddr reaches the const's own
  ## storage rather than a copy.
  AnsiString(data: cast[AnsiStringData](unsafeAddr lit.buf[0]))

# --- raw access, COW-correct ------------------------------------------------
# An AnsiString is a copy-on-write buffer: reading may share, writing must
# own. `s[i]` is the element accessor (a `var char` place - reading it shares,
# assigning through it detaches via `[]=`); `s.data[i]` is the raw read used
# by the byte-layout tests; `pasOwnStr(t)[i]` is the detaching place the
# emitter hands to an untyped Pascal `var` parameter (`Move`/`FillChar`).
# toCString is only the NUL-terminated reinterpretation.

template toCString*(s: AnsiString): cstring =
  ## AnsiString is *always* NUL-terminated (unlike a nimony string), so this
  ## is a pure reinterpretation: no terminator is added, nothing mutates.
  cast[cstring](s.data)

proc pasOwnStr*(s: var AnsiString): var AnsiString =
  ## The place an untyped Pascal `var` parameter must receive: Delphi and FPC
  ## make an AnsiString element unique before handing out its address, so
  ## `pasOwnStr(t)[i]` is a writable element whose buffer `t` owns.
  ## Taking `t[i]` directly would write through a shared or literal buffer.
  uniqueString(s)
  result = s

proc beginStore*(s: var AnsiString; newLen: int32; start = 0'i32):
    ptr UncheckedArray[char] =
  ## write access: detach (uniqueStringOfLen) before handing out a mutable
  ## pointer, so a copy of `s` can never observe the write.
  uniqueStringOfLen(s, newLen)
  result = cast[ptr UncheckedArray[char]](cast[uint](s.data) + uint(start))

template endStore*(s: var AnsiString) =
  ## AnsiString has no cached prefix to sync; kept for API symmetry.
  discard

# --- varString slot bridge --------------------------------------------------
# Explicit hard casts between a raw char pointer and an AnsiString, for
# Pascal source that does that itself (`AnsiString(P)`, `PAnsiString` data).
# The Variant no longer needs them: its varString payload is text (see
# systempas's Variant), not the raw pointer slot Delphi keeps in TVarData.
# A managed AnsiString field is still impossible in a case object - nimony
# 9e44454e miscompiles an assignment to a managed field of an object for a
# type with user hooks (crash + use-after-free). These helpers are the
# explicit casts for the cases that do need a pointer.

proc ansiToPtr*(a: AnsiString): pointer =
  ## share `a` and hand out its char pointer with one extra reference; the
  ## caller owns that reference.
  result = cast[pointer](a.data)
  if not isNil(a):
    let p = a.rec
    if p[].refCnt > 0:
      discard atomicFetchAdd(p[].refCnt, 1'i32)

proc ptrToNimString*(p: uint): string =
  ## the text of a varString slot as a *pure borrow*: no AnsiString reference
  ## is created, so there is nothing to release. Use this on the read path -
  ## a `ptrToAnsiString` temporary in an argument position is not destroyed
  ## by nimony, which would leak one reference per read.
  result = ""
  if p != 0:
    let data = cast[AnsiStringData](p)
    let length = data.rec.length
    if length > 0'i32:
      let dst = beginStore(result, int(length))
      copyMem(cast[pointer](dst), cast[pointer](p), int(length))
      endStore(result)

proc ptrToAnsiString*(p: uint): AnsiString =
  ## reinterpret a varString slot pointer as an AnsiString and take a
  ## reference; the caller owns (and destroys) the result. This is the
  ## `AnsiString(vString)` hard cast of Delphi/FPC. The slot travels as a
  ## uint so nimony's non-nil `pointer` parameter rule cannot reject it.
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
  copyMem(cast[pointer](cast[uint](result) + uint(la)),
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

# --- Pascal `+` on an explicit AnsiString ---------------------------------
# Mixed representation cases convert explicitly through toString/toAnsiString.
# The char cases build the buffer directly: passing a call that returns an
# AnsiString straight as an argument to `&` destroys an uninitialized
# raising-return slot in nimony, and a char needs no string round-trip anyway.
proc `+`*(a, b: AnsiString): AnsiString = a & b

proc `+`*(a: AnsiString; b: string): AnsiString =
  result = default(AnsiString)
  var t = toAnsiString(b)
  result = a & t

proc `+`*(a: string; b: AnsiString): AnsiString =
  result = default(AnsiString)
  var t = toAnsiString(a)
  result = t & b

proc `+`*(a: AnsiString; b: char): AnsiString =
  result = default(AnsiString)
  let la = a.len
  result.data = newAnsiString(la + 1'i32)
  if la > 0:
    copyMem(cast[pointer](result.data), cast[pointer](a.data), int(la))
  result.data[int(la)] = b

proc `+`*(b: char; a: AnsiString): AnsiString =
  result = default(AnsiString)
  let la = a.len
  result.data = newAnsiString(la + 1'i32)
  result.data[0] = b
  if la > 0:
    copyMem(cast[pointer](cast[uint](result.data) + 1'u),
        cast[pointer](a.data), int(la))

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
# the other representation: a Pascal comparison may mix an AnsiString with
# a plain string value (a literal is a system string here)
proc `==`*(a: AnsiString; b: string): bool = toString(a) == b
proc `==`*(a: string; b: AnsiString): bool = a == toString(b)
proc `<`*(a: AnsiString; b: string): bool = toString(a) < b
proc `<`*(a: string; b: AnsiString): bool = a < toString(b)
proc `<=`*(a, b: AnsiString): bool = cmp(a, b) <= 0
proc `<=`*(a: AnsiString; b: string): bool = toString(a) <= b
proc `<=`*(a: string; b: AnsiString): bool = a <= toString(b)

proc substr*(s: AnsiString; first, last: int): AnsiString =
  ## nimony's substr spelling for an explicit AnsiString; Pascal Copy(s,a,b)
  ## lowers to substr(s, a-1, a-1+(b-1)), so the bounds are already 0-based.
  result = default(AnsiString)
  if not isNil(s):
    let n = int(s.len)
    let lo = if first < 0: 0 else: first
    let hi = if last >= n: n - 1 else: last
    if lo < n and lo <= hi:
      let cnt = hi - lo + 1
      result.data = newAnsiString(int32(cnt))
      copyMem(cast[pointer](result.data),
          cast[pointer](cast[uint](s.data) + uint(lo)), cnt)

proc substr*(s: AnsiString; first: int): AnsiString =
  result = default(AnsiString)
  if not isNil(s):
    result = substr(s, first, int(s.len) - 1)

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
