import std/syncio
import pasansistring

# Shim-level AnsiString conformance test.
#
# This leg checks the runtime model directly, because the Pascal front end
# still maps `ansistring` onto nimony's `string`; the mapping split lands
# only after this test is green. Observable string behaviour is what the
# FPC/Delphi legs of ansi-oracle.sh pin; the refcount/literal details here
# are the model's contract with the emitter.
#
# Measured references: Delphi 2007/Win32 and FPC 3.2.2 both give the string
# values below (see .dsh/wiki/pas2nimony-ansistring-semantics.md).

var fails = 0

proc chk(name: string; got, want: string) =
  if got == want:
    echo("ok   " & name & " = " & got)
  else:
    echo("FAIL " & name & " got=" & got & " want=" & want)
    fails = fails + 1

proc cs(c: char): string =
  # nimony has no `$` for char
  result = newString(1)
  result[0] = c

proc bytes(s: AnsiString): string =
  # render the raw bytes so a trailing NUL is visible
  result = ""
  if not isNil(s):
    var i = 0'i32
    while i < s.len:
      result.add($int(cast[uint8](s[i])))
      result.add(" ")
      inc i

# --- construction / nil / length -------------------------------------------
chk("nil isNil", $(isNil(toAnsiString("")) == true), "true")
chk("nil len", $toAnsiString("").len, "0")
chk("nil high", $toAnsiString("").high, "-1")
chk("abc len", $toAnsiString("abc").len, "3")
chk("abc high", $toAnsiString("abc").high, "2")
chk("abc low", $toAnsiString("abc").low, "0")
chk("abc[0]", cs(toAnsiString("abc")[0]), "a")
chk("abc[2]", cs(toAnsiString("abc")[2]), "c")
chk("abc bytes", bytes(toAnsiString("abc")), "97 98 99 ")
chk("roundtrip", toString(toAnsiString("hello")), "hello")
chk("roundtrip empty", toString(toAnsiString("")), "")
# Delphi counts bytes: an embedded NUL is data, not a terminator. A
# cstring/fromCString round-trip silently truncates here.
block:
  let z = toAnsiString("ab\0cd")
  chk("embedded nul len", $z.len, "5")
  chk("embedded nul bytes", bytes(z), "97 98 0 99 100 ")
  chk("embedded nul toString len", $toString(z).len, "5")

# --- Delphi byte layout -----------------------------------------------------
# StrRec sits immediately before the data pointer: refCnt at -8, length at
# -4 (little-endian int32) - exactly Delphi's packed record. The compile-time
# `when sizeof(StrRec) != 8: {.error.}` guard caps the size; this reads the
# raw bytes, because old 32-bit Delphi code can depend on these offsets.
block:
  let s = toAnsiString("hello")
  let base = cast[uint](raw(s))
  chk("layout refCnt@-8", $cast[ptr int32](base - 8)[], "1")
  chk("layout length@-4", $cast[ptr int32](base - 4)[], "5")

# --- refcount / copy on write ----------------------------------------------
block:
  var a = toAnsiString("hello")
  chk("fresh refCount", $refCount(a), "1")
  var b = a
  chk("after dup a refCount", $refCount(a), "2")
  chk("after dup b refCount", $refCount(b), "2")
  b.setLen(3'i32)
  chk("cow a value", toString(a), "hello")
  chk("cow b value", toString(b), "hel")
  chk("cow a detached", $refCount(a), "1")

# --- setLen grow / shrink ---------------------------------------------------
block:
  var s = toAnsiString("abc")
  s.setLen(5'i32)
  chk("grow bytes", bytes(s), "97 98 99 0 0 ")
  s.setLen(2'i32)
  chk("shrink value", toString(s), "ab")
  var e = toAnsiString("")
  e.setLen(3'i32)
  chk("nil grow bytes", bytes(e), "0 0 0 ")

# --- append / concat --------------------------------------------------------
block:
  var s = toAnsiString("ab")
  s.add(toAnsiString("cd"))
  chk("add", toString(s), "abcd")
  chk("concat", toString(toAnsiString("ab") & toAnsiString("cd")), "abcd")
  chk("concat nil left", toString(toAnsiString("") & toAnsiString("cd")), "cd")
  chk("concat nil right", toString(toAnsiString("ab") & toAnsiString("")), "ab")
  chk("concat both nil", toString(toAnsiString("") & toAnsiString("")), "")

# --- comparison -------------------------------------------------------------
chk("eq", $(toAnsiString("abc") == toAnsiString("abc")), "true")
chk("ne", $(toAnsiString("abc") == toAnsiString("abd")), "false")
chk("lt", $(toAnsiString("abc") < toAnsiString("abd")), "true")
chk("prefix lt", $(toAnsiString("ab") < toAnsiString("abc")), "true")
chk("cmp eq", $cmp(toAnsiString("abc"), toAnsiString("abc")), "0")
chk("cmp prefix sign", $(cmp(toAnsiString("ab"), toAnsiString("abc")) < 0), "true")

# --- search -----------------------------------------------------------------
chk("indexOf word",
    $toAnsiString("hello world").indexOf(toAnsiString("world")), "6")
chk("indexOf miss",
    $toAnsiString("hello").indexOf(toAnsiString("xyz")), "-1")
chk("startsWith", $(toAnsiString("hello").startsWith(toAnsiString("he"))), "true")
chk("startsWith no", $(toAnsiString("hello").startsWith(toAnsiString("lo"))), "false")
chk("endsWith", $(toAnsiString("hello").endsWith(toAnsiString("lo"))), "true")
chk("endsWith no", $(toAnsiString("hello").endsWith(toAnsiString("he"))), "false")

# --- raw buffer / Move source ----------------------------------------------
# The primitive Pascal's Move(Source, Dest, Count) needs: an AnsiString is
# already a C string, and the raw pointer is the address of its first char.
block:
  var s = toAnsiString("hello")
  let p = cast[ptr UncheckedArray[char]](raw(s))
  chk("raw first", cs(p[0]), "h")
  chk("raw as cstring", fromCString(cast[cstring](raw(s))), "hello")

# --- weak slice (PartialString view) ---------------------------------------
block:
  var s = toAnsiString("hello world")
  let wk = weakSlice(s, 6'i32)
  chk("weakSlice len", $wk.len, "5")
  var acc = ""
  var i = 0'i32
  while i < wk.len:
    acc.add(cs(wk[i]))
    inc i
  chk("weakSlice value", acc, "world")

# --- string <-> AnsiString: correct conversion form ------------------------
# Conversions go through the counted byte copy, never toCString. Raw byte
# reads use the exported `data` field (`s.data[i]`, no detach); the model
# itself no longer exposes a nimony-style `readRawData`.
block:
  let src = "hello"
  let a = toAnsiString(src)
  chk("string->ansi value", toString(a), "hello")
  chk("string->ansi src value", src, "hello")
  chk("string->ansi src len", $src.len, "5")
  var b = toAnsiString("shared")
  discard toString(b)
  chk("toString refCount stable", $refCount(b), "1")

# --- Pascal NUL padding (a nimony string has none) --------------------------
# Delphi terminates at data[len] and, because the allocation is rounded even,
# leaves a second zero byte at data[len+1] for even lengths.
block:
  let odd = toAnsiString("abc")            # len 3
  chk("odd terminator", $int(cast[uint8](odd.data[3])), "0")
  let even = toAnsiString("ab")            # len 2
  chk("even terminator", $int(cast[uint8](even.data[2])), "0")
  chk("even second zero", $int(cast[uint8](even.data[3])), "0")
  chk("empty is nil", $(isNil(toAnsiString("")) == true), "true")

# --- no duplicate NUL when string -> AnsiString -----------------------------
block:
  let a = toAnsiString("ab")
  chk("no dup nul len", $a.len, "2")
  chk("no dup nul value", toString(a), "ab")
  chk("no dup nul bytes", bytes(a), "97 98 ")
  let z = toAnsiString("ab\0")             # an explicit NUL is data
  chk("explicit nul len", $z.len, "3")
  chk("explicit nul bytes", bytes(z), "97 98 0 ")

# --- AnsiString.toCString is a no-op reinterpretation -----------------------
block:
  var s = toAnsiString("hello")
  let cp = toCString(s)
  chk("toCString is raw", $(cast[uint](cp) == cast[uint](raw(s))), "true")
  chk("toCString refCount stable", $refCount(s), "1")
  chk("toCString first", cs(cast[ptr UncheckedArray[char]](cp)[0]), "h")
  chk("toCString terminator",
      $int(cast[uint8](cast[ptr UncheckedArray[char]](cp)[5])), "0")
  chk("toCString roundtrip", fromCString(cp), "hello")

# --- read access does not detach -------------------------------------------
block:
  var a = toAnsiString("hello")
  var b = a
  chk("read share refCount", $refCount(a), "2")
  chk("read p0", cs(a.data[0]), "h")
  chk("read p4", cs(a.data[4]), "o")
  chk("read no unique a", $refCount(a), "2")
  chk("read no unique b", $refCount(b), "2")

# --- write access detaches: a copy can never observe it --------------------
block:
  var a = toAnsiString("hello")
  var b = a
  let w = beginStore(b, 5'i32)
  w[0] = 'H'
  endStore(b)
  chk("write copy mutated", toString(b), "Hello")
  chk("write original intact", toString(a), "hello")
  chk("write original detached", $refCount(a), "1")
  chk("write copy unique", $refCount(b), "1")
block:
  var a = toAnsiString("ab")
  let w = beginStore(a, 4'i32)
  w[2] = 'c'
  w[3] = 'd'
  endStore(a)
  chk("beginStore grow", toString(a), "abcd")
  chk("beginStore grow terminator",
      $int(cast[uint8](a.data[4])), "0")

# --- const literal: refCnt = -1 (Delphi typed const) -----------------------
const pasLit_foo = ConstAnsiLit[7](
  rec: StrRec(refCnt: -1'i32, length: 7'i32),
  buf: ['f', 'o', 'o', ' ', 'b', 'a', 'r', '\0'])
block:
  let cl = toAnsiStringLit(pasLit_foo)
  chk("lit refCount", $refCount(cl), "-1")
  chk("lit len", $cl.len, "7")
  chk("lit value", toString(cl), "foo bar")
  chk("lit terminator", $int(cast[uint8](cl.data[7])), "0")
  chk("lit toCString", fromCString(toCString(cl)), "foo bar")
  var m = cl
  chk("lit shared refCount", $refCount(m), "-1")
  let w = beginStore(m, 3'i32)
  w[0] = 'A'
  w[1] = 'B'
  w[2] = 'C'
  endStore(m)
  chk("lit copy mutated", toString(m), "ABC")
  chk("lit unchanged", toString(cl), "foo bar")
  chk("lit refCount after mutate", $refCount(cl), "-1")

if fails == 0:
  echo("ansi-shim: ALL OK")
else:
  echo("ansi-shim: " & $fails & " FAILED")
