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

if fails == 0:
  echo("ansi-shim: ALL OK")
else:
  echo("ansi-shim: " & $fails & " FAILED")
