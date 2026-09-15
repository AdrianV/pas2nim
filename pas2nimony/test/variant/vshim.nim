import std/syncio
import systempas
import pasansistring

# Shim-level Variant conformance test.
#
# Every expected value here was *measured* on this box (see
# .dsh/wiki/pas2nimony-variant-semantics.md):
#   dcc32 = Delphi 2007/Win32 under wine, fpc = FPC 3.2.2 x86-64.
# This leg checks the shim directly, because the Pascal-side probes
# (vcore.pas, vops2.pas) additionally need the emitter to insert the
# explicit Variant constructions that nimony's missing converter support
# makes mandatory.
#
# Construction: `toVariant(x)` for a typed value, `pasVarLit(x)` for a
# Pascal integer literal (what the emitter will emit). The raising
# operations (`+ - * / div mod`, `[]`, `VarAsType`) are `{.raises.}`, so
# they are exercised inside a try block - the same constraint nimony puts
# on any call site.

var fails = 0

proc chk(name: string; got, want: string) =
  if got == want:
    echo("ok   " & name & " = " & got)
  else:
    echo("FAIL " & name & " got=" & got & " want=" & want)
    fails = fails + 1

proc code(v: Variant): string = $VarType(v)
proc text(v: Variant): string = VarToStr(v)

proc summarize() =
  if fails == 0:
    echo("variant-shim: ALL OK")
  else:
    echo("variant-shim: " & $fails & " FAILED")

# --- typed values: identical in dcc32 and FPC ------------------------------
var i32: int32 = 70000
var i16: int16 = -300
var i8: int8 = -5
var u8: uint8 = 200
var u16: uint16 = 60000
var u32: uint32 = 4000000000'u32
var i64: int64 = -1234567890123'i64
var f64: float64 = 1.5
var f32: float32 = 2.5
var s1: string = "ansi"
var b1: bool = true
var c1: char = 'Z'

chk("typed int32", code(toVariant(i32)), "3")
chk("typed int16", code(toVariant(i16)), "2")
chk("typed int8", code(toVariant(i8)), "16")
chk("typed uint8", code(toVariant(u8)), "17")
chk("typed uint16", code(toVariant(u16)), "18")
chk("typed uint32", code(toVariant(u32)), "19")
chk("typed int64", code(toVariant(i64)), "20")
chk("typed float64", code(toVariant(f64)), "5")
chk("typed float32", code(toVariant(f32)), "5")
chk("typed string", code(toVariant(s1)), "256")
chk("typed bool", code(toVariant(b1)), "11")
chk("typed char", code(toVariant(c1)), "256")

# --- literal typing: dcc32 picks Byte/Word/LongWord -------------------------
chk("lit 3", code(pasVarLit(3)), "17")
chk("lit -3", code(pasVarLit(-3)), "16")
chk("lit 300", code(pasVarLit(300)), "18")
chk("lit 32767", code(pasVarLit(32767)), "18")
chk("lit 32768", code(pasVarLit(32768)), "18")
chk("lit 70000", code(pasVarLit(70000)), "19")
chk("lit -70000", code(pasVarLit(-70000)), "3")

# --- the union views: an unsigned value reads back signed ------------------
var u: Variant = toVariant(u32)
chk("uint32 VInteger", $u.VInteger, "-294967296")
chk("uint32 VLongWord", $u.VLongWord, "4000000000")
# measured in dcc32 AND FPC: VarToStr dispatches on the tag, so a
# varLongWord renders unsigned while VInteger reads back signed
chk("uint32 text", text(u), "4000000000")

# --- Null / Unassigned / Clear (identical in both oracles) -----------------
chk("Null isnull", $VarIsNull(Null), "true")
chk("Null isempty", $VarIsEmpty(Null), "false")
chk("Null isclear", $VarIsClear(Null), "false")
chk("Unassigned isempty", $VarIsEmpty(Unassigned), "true")
chk("Unassigned isclear", $VarIsClear(Unassigned), "true")
chk("EmptyParam code", code(EmptyParam), "10")

# --- conversions (identical in both oracles) -------------------------------
var v: Variant = toVariant(i32)
chk("VarToStr int", text(v), "70000")
v = toVariant(i64)
chk("VarToStr int64", text(v), "-1234567890123")
v = toVariant(b1)
chk("VarToStr bool", text(v), "True")
v = toVariant(s1)
chk("VarToStr string", text(v), "ansi")
v = Null
chk("VarToStr null", "[" & text(v) & "]", "[]")
v = Unassigned
chk("VarToStr unassigned", "[" & text(v) & "]", "[]")

# --- varString holds a real AnsiString (FPC/Delphi pointer slot) -----------
# The slot is a raw pointer, hard-cast to an AnsiString only here - it is not
# a managed AnsiString field (that crashed on the raising-return path, because
# the generated hook freed whatever bits were in the union).
proc ansiChecks() =
  let hello = toAnsiString("hello")
  let vh = toVariant(hello)
  chk("ansi var type", $VarType(vh), "256")
  chk("ansi var str", VarToStr(vh), "hello")
  chk("ansi slot text",
      ptrToNimString(cast[uint64](vh.VAnsiString)), "hello")
  chk("ansi slot refcount", $refCount(hello), "2")
  var s: string = "plain"
  let vs = toVariant(s)
  chk("plain var type", $VarType(vs), "256")
  chk("plain var str", VarToStr(vs), "plain")
  let ve = toVariant("")
  chk("empty var type", $VarType(ve), "256")
  chk("empty var text", VarToStr(ve), "")
  let vc = toVariant('x')
  chk("char var type", $VarType(vc), "256")
  chk("char var str", VarToStr(vc), "x")

# --- the raising half ------------------------------------------------------
proc raisingChecks() {.raises.} =
  var f19: float64 = 1.9
  var f25: float64 = 2.5
  chk("VarAsType 1.9->int",
      text(VarAsType(toVariant(f19), varInteger)), "2")
  chk("VarAsType 2.5->int (half to even)",
      text(VarAsType(toVariant(f25), varInteger)), "2")
  var s42: string = "42"
  chk("VarAsType '42'->int", text(VarAsType(toVariant(s42), varInteger)), "42")

  var a: Variant = pasVarLit(1)
  var w: Variant = pasVarLit(2)
  var sum = a + w
  chk("int+int code", code(sum), "3")
  chk("int+int text", text(sum), "3")
  var l: Variant = toVariant(i64)
  var s2 = l + w
  chk("int64+int code", code(s2), "20")
  chk("int64+int text", text(s2), "-1234567890121")
  var sa: Variant = toVariant("a")
  var sb: Variant = toVariant("b")
  var cat = sa + sb
  chk("str+str code", code(cat), "256")
  chk("str+str text", text(cat), "ab")
  var q = a / w
  chk("int/int code", code(q), "5")
  chk("int/int text", text(q), "0.5")
  var d = pasVarLit(5) div w
  chk("int div int code", code(d), "3")
  chk("int div int text", text(d), "2")
  var nl: Variant = Null
  var nn = nl + a
  chk("Null+int code", code(nn), "1")

  # measured promotion: LongWord forces varInt64 (varLongWord + varByte ->
  # 0014), Currency with an integer stays varCurrency (-> 0006)
  var lw: Variant = toVariant(70000'u32)
  var sum2 = lw + a
  chk("longword+int code", code(sum2), "20")
  chk("longword+int text", text(sum2), "70001")
  var cuv: Variant = pasVarCurr(25000'i64)
  chk("currency code", code(cuv), "6")
  chk("currency text", text(cuv), "2.5")
  var csum = cuv + a
  chk("currency+int code", code(csum), "6")
  chk("currency VInt64", $csum.VInt64, "35000")
  chk("Null=Null", $(nl == Null), "true")
  chk("Null=Unassigned", $(nl == Unassigned), "false")

  var arr = VarArrayCreate([0'i32, 2'i32], varInteger)
  chk("array isarray", $VarIsArray(arr), "true")
  chk("array dimcount", $VarArrayDimCount(arr), "1")
  chk("array lowbound", $VarArrayLowBound(arr, 1), "0")
  chk("array highbound", $VarArrayHighBound(arr, 1), "2")
  arr[1] = pasVarLit(7)
  chk("array element", text(arr[1]), "7")
  var a2 = VarArrayOf([pasVarLit(1), pasVarLit(2), pasVarLit(3)])
  chk("VarArrayOf highbound", $VarArrayHighBound(a2, 1), "2")
  chk("VarArrayOf element 2", text(a2[2]), "3")

ansiChecks()

var raised = false
try:
  raisingChecks()
except:
  raised = true

if raised:
  chk("no unexpected raise", "raised", "none")
summarize()
