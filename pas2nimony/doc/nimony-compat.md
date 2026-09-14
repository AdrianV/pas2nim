# nimony compatibility

What the modernized pas2nimony translator emits, what works, and what is
deliberately out of scope for stage 1. This document is the contract the
emitted code has with the nimony (Nim 3) compiler.

Related: `doc/toolchain.md` (how the tool itself is built with nimony),
`../README.md` (original pas2nim, frozen reference).

## Pipeline position

```
foo.pas ──pas2nimony──▶ foo.nim ──▶ nifler ──▶ nimsem ──▶ hexer ──▶ lengc ──▶ cc
                         (single module; stage 2 replaces the nifler
                          step with a native .pas front end)
```

Stage 1 emits Nim source; stage 2 (pasler) will plug in as a language
front end that feeds `.p.nif` directly into nimsem, bypassing the Nim
parser entirely.

**M2 step 1 is proven** (`test/pasler-poc.sh`, PASS): a `.pas` file goes
through the chain with `nimony s` — the NIF-input mode — and the binary
runs, with no `.nim` file involved in the build. The mechanics:

- `nimony s <file.p.nif>` consumes parsed NIF directly
  (`deps.nim:toPair` returns the file unchanged for `.nif` inputs);
- the driver needs `<mod>.p.deps.nif` next to it, mirroring the import
  statements in plain (unhashed) module names — nifler `--deps` writes
  it, or pasler will;
- module suffixes (e.g. `sysol8l981`) are path-dependent hashes: keep
  the translated module's imports resolvable from one location and pass
  `--path:.` plus `--nimcache:` explicitly;
- the `.p.nif` is the single source of truth — hand edits (strings,
  vendor tag, even the claimed source filename) flow through untouched.

Phase 2 therefore only swaps step "nifler" for a direct NIF writer
built on `paspars.nim`'s AST; the driver becomes
`bin/pasler c foo.pas`.

### M1 status: complete

All five M1 items verified by `test/twounit/` (counter unit + program):

1. unit → own `.nim` (unit/program headers become comments).
2. `uses Foo` of own units → `import <file-stem>`; the unit's interface
   is parsed first (`absorbUnit`) so ctor rewrites, member-call
   wrapping and canonical spellings work cross-module. Unit files are
   found case-insensitively (`counter.pas` vs unit `Counter`).
3. `initialization` sections become module-level statements — imported
   modules run first, so unit init order is import order, correct by
   construction.
4. program file translates as the main module importing the units.
5. `sysstrs.nim` renamed to `systempas.nim` (proto-systempas unit).

Supporting fixes: bare `end.` unit terminators parse; interface
declarations get export markers (`*`) via a boolean node flag (NOT a
PostfixExport wrapper — that broke every `kind == nkIdent` walk);
implementation decls inherit the export so they don't shadow it;
Pascal function-values without parens (`obj.Value`) are wrapped into
calls; zero-arg ctors as values (`c := T.Create`) rewrite; pure-literal
Integer arithmetic is emitted as `int32(21 * 2)`.

### Text .p.nif vs binary .bif (investigated, decision recorded)

`src/lib/bif.nim` (nimony) is a complete binary TokenBuf codec — the
in-memory token array + pools + embedded index, 0.20x the text size,
loaded with one readBuffer instead of a tokenizer/parser pass. It is
the intended future cache format ("for a compiler cache the text is
pure overhead").

Adoption in the current pipeline:

| Stage | Reads | Writes | binary-capable today |
|---|---|---|---|
| nifler | .nim | .p.nif + .p.deps.nif | no |
| nimsem | .p.nif, imported .s.nif | .s.nif, .s.idx.nif | **no** (`programs.nim` text-parse) |
| hexer | .s.nif | .c.nif | no |
| lengc | .c.nif | .o/.c | **yes** — header sniffed (`lengc/nifmodules.nim`) |
| import-c modules | .nif-named | — | **yes** — header sniffed (`lib/foreignmodules.nim`) |
| macro plugins | .in.nif | .bif-capable | yes (`load`/`storeToString`) |

Consequences for pasler:

1. **Emit into a `TokenBuf`, not into a string.** The upstream pattern
   (`macro_plugin.buildPluginNif`) builds a `TokenBuf` programmatically
   and renders with `writeFileAndIndex(path, buf)`. Both formats
   serialize from the same buffer: text is `writeFileAndIndex`, binary
   is `bif.store(buf, path)`. Build the writer around TokenBuf
   emission from day one and the format switch stays a one-liner.
2. **Ship text `.p.nif` first.** nimsem cannot read a binary `.p`
   today — its readers (`programs.nim:loadModule`,
   `semos.parseFile`) parse text without sniffing. Text also keeps the
   artifacts diffable and the deps file format unchanged.
3. **The perf win is real but lives downstream.** Pasler's own `.p.nif`
   is written once and read once per build — small. The measurable win
   is nimsem re-parsing imported `.s.nif` *text* (stdlib!) on every
   build, and the ~5x cache disk shrink. That needs the bif-sniffing
   pattern (already proven in `lengc/nifmodules.nim` /
   `foreignmodules.nim`, ~10 lines per reader) ported to
   `programs.nim`/`semos.parseFile` — an upstream nimony change.
4. **Later switch**: once upstream sniffs, pasler grows `--bif` (one
   call: `bif.store` instead of `writeFileAndIndex`). Nothing in the
   writer design blocks it.

## Construct compatibility matrix

Legend: ✅ fully supported · 🟡 supported with documented, lossy
semantics · ❌ rejected with a clear error (phase-2 lowerings)

### Types

| Pascal | Emitted nimony | Notes |
|---|---|---|
| `Integer/Longint` | `int32` | Delphi sizes; full map in `passym.RtlNames` |
| `Cardinal/Longword` | `uint32` | |
| `Int64` | `int64` | |
| `Single/Double/Real/Extended` | `float32`/`float64` | `Extended` narrows to `float64` |
| `Boolean` | `bool` | |
| `Char/AnsiChar/WideChar` | `char` | |
| `String/AnsiString/WideString` | `string` | indexed **1-based** (Delphi); the translator shifts string index expressions by one |
| `PChar` | `cstring` | |
| `Pointer` | `pointer` | |
| `TextFile/Text` | `File` | |
| `TObject` | `RootRef` | |
| `array[0..N] of T` | `array[0..N, T]` | explicit initializers emitted |
| `array[lo..hi] of T` (non-zero lo) | same | lo preserved; *nimony 0.4.x limitation*: runtime indices on non-zero-low arrays trip an openArray bounds check (constant indices are fine) |
| `array of T` (dynamic, type decl) | `seq[T]` | |
| `array of const` (param) | `TArrayOfConst` | shim type in `runtime/sysstrs.nim` |
| `set of T` | `set[T]` | `in`/`include`/`exclude` map to `in`/`incl`/`excl`; set literals become `{}` |
| `record` | `object` | value type, `default(T)` initializers |
| `class` | `ref object of RootRef` | `{.inheritable.}` always emitted |
| `class(T)` / inheritance | `ref object of T` | parent gets `{.inheritable.}` |
| `object` / `object(T)` | `object` / `object of T` | value objects support inheritance via `{.inheritable.}` |
| `procedure of object` | record `{evProc: proc …; evObj: RootRef}` | bound method pointer (M4-3): `x.ev := X.H` lowers to two field asgns plus a module-level thunk `pasThunkN(self, …)` casting the bound instance back; `ev(args)` → `ev.evProc(ev.evObj, args)`; `Assigned(ev)` / `ev = nil` compare the `evProc` field. Works through fields, properties (mapped to the backing field), local vars and params (lowered as `var` so the body can rebind them; seed them by passing a method-pointer variable — plain record copy). Limits: passing a method *name* as a call-site argument is not marshaled yet; `function of object` untested; nested `A.B.Handler` bases unhandled |
| class without a constructor | synthesized `proc create(self: T): T` | Delphi's inherited `TObject.Create`; only when no ancestor declares a constructor either |
| `class var` | module-level `var` | one global per class, name kept |
| enums | `enum` (keyword form) | |

### Classes, methods, properties

| Pascal | Emitted | Notes |
|---|---|---|
| `constructor T.Create(x)` | `proc create(self: T; x): T` | `result = self` prepended; call sites `X := T.Create(…)` → `X = T(create(T(), …))` (cast inserted when the ctor is inherited) |
| `virtual` / `override` | `method` | interface `virtual`/`override` markers decide; impl-only methods stay `proc` — signature matches the forward decl |
| `inherited` (bare) | `procCall doIt(Parent(self))` | callee left unqualified so dispatch hits the parent |
| `inherited foo(args)` | `procCall foo(Parent(self), args)` | |
| `procedure T.M` (dotted) | method/proc with explicit `self` | `{.this.}` is gone; every member access is self-qualified by a post-pass |
| nested routines in methods | plain `proc` + explicit `self` argument at call sites | they see `self` implicitly in Pascal |
| `property P: T read F write G` | `template P(self: T): T` / ``template `P =` `` | inlined through the field or setter |
| array property `P[i]: T` | direct calls to the accessors | `obj.P[i]` → `getP(obj, i)`; `obj.P[i] := v` → `setP(obj, i, v)` |
| default array property (`obj[i]`) | same, resolved via a var-type table | requires the variable's class to be known (single module) |
| `is` / `as` | `of` / cast | `as` becomes an unchecked downcast |
| `Free` | `Free*(self: RootRef)` shim | no-op; nimony owns lifetimes |

### Statements and expressions

| Pascal | Emitted | Notes |
|---|---|---|
| `:=` | `=` | |
| `=`, `<>` | `==`, `!=` | |
| `+` on strings/chars | `&` via shim overloads | string `+` preserved through `sysstrs` |
| `for i := a to b do` | `for _ in pforTo(i, a, b)` | shim iterator drives the declared variable by `var`; Pascal-exact semantics (see below) |
| `for i := a downto b do` | `for _ in pforDownto(i, a, b)` | same, counting down |
| `for x in c do` | `for x in c` | |
| `repeat … until c` | `while true: … if c: break` | |
| `case x of` | `case x of … else: discard` | ranges supported; implicit `else: discard` |
| `Exit` | `return` | bare `Exit` → `return` with empty value |
| `Result` | `result` | |
| `try … except on E: C do` | `try … except ErrorCode as <hidden>: case <hidden> of <map>: var E: C = cast[C](pasCurrentExc); …` | **instance-binding (M4-2)**: the Pascal variable binds the stashed instance (downcast via `cast[]`); the ErrorCode dispatch stays lossy — distinct classes mapping to the same code share handlers (see below) |
| `try … finally` | `try … finally` | native |
| `raise E.Create(msg)` | `pasCurrentExc = cast[PasException](create(...)); raise <ErrorCode-const>` | instance rides the `systempas.pasCurrentExc` slot; ErrorCode via `passym.excSpelling`; raising procs get `{.raises.}` |
| bare `raise;` (re-raise) | bare `raise` | native nimony handler re-raise; the instance slot keeps the current exception |
| uncaught exception (program level) | main block wrapped in `try/except` printing `Exception: <Message>` | v1: execution continues after the report (Delphi terminates) |
| `write/writeln` | `write(stdout, …)` / `echo(…)` | multi-arg `write` folds into one string with `&`/`$` |
| `s[i]` / `s[i] := c` (string index) | `s[i-1]` | **Delphi 1-based**; literal index 1 folds to 0; bases tracked: vars, params, class fields (incl. bare `self` fields in methods), record fields. Untagged bases (function results, chained `arr[i][j]` into strings, char-typed indices) stay 0-based — documented limitation |
| `Pos(sub, s)` | `find(s, sub) + 1` | exact Delphi semantics: 1-based, 0 when absent (nimony's `find` returns -1) |
| `Copy(s, a[, b])` | `substr(s, a-1[, a-1 + (b-1)])` | `substr` is inclusive 0-based and clamps out-of-range like Delphi's `Copy` |
| `Delete(s, i, n)` / `Insert(src, s, i)` | `strDelete` / `strInsert` | 1-based shims in `systempas` (nimony has no string delete/insert); out-of-range is clamped, not raised |
| `Inc/Dec` | `inc/dec` | |
| `class operator Add(a, b: T): T` etc. | nimony operator procs | **v1 (M4)**: arithmetic (`Add Subtract Multiply Divide IntDivide Modulus`), bitwise (`And Or Xor LeftShift RightShift`), the six comparisons, and unary (`Negative Positive LogicalNot BitwiseNot`); call sites stay plain infix/prefix expressions - nimony's overload resolution picks them; works on records (visibility sections added) and classes; Inc/Dec, Explicit/Implicit and class-qualified explicit calls rejected |
| generics - Delphi style | nimony generics | **v1 (M4)**: `TPair<K, V> = class` with inline instantiations `TPair<Integer, String>` in type positions and `TPair<Integer, String>.Create(...)` constructor calls; generic members lower to `proc GetKey[K, V](self: TPair[K, V]): K`; constraints (`T: class, constructor`) parsed and dropped |
| generics - FPC objfpc style | the same lowering | **v1 (M4)**: `generic TFoo<T> = class` declarations and `type TIntPair = specialize TPair<Integer, String>` aliases; aliases behave as first-class classes (ctor/method machinery resolves through the generic) |
| `uses` clauses - bridging | direct nimony imports | **v1 (M4)**: `uses nim.std.strutils` (or generally `nim.<package.path>.<mod>`) imports the nimony module on both paths; its API is used as-is with no Delphi fidelity promises; exported identifiers are case-insensitively matched via a crude source scan (macro-generated exports need the exact nimony spelling); nimony quirks at the seam: no char->string conversion (use nimony's API shapes), `join(seq)` defaults to "" not " " |
| `StrUtils`/`Math` units | shim units | **M3**: `uses StrUtils`/`uses Math` resolve to our passtrutils/pasmath shim units - the Delphi API layered over nimony's std plus clean re-implementations (RoundTo = banker's rounding, SimpleRoundTo = half-away, case-insensitive ops, SplitString); char overloads absorb nimony's missing char->string conversion for 1-char Pascal literals; LeftStr/RightStr/MidStr are length/1-based like Delphi |
| `TDateTime` | float64 alias + pure nimony core | **M3**: ported from p4n's DateTime.hx (the same DATE_DELTA=693594 encode/decode cascade, the rounding-guarded time decomposition, unix timestamps); TDateTime is a plain float64 alias so Delphi arithmetic works (`EncodeDate(...) + 1` = next day); EncodeDate/EncodeTime/EncodeDateTime/DecodeDate/DecodeTime/YearOf.../DayOfTheWeek/Now/Date/FromUnixTimestamp/ToUnixTimestamp/IsLeapYear/DaysInMonth/LastDayOfMonth/MonthDelta/EasterSunday/ISOWeekNumber/FormatDateTime(common tokens, English names); Now/Date are UTC (nimony's clock) and EasterSunday keeps the simplified p4n formula (diverges from the true date for some years) |
| `SysUtils` core | shim unit | **M3**: `uses SysUtils` -> systempas; IntToHex, StrToFloatDef/StrToFloat (manual parser, char overload for 1-char literals), TrimLeft/TrimRight, CompareStr, AnsiCompareStr/Text, AnsiUpper/LowerCase, FileExists, the path family (ExtractFilePath/FileDir/FileName/FileExt, ChangeFileExt, Include/ExcludeTrailingPathDelimiter, both / and \); **Format** with Delphi array-of-const (the parser lowers `[a, b, c]` to toVrec calls, the shim takes openArray[TVarRec]): index (0-based), '-', width, precision, d/u/s/x/X/e/f/g/G/n/m, %% - float rendering follows C printf via formatBiggestFloat (n/m = fixed 2, no thousands separator in v1) |
| `DateUtils` unit | shim unit | **M3**: the naming layer over the TDateTime core - DateOf/TimeOf, Today/Yesterday/Tomorrow, IncDay/Week/Hour/Minute/Second/MilliSecond (float arithmetic), IncMonth/IncYear (calendar-aware, day clamped to the target month), the Between family (whole units truncated; Months/YearsBetween approximate via 30.4375/365.25-day units), DayOfTheYear/WeekOfTheYear/MonthOfTheYear, Start/EndOfTheDay/Month/Year + Start/EndOfAMonth/AYear, IsSameDay, RecodeDate/RecodeTime, IsValidDate/Time/DateTime |
| `TStringList` (Classes) | shim ref object | **M3**: hand-written ref type - Create (typedesc proc, `TStringList.Create` resolves), Add (returns the index; Duplicates honored, default dupAccept), Insert/Delete/Clear, default property `List[i]` / `List[i] := v` via `[]`/`[]=` (also string-keyed: `List['key']` / `List['key'] := v` = Values), IndexOf (case-insensitive unless CaseSensitive), Sort (insertion, honors CaseSensitive), Text, ValueOf/NameOfIndex (call-syntax Values/Names), SaveToFile/LoadFromFile (roundtrip verified). v1 gaps: Sorted does not auto-sort on mutation; IO errors swallowed; no Objects; no Text setter; indexed property forms `List.Strings[i]`/`Names[i]`/`Values[k]` unavailable (nimony auto-calls zero-arg dots - indexing a property name yields a char); use `List[i]` or the call-syntax forms |
| SysUtils file ops | shim unit | **M3**: FileExists, DeleteFile (removeFile, try-wrapped), DirectoryExists (char overload for the '.' literal), CreateDir/ForceDirectories (nimony's createDir makes the whole chain) |
| `uses` clauses - Delphi units | shim units | **v1 (M4)**: `SysUtils`/`System`/`Si_Strings` map to the systempas shim, `StrUtils`/`Math` map to std modules; a family of Delphi-shaped shim units (small layers over nim modules + clean re-implementations) is the planned M3 direction |
| anonymous methods - types | plain proc types | **v1 (M4)**: `TProc = reference to procedure(X: Integer);` lowers to a nimony proc type; the closure state is nimony's anonymous-proc machinery, which captures outer variables by reference; method-pointer types (`of object`) keep their evProc/evObj lowering |
| anonymous methods - literals | nameless lambdas | **v1 (M4)**: `F := procedure(X: Integer) begin ... end;` lowers to a nimony lambda (block form when assigned, inline `(stmt1; stmt2)` form in call arguments; captures are by reference, Delphi-compatible); a property name and a module-level var of the same name collide in nimony's namespace |
| `class procedure/function`, `class var` | module-level procs/vars | **v1 (M4)**: static only — `pasCm_<Class>_<Name>` / `pasCv_<Class>_<Name>`; callable via the class name, an instance, or bare inside the class's routines; class vars zero-initialized (hoisted after the last type section, exported when public); no `class virtual`, no `Self` (class reference) inside class methods, `class const`/`class property` rejected |
| `IFoo = interface` | abstract ref class + `method` dispatch roots | **v1 (M4)**: methods only (properties rejected); `procedure` roots discard, `function` roots `result = default(T)` (nimony's result-init proof); `class(TInterfacedObject, IFoo)` dissolves TInterfacedObject and inherits the interface's generated class — implementors are `method` overrides via the implicit-virtual registry; interface inheritance `IDog = interface(ICounter)` works; **no refcounting** (object lifetime independent), no QueryInterface (`is`/`as` ride the object's class), multiple interfaces / real parent + interface rejected with an error |
| `x is T` / `x as T` | `(x != nil) and (x of T)` / `pasAs[T](x)` | **runtime-checked (M4)**: nimony's `of` answers true for nil, so `is` guards with `!= nil` (Delphi: `nil is T` = false); `as` rides the generic `systempas.pasAs[T]` — nil stays nil, a failed check yields **nil** instead of raising EInvalidCast (the raising variant would mark every transitive caller `{.raises.}` — nimony only allows such calls inside try); a failed cast therefore surfaces as a nil-deref later |
| `label L1, L2, 10;` + `goto L` | forward: `block pasGotoL: …` + `break pasGotoL`; backward: `while true: …` + `continue` | **structured rewrite (M4)**: the region between the goto and the label becomes the block/loop; forward gotos from nested loops and out of `try..finally` work (the finally runs, like Delphi); numeric labels supported; label scope is routine-local. v1 rejects: a label targeted both forward and backward, backward gotos crossing a loop boundary, jumping into a nested block, labels not inside a begin/end — all with precise errors |
| `with E1, E2 do` | hidden temps `var pasW<n>: T = E` + member qualification | **instance-aware (M4)**: bare idents in the body resolve against the with-classes (innermost first, Delphi shadowing — with-members beat locals and `self` fields); later expressions are evaluated in the scope of the earlier ones (`with S, FOrigin do` = `FOrigin` of `S`). v1 heads: class-typed vars/params, `self`, ctor calls, member chains of class-typed fields; value objects/records unsupported (the temp would copy — needs ptr lowering). Planned: Oxygene-style `with E as X do` naming the temp explicitly |
| `{$ifdef X}` / `{$ifndef X}` | `when defined(X)` / `when not defined(X)` | answered at parse time from the target's symbol set (`-d:` plus `{$define}`s) |
| `{$if <expr>}` | `when <expr>` | **forwarded, never evaluated**: the frontend cannot answer `declared(X)` or `sizeof(Pointer)`, so the condition becomes real Nim and nimony decides. Closed by `{$endif}` or the Delphi `{$ifend}`; `{$elseif}`/`{$else}` chains, labelled closers, and nesting all supported. Works in statement, type-section, `uses`-clause and `begin`-block position (nimony rejects `when` inside a `type` section, so the emitter splits the section around it and hoists the arms' `type` blocks). Consequence: both branches must be parseable Pascal - see the milestone below |
| `Name: T absolute Target` | `template Name: T = Target` | variable aliases only (no absolute addresses in the corpus). The template reads and writes through; the renderers hoist it out of the enclosing `var` block, because `template` may not sit inside one |
| `{@exclude}` and `{@...}` | skipped | documentation directives, not declarations. The lexer hands the opener and its `}` over as separate tokens, both consumed |

### Case preservation

The translator is case-insensitive on input (Pascal semantics) and
case-preserving on output: the first declared spelling of every
identifier wins module-wide (`SymTab` registry; RTL names take
precedence, e.g. `uppercase` → `UpperCase`). `total`, `TOTAL` and
`Total` all render as the declared spelling — verified by
`test/samples.pas`.

### Loop variable semantics (solved with a var-param iterator)

A Nim `for` always binds a fresh iteration variable, but iterators may
take `var` parameters. The shim (`runtime/sysstrs.nim`, the proto
`systempas` unit for phase 2) defines:

```nim
iterator pforTo*[T: Ordinal](v: var T, a, b: T): T {.inline.} =
  v = a
  while v <= b:
    yield v
    inc v
```

and the translator emits `for _ in pforTo(i, a, b): body` — the
iterator drives the *declared* Pascal variable through `v`, the for
binds a throwaway `_`. This keeps every Pascal semantic:

- the body reads/writes the declared variable itself;
- `break` leaves it at the broken-out value;
- nested routines capture the live loop value (impossible with a
  shadowing for-loop);
- after normal termination it holds `b+1` (`b-1` for `downto`) —
  Delphi's practical convention;
- the `var T` parameter pins the iterator's `T` to the declared type,
  so `pforTo(I, 1, 10)` on an `int32` I type-checks without bound
  casts (and sidesteps `countup`'s 64-bit default step entirely).

### String indexing gap (documented, stage 1)

Pascal strings are 1-based, nimony strings are 0-based. `s[1]`
translates to `s[1]`, which addresses the *second* character. Arrays
with explicit ranges (`array[0..4]`) are unaffected. Callers that rely
on 1-based string indexing need adjustment; phase 2 will emit `s[i-1]`
by typing the index expression.

## The ErrorCode exception model

nimony replaces Nim's exception objects with C-style error codes:

- the only raisable values are `ErrorCode` enum constants
  (`Failure`, `SyntaxError`, … — builtins, not defined by us);
- a routine that raises needs `{.raises.}` (bare; a type list is
  rejected for enum constants);
- calling a `{.raises.}` routine outside `try` is a compile error, and
  bare re-`raise` needs `{.raises.}` too;
- `except ErrorCode as e` catches, `case e of <const>` dispatches.

Delphi's rich exception objects cannot ride `raise` itself, so M4-2
adds an instance side-channel: `systempas.PasException` (Nim's system
module reserves the name `Exception`) carries `Message: string`, and a
module-level `pasCurrentExc` slot connects raise sites to handlers.
The ErrorCode `case` dispatch stays lossy: distinct classes mapping to
the same code share handlers, and a handler trusts its declared class
(the `cast[]` downcast is unchecked). `raise;` re-raise is native.
The prelude constructor is `pasExcCreate` — a user subclass's own
`create` must not shadow it in nimony's name resolution.

## Frontend architecture (stage 2: pasler)

The stage-1 translator is a conventional source-to-source tool; its
parts are the stage-2 front end in disguise:

- `src/paslex.nim` — Delphi lexer (keywords, directives, numbers,
  strings). For pasler this becomes the nifler replacement producing
  `.p.nif` token streams.
- `src/paspars.nim` — recursive-descent parser building this tool's
  own AST + symbol tables (case registry, class/member registry,
  var-type table). The AST is deliberately close to nifcore's node
  kinds so the rendering step can later be replaced by direct NIF
  emission.
- post-passes: `selfQualifyAll` (explicit `self.`), `rewriteCtorCalls`
  (`T.Create` → `T(create(T(), …))`), `rewriteClassAsgns` (upcast
  assignments), `rewriteArrayProps` (array/default properties),
  `genPropertyAccessors` (property templates).
- `src/pasnimout.nim` — nimony-compatible renderer (the .nim anchor /
  fallback path; kept as a debugging aid).
- `src/pasnifout.nim` — **the M2 TokenBuf NIF writer**: emits
  `.p.nif` + `.p.deps.nif` directly from the Pascal AST, byte-shape
  compatible with nifler's parsed dialect (see below).
- `src/pasler.nim` — **the front-end driver**: walks the uses-closure,
  produces the cache artifacts and runs `nimony s`.
- `runtime/systempas.nim` — the Pascal RTL shim imported by every
  generated module (`IntToStr`, string `+`, `TVarRec`, …).

### How pasler plugs into the nimony chain (M2, implemented)

The nimony driver's import resolver maps a module name to a
`<module>.nim` source only (`semos.resolveFile` appends `.nim`); the
`.p.nif` cache files are always nifler-derived and hash-named, never
import-addressed. pasler uses the driver's own contract instead of
fighting it:

1. parse the main program — the parser's `absorbUnit` walks the whole
   uses-closure transitively (shared `UnitSet`).
2. phase A: write every unit's translated `.nim` as an *anchor* into
   `nimcache/` (also the always-correct fallback).
3. wait until the anchors are a whole second in the past — the
   driver's staleness comparison (`deps.nim getLastModTime`, built with
   Nim 1 `times.toUnix`) collapses to **whole seconds**, so a
   sub-second write gap would make nifler re-fire and overwrite our
   NIF.
4. phase B: emit each unit's TokenBuf NIF to
   `nimcache/<moduleSuffix(anchor, paths)>.p.nif` + `.p.deps.nif`.
   `moduleSuffix` (gear2/modnames, imported for parity) hashes the
   anchor's path — the exact name the driver computes when it resolves
   `counter` → `nimcache/counter.nim`. Because the `.p.nif` is now
   newer than the `.nim`, `execNifler` prints "nothing to do" and our
   NIF flows through nimsem/hexer/lengc untouched. The main module is
   passed by file path (no resolution) and keeps its plain stem name.
5. `nimony s --nimcache:C --path:C --path:. C/<main>.p.nif` builds the
   binary at `C/<main>/<main>.p`; `--run` executes it.

Verified: `test/run.sh`'s `pasler-twounit` test asserts both the
correct program output and that the cached unit NIFs carry
`(.vendor "pasler")` (nifler never touched Pascal sources).

`pas2nimony --emit-nif:BASE` remains the single-module debugging path
(writes `BASE.p.nif` + `BASE.p.deps.nif` directly).

### pasnifout.nim — the parsed-NIF writer (M2 core)

- Header directives (`(.nif27) (.vendor "pasler") (.dialect
  "nim-parsed")`) are a **text prefix**; the reader (`readDirectives`)
  skips leading `(. ...)` directives, so the TokenBuf holds the module
  body only. `nifpools.writeFile`/`toModuleString` would re-emit a
  header — do not use them for `.p.nif`.
- The `.p.deps.nif` must mirror nifler's format exactly: same
  directives with dialect `nim-deps`, plain idents, no line info, and
  the module's own imports wrapped in `(import ...)` trees.
- Interface member declarations inside class bodies are **hoisted** to
  module level as bodiless `(proc ... .)` decls, exactly like the .nim
  renderer does — otherwise imported units cannot resolve members.
- Var defs without an initializer get the `defaultInit` mapping as NIF:
  `0`/`'\x00'`/`false`/`""`/`0.0` literals, `(nil)` for class/ref/ptr,
  `(curly)` for sets, `(call default <typedesc>)` for records/arrays
  (nimony's definite-assignment analysis rejects missing inits).
- `(.indexat)`/`(.vendor)`/`(.dialect)` are directive text; the
  embedded index is only needed for `.s.nif` inputs, not for `.p.nif`.

### Known upstream gap (M5 candidate)

Making pre-parsed modules *importable by name* (no `.nim` anchor at
all) would need alignment across four nimony sites with two different
suffix conventions (`extractModuleSuffix` → `"counter"` vs
`moduleSuffix` → `"couf6ivs7"`): `semos.resolveFile`, `deps.toPair`,
`semimport`'s suffix computation, and the plan's `.s.nif` naming. Not
a one-liner; the `.nim`-anchor approach sidesteps it cleanly.

## FPC compatibility answer

The goal "full FPC/Delphi-dialect source compatibility" does **not**
require FPC's source. We consult FPC *behavior* where the Delphi
dialect is ambiguous (unit scoping, `Result` vs function name,
`array of const` layout, property defaulting, operator set) and mirror
the observable semantics, not the implementation. FPC is GPL: no code
is taken from it, and its RTL/test suite is used — if at all — only as
a behavioral oracle run separately, never vendored into this
MIT-licensed tool.

## What is verified

`test/run.sh` translates every sample, builds it with the nimony chain
and runs it. Current samples:

- `testMethod.pas` — classes, virtual/override + `inherited`,
  properties incl. array + default array property, constructors,
  nested procs, `{$if false}` blocks, case-different identifiers.
- `samples.pas` — for/while/repeat, sets (`in`/`include`/`exclude`),
  case statements, strings/chars, records, arrays, case-canonicalized
  uses (`write('total=', total)` for declared `Total`).
- `except.pas` — raise/except mapping with `ErrorCode`, try/finally.
- `twounit/` — counter unit + program (classes across modules, unit
  initialization), built twice: once as translated `.nim` (twounit
  test) and once through pasler's direct `.p.nif` path with a
  `.p.nif` vendor check (pasler-twounit test).
## M5: the FPC oracle

`test/oracle.sh` compiles every sample in `test/oracle/` with real
FPC 3.2.2 (`-Mdelphi`) and with pasler and diffs the outputs
byte-for-byte. Current samples: strings, formatfloat, formatint,
strapio (StrUtils), mathfnc, dttime (DateUtils), strlist
(TStringList), lang (sets/case/records/virtual dispatch/negative
div-mod/exceptions). All eight are byte-identical today.

Divergences and fidelity fixes the oracle drove:

- FPC float rendering (verified against the variable path, which is
  what array-of-const always is): `%e` = 16 digits after the point +
  `E+-ddd`; `%g` = the exact digits, scientific iff `exp >= precision`
  (default 17), trailing zeros stripped, no plus sign in the exponent;
  `%f` = C-printf rounding at the requested precision (default 2).
  FPC *float literals* inside array-of-const are promoted to Extended
  and round differently (`Format('%.2f', [2.675])` = 2.68 while the
  same call with a Double variable gives 2.67) - documented v1
  divergence, we keep Double semantics.
- `FloatToStr` = FPC's general format with 15 significant digits
  (2.0 -> "2", 1e15 -> "1E15").
- `IntToHex` pads but never truncates: `IntToHex(4096, 2)` = "1000".
- `Format('%x')` prints the full hex digits without padding
  (255 -> "FF", 10 -> "A").
- `FormatDateTime(fmt, dt)` - the format string comes first
  (Delphi/FPC order); our earlier shim had it reversed.
- `DecodeDate`/`DecodeTime` take `Word` var params (Delphi
  signature); `DaysInMonth(dt)` vs `DaysInAMonth(y, m)` are distinct.
- `writeln(bool)` renders TRUE/FALSE. Imported `$` overloads do not
  resolve across nimony modules (verified), so the writeln lowering
  converts what the parser can prove: char literals become string
  literals, bool-typed operands (recorded vars, shim calls that
  return bool, `in` tests) go through `delphiBool`.
- `BoolToStr(b, useBoolStrs)` added.
- Pascal's declared array ranges: nimony honors `array[1..5]`
  indexing but its runtime check assumes 0-based storage, so both
  emitters now emit the 0-based size (`array[5, int32]`) and a
  post-pass (`adjustArrayIndices`) offsets index accesses by the
  recorded low bound. v1: literal ranges, module-level and local
  vars only.
- SplitString treats the delimiter as a character SET (Delphi
  semantics); FPC splits on the whole substring - oracle samples use
  single-char delimiters.
- `raise` is only allowed inside a routine (nimony limit): program
  bodies must raise from a proc. Our ErrorCode raise machinery works
  unchanged.
- Bodiless members of PROGRAM-local classes now register as methods
  (the parser used to require an interface section), and the `;`
  between a function's return type and its specifiers is consumed -
  `function Speak: string; virtual;` finally parses.
- `IntToStr` had a duplicate int64/int overload (nimony's `int` IS
  int64) - removed.

## M5-2: Str/Val/FormatFloat + oracle growth

Four new samples (ordinals, records, funcs, strconv) join the
oracle - eleven in total, all byte-identical. New fidelity work:

- `Str(v, s)` and `Str(v:w:p, s)`: FPC's Str rounds the SHORTEST
  round-trip decimal half-away (2.675:0:2 -> "2.68") - a different
  path from Format's %f variable path. The `:w[:p]` argument syntax
  parses in call args and lowers to a `pasW` shim call.
- `Val(s, v, code)`: FPC semantics - leading blanks skipped,
  optional sign, decimal and `$hex`, code = 1-based position of the
  first offending character, value zeroed on error. int32/int64/
  float64 overloads (nimony var params match exactly).
- `FormatFloat(pattern, value)`: '0' mandatory, '#' optional,
  ',' thousands, '0.00E+00' scientific, ';'-sections, half-away
  rounding, "-0" for negatives that round to zero (FPC does too).
- `Odd`/`Even` shims; `Ord('A')` keeps char args (the 1-char
  literal seam: most callees want a string, char-arg procs -
  StringOfChar, Ord, Chr - are on a denylist).
- FPC's `shr` is LOGICAL (zero fill) while nimony's is arithmetic;
  `a shr b` lowers to a delphiShr shim call so negatives match.
- `Inc(x, n)`/`Dec(x, n)` lower to typed assignments - nimony's
  inc/dec reject mismatched offset types (int64 literal vs int32).
- `with` on RECORD variables qualifies members directly against
  the original expression (records copy by value; a hidden temp
  would swallow writes). Class `with` keeps the ref temp.
- Variant records (`case Integer of` and `case Tag: Type of`)
  parse; v1 flattens every branch into plain fields (typical use
  writes one branch and reads it back - identical behavior).
- Aliased array types (`TArr = array[1..3] of TPoint`) now carry
  their low bound into variable declarations (index offset pass).
- Typed constants (`const Answer: Integer = 42`) and default
  parameter values (`punct: string = '!')` parse; nimony needs the
  default's type to match, so 1-char literals become strings.
- Documented v1 gaps found on the way: untyped integer constants
  in mixed arithmetic (declare them typed), literal arguments to
  int/float overloads (nimony resolves ambiguously - use typed
  vars), FPC requires `;overload` on every overload in a unit,
  Succ/Pred past the last enum value is out of range in FPC.

## M5-2b: writeln/write width syntax

The Delphi `e:w[:p]` argument form now works in write and writeln
(13th oracle sample, byte-identical). The probed FPC semantics:

- ints and strings: right-aligned, space-padded to the width.
- floats with only `:w`: a SIGN SLOT (a space for non-negatives) +
  a mantissa with `max(width - 8, 1)` digits after the point +
  `E+-ddd` (minimum 3 exponent digits). Probed across magnitudes
  from 1e-10 to 1e15. FPC float LITERALS in these positions are
  Extended-promoted and render differently - documented divergence,
  the samples use variables.
- floats with `:w:p`: fixed-point, rounded shortest-repr half-away
  (the Str rounding path), left-padded to the width.

## M6-1: Delphi class operators (the deferred set)

Implicit, Explicit, Inc and Dec class operators now work on value
types (14th oracle sample `operators.pas`, byte-identical):

- `class operator Implicit(a: T1): T2;` / `Explicit` / `Inc` /
  `Dec` lower to uniquely named procs keyed by the signature
  (`opImplicit_Integer_TMyInt`), registered in a new SymTab
  conversion registry. The arithmetic/bitwise/comparison operators
  keep mapping to nimony operator symbols (`Add` -> `+` and so on).
- Assignment statements insert Implicit calls when the sides have
  known incompatible types: record <-> record (different types),
  record <- int literal/int-typed expression, record <- float,
  base <- record. A best-effort RHS type guess (varTypes, param
  types, record field types, literal/infix shapes) drives it.
  v1: no conversion inside arbitrary expressions, only at
  assignment targets.
- `Inc(x)`/`Dec(x)` on a record operand lower to
  `x = opInc_cls(x)`; int operands keep the typed-offset
  arithmetic lowering.
- Type casts over records (`Double(m)`) lower to the registered
  Explicit proc.
- Pascal widens int expressions into float assignment targets
  silently; nimony needs the cast - inserted for float-typed
  targets (vars, params and `result`).
- `result` carries the routine's return type during body parsing
  (converted assignments inside record/float-returning functions
  work); routine params record their types so `a.Value` resolves.
- nimony quirk: a record-returning function needs
  `result = default(T)` before field assignment (init proof) -
  the parser injects it (already the case for plain functions).
- Delphi note: `Inc`/`Dec` class operators are unary; the two-arg
  `Inc(x, n)` statement form stays arithmetic-only.

## M6-2: real-world "food" sample (wordfreq)

`test/oracle/wordfreq.pas` - a corpus generator + word-frequency
analyzer - is the 15th oracle sample, byte-identical with FPC.
Building it flushed out eight real gaps, all fixed:

- forward-style one-liner class decls (`EBadLine =
  class(Exception);`) now emit the real type (empty body).
- parentless classes: `inherited`, `inherited Create/Destroy`
  lower to no-ops (TObject semantics; argful parent calls still
  error). Parentless classes inherit RootRef like the M4 ones.
- `try..finally` bodies parse all statements up to `end` (the
  parser stopped after one); the .nim renderer prints every
  nkFinally son (it dropped all but the first).
- `break` / `continue` statements (v1 lexer has no keywords for
  them; statement-position symbols lower to the loop-control
  nodes).
- method return info is keyed per class
  (`returnsValue["tstringlist.add"]`): a user `TFoo.Add` can no
  longer suppress the discard for `TStringList.Add` call sites.
  Shim ref-object types now register as classes and derive
  RootRef (the `Free(self: RootRef)` call type-checks).
- Pascal `/` (real division) converts non-float operands to
  float64 explicitly - nimony's `/` accepts floats only.
- int->float assignment widening casts (M6-1) also cover the
  `result :=` bodies of float-returning routines.
- the discarded-value rule now consults the receiver's class
  before the global name flag.

## M6-3: nimony-compatible command line

Both drivers now speak the nimony CLI:

- `pasler [command] [options] program.pas [-- program-args]` with
  nimony's command verbs: `c` (default; the fast NIF pipeline),
  `n` (native libc-free backend), `w` (wasm - the shims are not
  freestanding-safe yet, nimony's own error surfaces), `check`
  (type-check only), plus `m`/`s`/`doc`/`l` tolerance. `n`,
  `check`, `w`, `m`, `doc`, `l` delegate to nimony's own project
  graph over the rendered .nim anchors; `c` keeps the TokenBuf
  NIF pipeline.
- Options nimony understands are forwarded verbatim (`-d:SYM`,
  `--path:DIR`, `-o:`, `--cc:`, `--opt:`, `--passC:`, ...);
  FPC-style `-dSYM` is accepted too. `--run`/`-r` builds and
  executes; arguments after `--` reach the program.
- `--path:` is shared state: it feeds the Pascal uses-resolution
  (units can live in --path: dirs, not only beside the importer)
  and nimony's import paths.
- Conditional compilation (Delphi model): `{$ifdef}/{$ifndef}/
  {$else}/{$endif}` evaluate at parse time from CLI defines plus
  source `{$define}/{$undef}`; the dead branch is skipped at the
  token level, so its units never absorb and its declarations
  never register. CLI defines propagate into used units; source
  defines stay local. `{$if}` still lowers to a nimony `when`.
- pas2nimony accepts the same option surface (verbs are accepted
  and ignored - it always translates).
- Suite section `pasler-cli` covers define on/off runs and
  `check`; `pasler n` verified manually on the probe (native
  binary produced and runs).

### Why the non-`c` paths go through `.nim` and not the NIF pipeline

Design rationale, probed and fixed in M6-3 - kept here so we do not
re-derive it:

1. **nimony exposes exactly one entry point into pre-digested NIFs.**
   `nimony s` takes a `.p.nif` and runs the full buildGraph including
   link - it is "finish this already-parsed project", not a generic
   "apply backend X to a NIF". There is no sem-only or
   backend-pluggable NIF action, so `check` over our NIFs is not even
   expressible today: the check path starts at `.nim`.
2. **Backend selection lives at the project-graph level.** `n`/`w`
   swap in a different stdlib configuration (nimNativeAlloc /
   nimNativeIo for the libc-free native build, freestanding rules for
   wasm), a different system module variant, different CC/linker
   invocations. Those knobs are applied when nimony builds the module
   graph from sources.
3. **Our TokenBuf NIFs bake in the default config's world.** The NIF
   pipeline pre-resolves unit references into hashed module names and
   shim symbols against the default (C-with-libc) sem world. Probe:
   `nimony s --native hello.p.nif` -> `undeclared identifier:
   pasCurrentExc`, while the same shims under `nimony n hello.nim`
   build and the binary runs, because there nimony parses and
   semchecks everything itself - including systempas.nim - under one
   coherent configuration. The breakage is in the pre-resolved NIF
   graph, not in the shims.
4. **Delegation is cheap: the artifact already exists.** The `.nim`
   anchors are phase A of the pipeline we maintain anyway. The cost
   is a re-parse by nimony's own nifler - irrelevant for the
   non-hot-path commands - and NIF dialect drift cannot break those
   paths, since nimony generates its own NIFs from our source.
5. **It puts future work in the right place.** The `w` failure
   (`no filesystem on a freestanding target` from std/os.nim) is a
   shim problem, not an emitter problem. Once the shims are
   freestanding-safe, `pasler w` works with zero pasler changes.

If we ever want native/wasm on the NIF route, that needs nimony to
expose backend selection for the NIF entry point plus
config-agnostic emission on our side. Until then the `.nim`
delegation is the honest interface, and `c` - the oracle-critical,
byte-identical path - stays untouched.

## M6-4: real-world sample happy.pas (lucky-ticket counter)

Three language gaps closed while compiling a real 45-line program
(nested loops over a triangle of digit sums, `x in [0..9]`,
`TDateTime`/`Now`/`MilliSecondsBetween`):

- **Paren-less calls**: Delphi calls parameterless functions without
  parentheses in expression position (`d1 := Now`). The parser now
  tracks known routines' parameter counts (shim sources via
  scanNimExports, user routines via parseRoutine) and a bare
  identifier resolving to a 0-arg routine - not shadowed by a
  variable - becomes a call. Statement position already called.
- **`in` over a literal set**: `x in [0..9]` lowers to a comparison
  chain (`(x >= 0) and (x <= 9)`; singleton -> `==`; several elements
  -> an or-chain; empty -> false). nimony types a `{0..9}` set as
  set[int] and rejects int64 as a set element, so the set-typed
  emission could not work; comparisons match Delphi semantics for
  scalar membership exactly. Non-literal set operands keep the set
  emission (future set-type support).
- **Unit-qualified calls**: `DateUtils.MilliSecondsBetween(...)` now
  spells the module the way the uses clause imported it
  (pasdateutils), mirroring the shim mapping.

The run is byte-identical to FPC on the deterministic line:
`Found 4816030 tickets.` (the msec timing is wall-clock, not
comparable). Suite section `pasler-happy` guards the count.

## M6-5: real-world sample TestSHL_SHR (derived)

Adrian's own Delphi shift-semantics investigation program (102
lines: shl/shr vs div, int8/16/32 width functions, Format,
GetTickCount benchmark). The derived sample drops the two inline
x86 `asm` bodies (no v1 backend; the block skipper keeps other
programs parsing) and the blocking `Read`. The run is identical to
FPC 3.2.2 on all 128 deterministic lines (`time =` filtered);
FPC on Linux needs a 5-line `windows.pas` stub unit for the
comparison (`uses SysUtils, Windows` is kept as the real-world
shape).

Gaps closed:

- **`inline;` routine directive**: lexes as its own `pxInline`
  token, which `parseRoutineSpecifiers` never accepted (it looked
  for `pxSymbol` words only). The directive is accepted and
  ignored (nimony's default call convention is nimcall anyway).
- **`asm ... end;` blocks**: skipped at the token level - as a
  routine body (no `begin`) or as a statement - with an "# asm
  skipped" comment in the output. The body's assembler semantics
  are a documented non-goal for v1.
- **`Windows.GetTickCount`**: `uses Windows` resolves to
  systempas (like SysUtils); the shim computes epoch milliseconds
  (divergence: no boot-time origin, no 49.7-day wrap - programs
  use deltas).
- **`E.Classname`**: exception shim method returning
  "Exception" (the shim keeps one canonical runtime type -
  subclasses do not report their own name).
- **Negative-literal width casts**: `A := -512` on an int32 now
  casts (`-512` types as int64 in nimony); nkPrefix joined the
  width-cast set.
- **Width-correct shr**: the old delphiShr shim shifted in 64-bit
  width, so `int32(-512) shr 1` truncated wrong (-256 instead of
  FPC's 2147483392). The parser now emits the unsigned twin at the
  operand's declared width (`int32(uint32(a) shr uint32(b))`),
  falling back to the shim only for untyped operands.
- **toVrec overloads** for the small integer widths (the Format
  array-of-const slots rejected uint32 subtractions).
- **try-body statement separators**: a compound statement
  (for/begin..end) leaves its `;` unconsumed; the try body loop
  now eats it between statements.

Suite section `pasler-shlshr` diffs the 128-line FPC-verified
expected output (timing line filtered).

## M6-6: routine directive forwarding and directive diagnostics

Routine directives are no longer silently dropped:

- **Forwarded as pragmas**: `inline` (already lexed, now also
  rendered), plus the nimony-fulfillable calling conventions
  `cdecl` and `stdcall` (probed: nimony compiles all three and
  resolves call sites consistently). Both the .nim renderer and
  the TokenBuf NIF emitter forward the def's pragma son; `raises`
  is excluded from forwarding (the M4 machinery pre-stuffs it into
  the pragma node and the raise analysis owns its emission).
- **Unfulfillable conventions** (`register`, `pascal`, `safecall`)
  produce a diagnostic: a WARNING by default (deduplicated per
  process, so the multi-pass pipeline prints each site once) and
  an **error under `--strict`** (both pasler and pas2nimony),
  which stops the build.
- Everything else (reintroduce/abstract/dynamic/deprecated/
  platform/experimental) stays consumed-and-ignored for now.
- Suite section `pasler-cc`: one warning + correct run by default,
  an error exit under `--strict`.

### Directive-by-directive status (M6-6 follow-up)

| Delphi directive | handling |
| --- | --- |
| `inline` | forwarded as `{.inline.}` |
| `cdecl`, `stdcall` | forwarded as `{.cdecl.}` / `{.stdcall.}` |
| `deprecated [ 'msg' ]` | forwarded as `{.deprecated.}`; the message is documentation only and is dropped (nimony's NIF pragma-argument form is not reproduced by the hand emitter) |
| `dynamic` | fulfilled: message-based dispatch lowers to the same virtual model (`isVirtual`) |
| `abstract` | no body follows (like `forward`); dispatch goes to overriding descendants |
| `reintroduce`, `platform`, `experimental` | genuine no-ops (Delphi hint suppressors / documentation annotations) - consumed and ignored, no diagnostic |
| `register`, `pascal`, `safecall` | unfulfillable calling conventions: warning by default, error under `--strict` |

### Method hiding: `reintroduce` vs `override` (M6-6c)

The v1 dispatch model previously conflated a descendant's `virtual`
with `override`: both became nimony `method` defs, so a base-typed
reference dispatched to the descendant's redeclaration - diverging
from Delphi, where a descendant `virtual` (no `override`) introduces
a **new slot** and a base-typed reference keeps dispatching to the
ancestor's method.

Semantics now:

- `procedure Foo; reintroduce; virtual;` (or plain `virtual` over an
  ancestor's same-name method) lowers as a **static per-class
  routine** - the per-class call resolution plays the new slot; a
  base-typed reference stays on the ancestor's method (FPC-verified:
  shadow.pas prints child-newslot / base / child2 in both).
- `override` keeps nimony `method` dispatch (same slot) - unchanged.
- Hiding **without** `reintroduce` produces the Delphi-parity warning
  "method 'Foo' hides virtual method of ancestor type - add
  reintroduce to acknowledge" (deduplicated per process); with
  `reintroduce` it is silent. `reintroduce` therefore has a real
  role: it acknowledges the hide, matching Delphi's hint.
- The implementation parser keeps the class body's decision: a
  self-only `isMethodDeclared` check replaces the ancestor-walk
  flip, so a hidden method's implementation stays static while an
  override's implementation dispatches.
- Known v1 divergence: a *further* descendant overriding the new
  slot virtually (`TDog2 overriding TChild.Foo`) lowers statically -
  deep virtual chains through reintroduced slots are not modeled.

## M7: private compiler food + declaration-position directives

### Private corpus structure

Closed-source Delphi sources (stock RTL/VCL, FastScript, FastCube,
HCL sample units) feed the parser from `test/private/`, which is
gitignored and never published. `test/private/corpus/` holds the
units, `test/private/run.sh` sweeps them with FPC-on-Linux target
defines (`-d:LINUX -d:UNIX -d:POSIX`), and the suite's
`private-corpus` section reports parse health (skip-safe when the
corpus is absent). Derived samples for the published test/ tree must
be minimal reproductions, never copies.

### M7 parser additions

- **Uses-clause conditionals and comma-first continuations**:
  `{$IFDEF DXE2UP} System.ObjAuto, {$ENDIF}` inside a uses list and
  the `,
  NextUnit` continuation style parse. The uses loop is now
  fully directive-tolerant and ends only at `;`/Eof.
- **Declaration-position directives**: a shared `declDirective`
  handles conditionals between type/const/var definitions, routine
  local decls, uses clauses, class bodies, begin-blocks (including
  conditionals that span a begin/end boundary), after `then`, and
  between unit-level declarations. Dead branches skip at the token
  level; taken branches' tokens flow through the enclosing loop; the
  trailing `{$else}`/`{$endif}` of a handled group are consumed in
  statement position. `{$IF DEFINED(X)}` is the only supported
  `{$IF}` form here.
- **`{$EXTERNALSYM X}` and friends** between definitions are consumed
  and ignored (they no longer close a type section).
- **`array of X` type aliases** work (they already flowed through
  `parseTypeDesc`; the section-tolerance fix unlocked them).
- **`out` parameters** lower like `var` (initialization semantics not
  modeled - documented).
- **Var/field defaults** `var X: T = init;` (module, local, class
  fields) parse and render; the former check tested `:=`.
- **`inherited` in expression position** (`Result :=
  inherited Add(x)`) unwraps the statement form's discard wrapper;
  the statement form keeps its old no-op semantics.
- **Underscore identifiers**: `_FILETIME` and friends lex now.

### Corpus status and remaining gaps

15 of 41 surveyed units translate; the parse-error classes left:
`with T.Create do` on non-class-variable bases (v1 with-expression
limit), a class with a real parent implementing an interface,
`inherited` against shim-parent classes, conditionals spanning an
if-header (`{$IFDEF} cond1 {$ELSE} cond2 {$ENDIF} begin`), and
`Types.pas`'s first derail (pointer chains inside wider contexts).

## M7b: corpus-driven parse fixes

Sweeping the private corpus exposed further parse gaps, fixed the
same round (corpus health: 17 of 41 units translate):

- **Comments between definitions**: type/const/var section loops now
  skip comment tokens; a `{...}` banner comment between type aliases
  no longer closes the section (the follow-up aliases spilled into
  expression space).
- **Interface members**: the COM GUID bracket member `['{...}']` and
  property declarations inside interfaces are consumed (accessor
  plumbing; method calls through the interface still lower).
- **Shim-parent inherited**: a class whose declared parent is not in
  our registry (e.g. `class(TList)` against the TStringList shim)
  lowers `inherited` to a no-op with a one-time warning (statement
  form) or `0` (value form). Named calls, array-property getters and
  `inherited Data[i] := v` assignments are consumed at the token
  level.
- **Nested constructor/destructor** declarations are allowed between
  routine local declarations (like nested procedures).
- **`asm`-bodied routines**: `function F: T; asm ... end;` - the asm
  block is the whole body; no begin-block follows (the previous
  behavior consumed the *next* routine's begin as this routine's
  body, swallowing every following declaration).
- **With on array elements**: `with Buckets[i] do` resolves the
  element class/record of array-typed fields, properties and
  `array of X` aliases (new `arrayAliases` map); record withs qualify
  against the original expression text (`Buckets[ABucket].Items`),
  and record-scope member chains (`with A.B[i] do` through an outer
  with) resolve through classFieldTypes.
- Remaining v1 gaps (documented, not fixed): with on arbitrary call
  results (`with Unit.Object.RegisterClass(...) do`), with on fields
  of classes from un-absorbed units, interface impl with a real
  parent, if-header-spanning conditionals.

## M8a: case bodies, routine types, multi-dim arrays (corpus-driven)

- **Case branch bodies keep trailing `;`** (`of 1: Foo();` before the
  next `of`); **case-`else` takes a statement sequence** until `end`
  (ObjAuto's `else Result := ...; ... end;`).
- **Routine types**: `assembler` joins the no-op specifier set;
  `parseRoutineType` parses the return type before the `of object`
  marker, so method-pointer types `function(x): WideString of object`
  parse; keyword-escaped parameter names (`const Operator: TVarOp`)
  get the `pasX` rename and are accepted as tokens.
- **Multi-dim static arrays** lower to nested single-dim chains
  (`array[a..b, c..d] of T` -> `array[a..b] of array[c..d] of T`),
  including `of const` element tables.
- **Repeat bodies** consume statement separators; **uses-clause head
  breaks** on `;`/EOF; empty `while C do ;` bodies tolerated.

## M8b: with on arrays, pointer aliases, open-array params

- `with PWideStrData(Data)^ do` (pointer-cast deref) and `with
  PExpr^ do` (pointer-alias deref) resolve via a `pointerAliases` map
  (`P = ^T` element types) registered in the type section.
- Array variables with class/record elements register their element
  type (`arrayVarElems`) so `with List[i] do` qualifies; open-array
  params `const X: array of TRec` register the element likewise.
- Nested variant records (`case Boolean of True: (...); False: (...)")
  recurse in the record-case machinery; `implements` lists parse and
  lower the parent type only (v1).

## M8c: try/except `else`, plain handlers, class casts

- The bare `else` after `on` branches is the `else` **keyword token**,
  not an identifier; the except-section's else path accepts both
  spellings (`on EConvertError do <comment> else raise; end;` from
  IniFiles/Registry).
- A plain `except <stmts> end` handler (no on/else) parses its body
  as a statement sequence stored as the branch's third son (the
  renderer reads the body at index 2; the old `[1] =` write crashed
  the renderer on every multi-statement handler).
- `with THashedStringList(expr) do` resolves the cast's class.

## M8d: with on call results

- The parser records each function's return spelling in a
  `routineReturns` map (bare + class-qualified keys) and the
  with-expr-class reads it: `with SomeRoutine(...) do` resolves when
  the routine is known (declared in the unit or an absorbed unit);
  bare routine calls resolve the same way. The ht-family variant - a
  call on a unit-level variable from an un-absorbed unit - stays a
  documented v1 gap.

## M8e: quoted directive bodies, bare raise/property, dispinterface

- The lexer treats old-style `(*$HPPEMIT '...'*)` and curly
  `{$HPPEMIT '...'}` bodies whose content starts with a quote/char
  literal as raw text up to the end marker (bodies may contain both
  quotes and asterisks - StdVCL); the opener token is still delivered
  and the end marker waits one getTok.
- Bare `raise` followed by `end`/`finally`/`else` is a re-raise
  (ZLib's `except ... raise end;`).
- Bare `property OnProgress;` (re-exposed ancestor property) parses
  without a type clause; `readonly`/`writeonly`/`dispid N` property
  tails are consumed as COM plumbing.
- `X = interface;` forwards and `X = dispinterface ...` bodies lower
  to comments (no COM dual-interface lowering in v1).
- Routine-header specifiers tolerate `{$IFDEF} external; {$ENDIF}`
  wrappers between them, so the Windows-only `external;` dead branch
  no longer ends the specifier list.

- Pristine-RTL census additions: IniFiles, Registry, ZLib, StdVCL
  join the translating set (ZLib/StdVCL only as comments-free type
  shims where COM surfaces were skipped).

## M9: with fallback, metaclasses, mapping clauses, full corpus

- **With on unknown class** lowers to a hidden temp with an empty
  type node (type inferred from the initializer); the body's members
  stay unqualified (a call on a unit-level variable of an
  un-absorbed unit, e.g. the ht family). Corpus 25->28.
- **`class of T` metaclasses** (`TPersistentClass = class of
  TPersistent;`) lower to a comment alias in parseRecordOrObject's
  head - v1 has no class-reference lowering. **Interface-method
  mapping clauses** (`function IUnknown.QueryInterface =
  ObjQueryInterface;`) lower to a comment; the implementation keeps
  its own name. Property `stored <expr>` / `immutable` streaming
  specifiers are consumed. Corpus 28->35; Classes.pas and the COM
  family translate.
- **Conditionals in expression, case-label and loop positions**: the
  type header (`T = {$IFDEF X}{$ELSE}packed{$ENDIF} record`),
  anonymous record members, the repeat body's until, the if's else
  chain, empty then-bodies, case branch labels
  (`tkInteger, tkClass {$IFDEF FPC} ,tkBool {$ENDIF}:`), conditional
  wraps around whole case branches and case-else, finally body
  separators, and expression positions
  (`x = {$IFDEF}#10{$ELSE}#13{$ENDIF}`). Corpus 36->**39/39 - the
  private sweep is complete**.

## M9d: raise-at, operator identifiers; pristine census complete

- `raise E at ErrorAddr;` keeps the plain raise and consumes the
  location qualifier (v1 has no raise-location lowering).
- Keyword-escaped parameter names work in expression position too
  (`case Operator of` on `const Operator: TVarOp`).
- **The whole pristine rtl/common directory (28 units) translates.**

## M10: nimony compile-health tier (corpus .nim builds)

The compile tier asks a second question per corpus unit: does the
generated `.nim` actually build under the nimony chain
(`nimony c --path:.` in `test/tmp/pc`, with the runtime shims and the
placeholder shims copied in)? `test/private/compile.sh` and
`compile-errors.sh` sweep all 39 units; baseline 3/39 -> **11/39
compile-clean**.

Emitter and shim layers this milestone:

- Shim absorb: units with no `.pas` source are scanned from
  `runtime/` and the new `runtime/placeholders/` shims
  (`absorbNimModule`), so zero-arg shim routines seed the paren-less
  call machinery and shim methods register per-class member sets
  (`s.addRoutine(cty, name)`) for bare-call self-qualification.
- `Libc.nim` is a real shim now: `TSemaphore`/`TRTLCriticalSection`,
  the `sem_*` family (RtlNames-mapped to semInit/semWait/...),
  CriticalSection stubs, and the Win32 compat surface with exact
  widths (`QS_*`/`PM_REMOVE` `'i32`, `WAIT_*`/`INFINITE` `'u32`),
  `FARPROC` proc type, `GetModuleHandle`/`GetProcAddress` stubs.
- Plain proc-type aliases register via `procTyTypes`; `Assigned(x)` on
  a proc var lowers to `x != nil`; calling conventions on proc-type
  aliases render as real cc pragmas (matching the callee signatures).
- Delphi `var X;` untyped var params emit `var pointer` stand-ins
  (nimony rejects `var untyped` everywhere, including proc types).
- Exception handlers bind through `pasAs[T](pasCurrentExc)` (nimony
  rejects bare downcasts between ref types); `raise` keeps the
  ErrorCode model from M9d.
- `result`'s varTypes slot resets on class returns (a stale width from
  the previous routine cast the next body's result asgn to int32).
- Cross-type literal assignments: char literal -> string target
  ("" & ch), empty string -> char target (cast[char](0)), narrow
  ord() call args at int32-param call boundaries.
- Runtime shells: root `Destroy` method, `ClassType`/`InheritsFrom`,
  `RaiseLastOSError`, `Maxint`, `TMethod`/`TPoint`/`IInterface`,
  `Variant`/`TVarData`, `TInvokeableVariantType`/`EVariantError`;
  `TStringList` inherits `TStrings`; TList member shims
  (Add/Count/`[]`/`[]=`/Extract/First/Clear/Delete), TStringList
  Sorted/CommaText accessors; `StrToDate`/`StrToDateTime`/`StrToTime`
  raise stubs; `EConvertError` + Create.
- The suite's pasler blocks now copy the runtime shims into their own
  directories and clear stale nimcache module copies (the pasler
  pipeline resolves imports from its own dir; leftover copies
  shadowed fresh shims and broke the suite).

Remaining compile-tier census (each is its own machinery layer):
default array properties render wrong inside for loops
(`Items[Index]`/`Format[i]` -> mangled calls, Contnrs/fcCustomFormat),
enum-set const literals (`tkAny - tkMethods`), narrow-width call args
beyond `ord()` (IniFiles), `SyncObjs` corpus MSWINDOWS-only bodies
under LINUX defines, and the VCL tier (`TFrame`/`TForm`/`Menus`) plus
the ht-family adapter units. FPC cannot compile SyncObjs on Linux
either (it needs Kyrix's Libc); the frames are a VCL shim tier.

## M11 - Delphi 2007 oracle tier (dcc32 under wine)

The oracle contract gains a Delphi leg: `test/delphi-oracle.sh` translates
each corpus sample, compiles it with dcc32 (Delphi 2007) inside a wine
prefix relocated into the workspace, runs both binaries and diffs the
normalized output. FAILs are findings-only (the script exits 0); the
census tracks the tier separately from the nimony leg.

- Prefix: `../.wine/delphi2007/` (gitignored via `.wine/`). The DSH
  sandbox is workspace-write only, so the prefix MUST live inside the
  workspace - wine writes outside it hit "Read-only file system" and
  dcc32 fails with `F2039 ...drf`.
- Wine: `~/.PlayOnLinux/wine/linux-x86/7.11/bin/wine` (32-bit for the
  win32 prefix). Skip `wineboot -u` (it times out; wine self-configures
  on first use). Always `export WINEPREFIX=... WINEDEBUG=-all
  XDG_CACHE_HOME=/tmp/xdg-cache` and normalize CRLF with `tr -d '\r'`.
- dcc32 rule: with an ABSOLUTE source path the .exe lands next to the
  source; compile the scratch copy under a RELATIVE name instead.
- MSWINDOWS defines tier: the uses bridge grew a `windows` branch
  importing `runtime/placeholders/Windows.nim` (the Win32 compat shim:
  THandle/HWND/HWnd/HMODULE/HINSTANCE as `uint32`, the WAIT_*/QS_*/
  PM_*/RPC_* consts, the wait/mutex/critical-section/window/OLE API
  stubs, `Win32Platform`, `HWND_MESSAGE` = `0xFFFFFFFD'u32`). The
  spelling passed to `absorbNimModule` must match the file name
  (Linux is case-sensitive) or the scan silently runs on nothing.
- `HMODULE`/`HINSTANCE` are `uint32` (D2007: `= THandle`), and
  `FARPROC` carries the exact TCoWait signature so
  `GetProcAddress(...)` assigns to the corpus's proc-type var.
- Emitter additions this tier: `sameRef` class comparisons (nimony's
  `==`/`!=` refuse ref objects; the rewrite runs at routine-body
  completion where the per-routine param maps are still live),
  `pasCStr` for `PChar(<int expr>)` (nimony forbids int<->cstring
  casts outright), PChar(string var) passes through bare (nimony
  accepts only string literals as cstring() args), ord() operands of
  bit operators widen to the operand's width (rhsExprType now
  propagates the narrow width through parens and bit chains), setLen
  widens its count to int64, `New`/`Dispose`/`HexToBin`/`BinToHex`
  shims, `TStrings.Get/CaseSensitive(+setter)/AddObject/GetObject/
  BeginUpdate/EndUpdate/IndexOf/Changed`, `TMemoryStream`
  (Memory as an int32 address number), and array consts of string
  convert their char literals (`array[Boolean] of string = ('0','1')`).
- `inherited m(...)` renders the arg form with the cast to the
  member's DECLARING class (nimsem resolves the bare name through the
  receiver's static type; the slot at that class carries the declaring
  signature). The shim scan now parses `ref object of Parent` chains
  (shim ancestor walks no longer collapse to no parent).

### Probed: nimony's I/O does not translate line endings on Windows

A cross-compiled win64 probe (nimony C backend + x86_64-w64-mingw32-gcc,
run under wine) shows nimony's `std/syncio` is a pure-Nimony layer with
**no text-mode translation**: `echo`, `writeLine` to a text-mode file
and `writeFile` all emit bare `\n` on Windows (nim 1 differs: it goes
through C `stdout` in text mode, so the MSVCRT translates `\n` to
`\r\n`). Consequences:

- The platform line ending is the runtime's job: `pasLineEnding`
  resolves to `"\r\n"` under `-d:MSWINDOWS` and `"\n"` otherwise
  (mirroring FPC's `LineEnding`), and `TStrings.Text`/save paths plus
  the `Writeln` shim route through it.
- Locale surfaces stay our shim's job on every platform (the nimony
  runtime reads no locale; float formatting is always `.`).
- Cross-compilation caveats found while probing: the i386 Windows
  target fails to link (nimony's Windows system C code declares Win32
  procs without `__stdcall`, so Ubuntu mingw's decorated imports
  `_ExitProcess@4` never match); use `--cpu:amd64` for now. Old wine
  (6.17-staging amd64) hangs a nimony PE at exit - wine 11.x works.

### M11b - cross-platform line endings, locale record, banker's rounding

Landing of the "Delphi on Windows, FPC normalization elsewhere" policy:

- `runtime/systempas.nim` gains `pasLineEnding` (`"\r\n"` under
  `-d:MSWINDOWS`, `"\n"` otherwise - the FPC `LineEnding` model) and
  `TFormatSettings`/`pasFormatSettings` (the FPC-shaped locale record;
  v1 defaults are `.`/`,`/ISO-ish, a per-platform locale query can fill
  the same fields later). `FloatToStr` and the `Writeln` float path
  route through `applyDecSep`, so populating `DecimalSeparator` from a
  locale makes the German-locale outputs (`0,6046875`, `ratio=2,6`)
  match Delphi without code changes.
- `runtime/pasclasses.nim`'s `TStrings.Text` joins with
  `pasLineEnding` - Delphi's CRLF-joined text on Windows, FPC's LF on
  Unix from the same source (the `strlist` oracle sample's
  `textlen=23` finding is resolved by this).
- `pasRound` implements Delphi/FPC banker's rounding (halves to the
  even neighbor; nim's `round` is half-away-from-zero) and the RTL
  spell map now sends `Round` there - `RoundTo(2.5, 0) = 2` matches
  Delphi's `rt=` row.
- The SimpleRoundTo finding turns out to be the Extended-precision
  divergence, not rounding: Delphi's 80-bit Extended keeps
  `2.675/0.01` below the half, FPC (and our double-based shim) print
  `2.68`. Ours matches FPC, which is the anchor per policy; documented
  as a deliberate divergence.
- `test/delphi-oracle.sh`'s pasler leg now compiles with
  `-d:MSWINDOWS -d:WIN32 -d:WINDOWS` (the Windows deployment of our
  chain - CRLF line endings) while `oracle.sh`'s FPC leg stays LF.
  Delphi census: 6/15 PASS (was 5); `funcs`/`ordinals`/`strapio` still
  fail at the dcc32-compile step (VCL-tier census units).
- `test/private/compile-errors.sh`/`compile.sh` thread the LINUX/UNIX/
  POSIX defines into their nimony compile lines so the shims see the
  same platform identity the translation used.

## M12 - IniFiles tier: TStrings-object surface, exception-flow cascade

The pointer-pool/TStrings-object tier landed. `IniFiles` moved from
failing to compile-clean in the census (12/39 OK, was 11).

- **nimony checked-exception model (probed)**: a routine whose body
  contains a `raise` must announce `{.raises.}` (both renderers already
  did via containsRaise); its call sites may only sit inside an
  except-bearing `try` - from another `{.raises.}` routine too, and
  inside a bare `try`/`finally` does not count (the frontend regression
  test in nimony's tree documents the same rule). A re-raising `except`
  is fine - the checker only rejects unprotected call sites. New
  `wrapRaisingCalls` fixpoint pass: every unprotected call site of a
  raising routine is wrapped in `try: <stmt> except: raise` (plus a
  `result = <default>` before the raise for value-returning routines -
  nimony's flow check demands the result slot initialized), and the
  added bare raises pull the enclosing routines into the raising set
  until the call graph settles.
- **hexer bug (probe)**: a bare `raise` inside a `case` scrutinee's
  else inside an except arm fails hexer with
  `could not find symbol: pasECode2.0`; the identical shape as an
  if-chain compiles and runs. The `on X do H else raise` lowering
  therefore splices the else body into an if-chain
  (`if pasECode2 == <code>: H else: raise`) instead of the case's else.
  The pre-existing v1 gap that *dropped* `else raise` entirely (the
  on-section's else was discarded with a "not supported" comment while
  the dropped node still triggered the raises marking) is closed.
- **Properties of ancestor classes**: `isMemberName` now walks the
  property list at every link of the parent chain (it previously
  checked only the routine's own class), so a corpus method's bare
  `FileName` (a TCustomIniFile property) self-qualifies inside a
  TMemIniFile method. `with`-expressions qualify their implicit-self
  members at parse time because the lowered temp's init lands in a var
  section the def-level self-qualify walk stops at.
- **Assignment LHS**: `wrapMemberCalls` no longer wraps an assignment's
  LHS in a read call - the shim setter sugar needs the bare member form
  (`Stream.Position = v` targets the TMemoryStream field).
- **Default indexed property**: a class-typed bare index base
  (`Strings[I]`) is Delphi's default property; the parser inserts the
  member (`Strings.Strings[I]`) so the indexed-property lowering
  applies, and the renderer lowers indexed-property reads to the
  getter calls (objects/strings/values/names) and indexed-property
  writes to the setter calls (PutObject/Put).
- **Shims**: TStrings GetName/GetText/Assign/Put/PutObject stubs on the
  abstract base, TStringList.Put (`fLines[i] = s`), SysUtils const tier
  (NameValueSeparator/PathDelim/DriveDelim/ExtensionSeparator), the
  reserved `pasFind` spelling for Pos/AnsiPos (+ RTL pin + char
  overload - the corpus's own `Find` methods must not pollute the canon).
- Delphi census stays 6/15 PASS; the oracle suite stays 15/15.


## M14 - directive forwarding completed, `absolute`, corpus closure complete

The private-corpus sweep went from **20 to 47 of 55 units translated, 0 failures** (8 more
satisfied by runtime shims), and the suite grew to 54 sections, all green.

- **`{$if <expr>}` forwarding finished.** The parse-time evaluation scaffolding was
  removed; every `{$if …}` now becomes a Nim `when`, no exceptions. Four positions had
  to be handled: statement (already), type section, `uses` clause, and inside a
  `begin … end` block. The last one was a single bug worth 11 units - the block loop
  `break`ed at the directive and then reported `expected end but got: {$`. `parseIfDir`
  is called from all four, carrying a `condWhenStack` so a closer is only consumed by the
  group that owns it.
- **`absolute` variable aliases** (35 occurrences, 12 units) lower to a template with the
  target as its body. Verified at module and local scope, including the write-through
  case. A local alias needs no initializer of its own; the target's declaration owns the
  storage.
- **`{@exclude}`** used to hang the parser: `parseStmt` had no `pxCommand` arm, so the
  token was never consumed and the unit loop called `skipCom` forever, producing no output
  at all. Skipping the annotation (and the `}` the lexer delivers separately) fixed it.
- **A unit-level no-progress guard** now turns that whole failure class into a warning plus
  a step over the offending token, capped at 32 per unit - a batch translator hanging
  silently is the worst possible failure mode.
- **Nested `goto` resolved at the source, not in the translator.** The last 10 units all
  died on one routine in a corpus unit, where a `goto` jumped
  two blocks *inward* to skip the cursor advance on the first pass. `FFirst` already
  carried that state, so each of the two scan arms now reads
  `if FFirst then FFirst := False else inc(<cursor>)` with no label and no jump. The
  refusal for genuinely inward jumps stays, pinned by
  `test/negative/goto_nested.pas`.
- **`Registry` shim**: the last MISSING unit. One corpus unit listed it in `uses` without
  taking a symbol from it, but the closure could not resolve it. The API surface was read
  from the Delphi 2007 RTL source shipped in the wine prefix
  (`source/Win32/rtl/common/Registry.pas`); `runtime/placeholders/Registry.nim` now covers
  the full `TRegistry` surface plus `TRegIniFile` over an in-memory store. Documented
  divergence: pasclasses' `TStrings` is an unbacked base (its `Add` is a no-op), so the
  enumeration helpers take a `TStringList`.

**Corpus repairs, not translator workarounds.** Forwarding a `{$if}` as a `when` means both
branches must be parseable, which Delphi never required of a dead branch. Two were needed:
One corpus unit had `:` where `;` belongs, and another needed the nested `goto` above.

**Status: nothing in this tier has been type-checked.** Translation is reach; compilation
is the next milestone.

## M15 - the lazyBtree tier type-checks (P4-P7)

Translation was reach; this milestone opens the compile tier. The first target is the
five `lazyBtree*` modules (`lazyBtree`, `lazyBtreeInt64`, `lazyBtreeDateTime`,
`lazyBtreeString`, `lazyBtreeText`), the generic-container core of the corpus. They go
from **197 nimony `Error:` lines to 0** - each module now builds a binary (`nimony c`
exit 0). The suite grows to 86 sections and stays otherwise green (the only failures are
the pre-existing NIF shapes `pasler-withalias` / `pasler-withptr`), so no regression was
traded for the tier.

The work was a census-driven sequence of error classes:

- **P4 - literal and size widths.** `sizeof`/`high`/`low` results coerced to their
  declared int32 context; untyped integer constants that fit int32 emit as `int32(N)`;
  non-literal `case` labels are wrapped so nimony can range-check them.
- **P5 - Pointer / typed-pointer / ref boundaries.** Delphi lets `Pointer`, typed
  pointers and object refs pass implicitly; nimony needs explicit conversions, so
  `coercePtrArgs` and `coercePtrAsgns` insert `T(x)` / `cast[T](x)` at routine
  boundaries, driven by the `typeAliasTargets` / `varRawTypes` registries. Set-element
  narrowing (`Include`, `in`) casts to the declared subrange.
- **P6 - method pointers (`procedure … of object`).** The `T = object evProc, evObj`
  lowering now carries the function return type, represents `evObj`/`self` as `pointer`
  (so `TMethod(v).Code`/`.Data` assign to a plain proc type without a forbidden
  proc-to-proc cast), rewrites `TMethod(x).Code`/`.Data` to `evProc`/`evObj`, and keeps
  the record-proc-type field spelling so the assignment knows its target.
- **P7 - `PVariant`.** The shim gains `PVariant* = ptr Variant` and a pointer
  `Dispose` overload; `absorbNimModule` records shim `X* = ptr Y` aliases so a
  `v: PVariant` participates in the pointer coercions. `isVariantExpr` recognises `v^`
  (deref of a `ptr Variant`), and `initValueResult` covers `Variant` returns
  (`result = default(Variant)` = `varEmpty`, matching Pascal's uninitialised `Result`).

Fixing those revealed four **general** lowering bugs that the earlier errors had masked:

- **Nested routines that capture outer locals** need an explicit `{.closure.}`; nested
  routines now get one.
- **`out` parameters.** Pascal `out` was lowered as `var`, which made nimony demand an
  initialised argument at the call site. It now emits `out` (a new `Node.isOutParam`
  flag, rendered by the source writer; the NIF writer still emits `mut`).
- **Pascal value parameters are mutable**, nimony's are not. A value parameter that is
  directly reassigned has its formal renamed and a `var <name> = <fresh>` local copy
  prepended, so the body keeps its own mutable local and call sites stay by-value.
- **A variable typecast passed to a `var` parameter** (`TValueType(Value)` as an
  argument) is an lvalue in Delphi, but a plain cast is not passable by `var`; it lowers
  to the address-cast lvalue `cast[ptr T](addr x)[]`.

Two harness/robustness fixes were prerequisites for trusting the census at all:

- `test/run.sh` piped every build through `grep … || true`, so a failing build exited 0
  and the runner printed `-- ok` - a false positive that had hidden failures. Builds now
  capture the real exit status and the runner asserts it.
- A missing `{$I file}` is now a hard error (`include file not found: …`) instead of a
  silent skip, which only resurfaced later as an undeclared identifier. Locked in by
  `test/negative/missing_include.pas`.

New samples pin each fix: `inctest` (nested/quoted includes with `.inc` companions),
`addrparam`, `iftypesec`, `localif`, `msgwhen`, `uscore`, `dotparent`, `inharith`,
`uscmethod`, `multidim`, `mathfn`, `sysmem`, `shortstr`, `localtype`, and the NIF-shape
probes `withalias`/`withptr`/`withptrparam` (the first two still fail on the NIF path).

**Status: 5 of the 55 corpus units now type-check and build.** The rest were only ever
translated, not compiled, so the next milestone walks the same census loop across the
remaining units.
