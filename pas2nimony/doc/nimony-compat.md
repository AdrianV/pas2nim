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
| `x is T` / `x as T` | `(x != nil) and (x of T)` / `pasAs[T](x)` | **runtime-checked (M4)**: nimony's `of` answers true for nil, so `is` guards with `!= nil` (Delphi: `nil is T` = false); `as` rides the generic `systempas.pasAs[T]` — nil stays nil, a failed check yields **nil** instead of raising EInvalidCast (the raising variant would mark every transitive caller `{.raises.}` — nimony only allows such calls inside try); a failed cast therefore surfaces as a nil-deref later |
| `label L1, L2, 10;` + `goto L` | forward: `block pasGotoL: …` + `break pasGotoL`; backward: `while true: …` + `continue` | **structured rewrite (M4)**: the region between the goto and the label becomes the block/loop; forward gotos from nested loops and out of `try..finally` work (the finally runs, like Delphi); numeric labels supported; label scope is routine-local. v1 rejects: a label targeted both forward and backward, backward gotos crossing a loop boundary, jumping into a nested block, labels not inside a begin/end — all with precise errors |
| `with E1, E2 do` | hidden temps `var pasW<n>: T = E` + member qualification | **instance-aware (M4)**: bare idents in the body resolve against the with-classes (innermost first, Delphi shadowing — with-members beat locals and `self` fields); later expressions are evaluated in the scope of the earlier ones (`with S, FOrigin do` = `FOrigin` of `S`). v1 heads: class-typed vars/params, `self`, ctor calls, member chains of class-typed fields; value objects/records unsupported (the temp would copy — needs ptr lowering). Planned: Oxygene-style `with E as X do` naming the temp explicitly |
| `{$ifdef X}` etc. | `when defined(X)` / `when false:` | `{$if …}` conditions limited to `defined()` combinations |

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