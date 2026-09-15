# Toolchain

pas2nimony is built and tested **with the nimony toolchain itself**:

```
../nimony/bin/nimony   (nimony 0.6.3 development at time of writing)
```

Two consumption paths exist, and both must be kept working:

| path | front end | driver invocation |
|---|---|---|
| `.nim` | `pas2nimony` renders Nim source | `nimony c --path:. foo.nim` |
| pasler NIF | `pasler` writes parsed NIF directly | `nimony s --path:… foo.p.nif` |

`pasler` is the primary one; `test/run.sh` exercises both, as does
`test/oracle.sh` (differential against FPC 3.2.2).

## Version history and what each bump changed

- **0.4.0 → 0.6.3 development** (nimony `19973add..2a458ed9`, 2026-08-30 →
  2026-09-11, 66 commits; nativenif `73eadb4..7bd3d04`, 22 commits). The
  update was verified **not** to break this project: no emitted tag moved
  (all 48 tags pas2nimony registers resolve to identical numeric ids), the
  parsed-NIF contract and `nifmake`/`--path`/`-o:`/nimcache handling are
  unchanged, and the three failures it was suspected of causing reproduce
  identically on a 0.4.0 build (see lessons 37/38).
- Upstream's `formatBiggestFloat`/`formatFloat` lost their `snprintf`
  dependency in this range. The shim's `fpcFormatE`/`fpcFormatG`/
  `fpcSciMantissa` route through those, so FPC-equivalence was re-measured:
  digit-for-digit identical to FPC 3.2.2 across `%f`, `%e`, `%g`, `%n`,
  `%m` and a grid of magnitudes and precisions, with **two** known
  non-equivalences that are ours, not upstream's: `%.0f` of a `x.5` value
  rounds half-to-even (FPC rounds half-away) and non-finite values lose
  their sign/spelling (`FloatToStr(1.0/0.0)` prints `0`, FPC prints
  `+Inf`). Both reproduce on the 0.4.0 build, so neither is a regression -
  they are latent shim defects.

## Pinned nimony commit

- pas2nim repo HEAD at the time this subfolder was created:
  `cbd3e432403e1d6e10d0d54903f7c301aaa015b1` (pas2nim)
- **Pinned nimony commit: `2a458ed96bdaed53ae64483596050ba279d55d86`**
  (`2a458ed9`, 2026-09-11, "std/regex: port lexim to Nimony as a plugin").
  The checkout at `/home/adrian/dev/nimony` is held at this commit until
  `TestSimpleQuery.exe` links under `--os:windows`; bump it only together
  with a full `build.sh` + `test/run.sh` + `test/oracle.sh` re-measurement,
  because nimony's strictness on integer-width and `var`/`out` argument
  binding is itself part of what we are validating.

## Build

```
./build.sh            # builds bin/pas2nimony with ../nimony/bin/nimony
```

The produced binary lands in `bin/pas2nimony`.

`build.sh` resolves the compiler as `../../nimony/bin/nimony`, i.e. it
expects a checkout whose parent directory also holds `nimony/`.

**Never put a `nimony` symlink (or directory) next to this repository.**
nimony refers to its own standard library by a path it computes relative
to the build directory. A `nimony` entry at the repository root makes
that path resolve through the symlink, and the build then dies with a
*stale-looking* error such as

```
Error: unhandled exception: nifcore.nim(1278, 3) `c.rem == 0` into:
  body did not consume all 18 children (left 2) [AssertionDefect]
```

or, after clearing `nimcache`,

```
[Error] cannot open: nimcache/<hash>.s.nif
```

Both are symptoms of the extra `nimony` entry, not of the source being
edited. If a build fails this way, check `ls -ld nimony` in the
repository root and remove it before touching `src/`. A `git worktree`
such as `.head-check/` needs no symlink at all: from
`<root>/.head-check/pas2nimony` the `../../nimony` path already resolves
to the sibling checkout.

Also: `./build.sh` reports failure only in its last lines, and an earlier
successful binary stays in place. Always confirm the rebuild by checking
that `bin/pas2nimony`'s mtime moved, not just by an empty `grep Error`.

## Notes on writing nimony-compatible Nim (lessons so far)

These are the practical constraints we hit when writing a real program
(a lexer/parser tool) against nimony. They are the "skills for nimony and
its toolchain" this project is meant to accumulate — keep them current.

1. **Case sensitivity**: nimony is fully case-sensitive (`FData` and
   `fdata` are different identifiers). Nim-2 style insensitivity is only
   available via `{.feature: "ignoreStyle".}` which we deliberately do
   not use — pas2nimony's *output* must match the declared case exactly.
2. **No `{.this.}` pragma**: the pragma is gone (semantic error). Member
   access must be written as `self.field` / `self.method(...)`.
3. **Pointers are not-nil by default**: `ref`, `ptr`, `pointer`,
   `cstring` and proc types cannot hold `nil` unless declared with the
   `nil T` prefix. Pascal semantics require nilable pointers and
   nil-initialized managed types, so generated modules start with
   `{.feature: "lenientnils".}`.
4. **Strict definite assignment** (`contracts.nim`): using a local
   variable that was not proven initialized on every path is an error
   (`cannot prove that x has been initialized`), regardless of feature
   flags. Consequence for generated code: Pascal locals get explicit
   initializers. Consequence for tool code: prefer constructor-style
   initialization (`p.open(...)`) as `std/parsejson` does; avoid
   `var L: T` + open through an intermediate local stream variable.
5. **`{.feature: "v2".}`** is the Nim-2 compatibility bundle (implies
   lenientnils, lenientfloats, ignoreStyle, ...). Not used by default.
6. **Toolchain layout**: `bin/nimony c foo.nim` runs nifler → nimsem →
   hexer → lengc → cc and drops the binary in `nimcache/<mod>/<name>`.
   The pipeline accepts pre-parsed `.p.nif` files (see
   `../nimony/src/nimony/deps.nim`: `toPair`, `execNifler` skip nifler
   for `.nif` inputs) — **verified end to end** in
   `test/pasler-poc.sh`: place `<mod>.p.nif` + a `<mod>.p.deps.nif`
   (headers `(.vendor …)`, `(.dialect "nim-deps")`, then `(stmts …)`
   with plain-name `(import …)` entries) in `nimcache/`, then
   `nimony s --nimcache:nimcache --path:. nimcache/<mod>.p.nif`.
   Module suffixes are path-dependent hashes — one import location per
   module, or sem fails with `cannot open: nimcache/<otherhash>.s.nif`.
   Pure-literal integer arithmetic (`21 * 2`) is typed `int` (64) —
   assignments to Pascal Integer vars are emitted as `int32(…)`.
   This is the hook the pasler frontend (stage 2) builds on.

## Rebuilding nimony

If the nimony checkout changes:

```
cd ../nimony && nim c -r src/hastur/hastur build nimony
```

## Cross-compiling to Windows (verified working)

nimony's C backend builds genuine Windows executables from Linux, and they
run under Wine. Verified end to end on a hello-world and on a
class/ctor/try-except probe: Pascal -> PE32+ -> runs.

Two flags are **both** required, and one environment variable:

```
CC=x86_64-w64-mingw32-gcc bin/pas2nimony src.pas -o:src.nim
CC=x86_64-w64-mingw32-gcc ../nimony/bin/nimony c \
    --os:windows --cpu:amd64 \
    --cc:x86_64-w64-mingw32-gcc \
    --path:. src.nim
```

- **`--cpu:amd64` is mandatory.** 32-bit Windows fails to link: nimony's
  Windows system C declares Win32 procs without `__stdcall`, so Ubuntu
  mingw's decorated imports (`_ExitProcess@4`) never match.
- **`--cc:` alone is not enough — `CC` must be set too.** The two flags
  cover two different steps, and missing either one fails late and
  confusingly:

  | step | driven by | symptom when wrong |
  |---|---|---|
  | compiling the generated `.c` | `--cc:` (`config.cc`) | glibc's `compilers.h` is pulled in via `-I.`/system headers and the target's `windef.h` is "not found" |
  | the **final link** | `CC` env var | `niflink` falls back to `cc`/`gcc` and `ld.bfd` tries to link Windows COFF objects: `undefined reference to ExitProcess`, `__emutls_get_address`, `__main` |

  `niflink` picks its driver from `getEnv("CC", "cc")`
  (`src/niflink/niflink.nim`). Passes `--cc:` through to the compile and
  link nodes from `config.cc`, but the final link is a separate
  `niflink` invocation that only reads `CC`.
- **`--linker:` is not the fix.** `config.linker` sets a *custom* linker
  name that is resolved as a tool, not a compiler driver; leaving it
  empty is correct.

Wine needs a writable prefix; the default one is not usable here. Use the
repo's own prefix (already in `.gitignore`):

```
WINEPREFIX=$PWD/.wine/probe64 wine build/Prog.exe
```

Wine 11.16 (staging) is the version this was verified against — the
project's earlier notes record that old wine (6.17-staging) hangs a
nimony PE at exit.

`pasler --run` cannot be used for a Windows target: it executes the
output directly, and Linux cannot exec a PE (`Exec format error`). Build
with `pasler c`/`nimony c` and run the `.exe` under `wine` yourself.

## New lessons from stage 1 (translator development)

These were verified against nimony 0.4.x while building and testing the
translator itself — keep current:

7. **Uninitialized `result` is garbage**: a proc whose result is only
   assigned on some paths (e.g. `return true` inside a loop) must start
   with `result = false` — Nim 1 zero-defaults, nimony does not.
8. **`Table[]` reads are `{.raises.}`**: `t[key]` (KeyError) cannot be
   compiled outside `try`; use `t.getOrDefault(key)` for reads. `[]=`
   writes are unannotated and safe. Same for `openFileStream` and
   `writeFile`.
9. **`func` implies `{.noSideEffect.}`** — use `proc` in tool code that
   mutates parser state.
10. **Bare re-`raise` needs `{.raises.}`** on the enclosing proc even
    with `{.feature: "canraise".}`.
11. **The exception model is `ErrorCode`-based**: the only raisable
    values are builtin `ErrorCode` constants (`Failure`,
    `SyntaxError`, …); raising procs take bare `{.raises.}` (a type
    list is rejected for enum constants); `except ErrorCode as e`
    catches; do not define your own `ErrorCode` — it shadows the
    builtin and breaks `raise`.
12. **For-loops over 32-bit counters**: nimony's for-loop itself is
    fully generic — the limitation is `countup`'s signature
    (`step: Positive = 1` is 64-bit, and the `succ` magic requires the
    step type to match the counter type, so `inc(i32var, int64val)`
    fails). The stdlib `..` / `>..` iterators step with single-arg
    `inc i` / `dec i` and instantiate for every ordinal type including
    `int32` and `char`.
13. **Iterators may take `var` parameters** and this gives Pascal
    for-loop semantics: `pforTo(v: var T, a, b: T)` drives the
    *declared* Pascal variable through the `var` argument while the
    for-loop binds `_`. Body reads, `break` value, nested-routine
    captures and the after-loop value (b+1 / b-1) are all Pascal-exact.
    Defined in `runtime/sysstrs.nim` (the proto-systempas unit).
14. **Static arrays with `low != 0` + runtime index is broken in
    nimony 0.4.x**: `d[3]` with constant 3 on `array[1..3]` works
    (range-aware), but `d[i]` with a runtime `i` fails the openArray
    converter's zero-based `idx < x.len` requirement. Zero-based arrays
    are fine. Upstream issue; affects translated Pascal code with
    1-based static arrays.
13. **Statement-level calls of non-void procs need `discard`** — Nim 1
    allows ignoring results, nimony does not.
14. **Forward declarations are supported** (`proc f(x: T)` before the
    body proc) — used for Pascal interface sections.
15. **Table/seq assignment is MOVE semantics.** `a.field = b.field`
    transfers the buffer; destroying both owners double-frees. Shared
    mutable state between objects must live behind a `ref object`
    holder (see `UnitSet` in paspars.nim for the cross-unit cycle
    guard). Symptom: SIGSEGV in `destroyTParser`.
16. **Object constructors don't reliably default-init unlisted fields**
    under nimony — build structs incrementally:
    `result = T(); result.newField = initTable[...]()`. Adding a field
    to an initialized object requires updating every init site.
17. **`nimony c` resolves imports against the lib dirs only** — add
    `--path:.` for multi-module projects, and note module lookup is
    case-sensitive: the import ident must match the FILE stem exactly
    (Pascal unit `Counter` may live in `counter.pas`).
18. **Debugging translated-output or translator crashes**: gdb gives
    mangled-but-usable symbols (`destroyTParser` etc.) on nimony-built
    binaries; assertions surface as `[AssertionDefect]` + exit 1
    without a stack trace.
19. **nimony's import resolver only maps module names to `.nim`
    sources** (`semos.resolveFile` appends `.nim` and searches origin
    dir + `--path` dirs). A hand-written `.p.nif` for an *imported*
    module is invisible to resolution — the working contract is a
    `.nim` anchor on the path plus the `.p.nif` in the cache under the
    anchor's `moduleSuffix` hash (that is exactly what pasler does).
20. **The driver's staleness comparison has whole-second granularity**
    (`deps.nim getLastModTime` → Nim 1 `times.toUnix` in the built
    binaries). Writing a `.p.nif` milliseconds after its `.nim` anchor
    makes nifler re-fire and overwrite. Leave a >=1s gap between
    anchor and cache writes.
21. **`(.nif27)`-style directives are reader-level text**: the reader's
    `readDirectives` skips leading `(. ...)` tokens, so a TokenBuf
    holding only the module body + a text header prefix is a complete
    `.p.nif`. `nifpools.writeFile`/`toModuleString` re-emit their own
    header (and an `.indexat`/index section meant for `.s.nif`) — never
    use them to write a parsed module.
22. **nimony's `open(f, filename, mode)` returns bool with `out File`**
    — `var f = open(...)` compiles against the wrong overload and fails
    silently. Use `var f: File; if f.open(...)` and handle false.
    File ops live in `std/dirs`/`std/paths` (`createDir(path(...))`),
    read/write via syncio `readFile`/`writeFile`; there is no
    `copyFile`/`sleep` in the nimony stdlib (`usleep` importc for
    POSIX waits).
23. **Module identity is the path hash**: `moduleSuffix(path, paths)`
    = first 3 chars of the module name + base36 of `uhash(relative
    path)` — e.g. `counter.nim` on `--path:nimcache` becomes
    `couf6ivs7`. Front-end drivers must import `gear2/modnames` for
    exact parity instead of re-implementing the hash.
24. **Parsed-NIF shapes for direct emission** live in the
    `nimony-codegen-checklist` skill and `doc/nimony-compat.md` ("the
    parsed-NIF writer"); the recurring theme: probe nifler's output for
    the same construct and diff slot-by-slot — dot counts differ per
    node kind (5-slot let/param/fld, 4-dot proctype, empty-pragmas dots
    on type/record, `(nil)`/`(curly)` tags, `(quoted name =)` for
    operator templates).
25. **nimony's ctor-call semantics**: inside a ctor body a
    value-returning call must be `discard`ed; a bare dropping call
    fails with "got ref X.Obj but wanted ref Y.Obj". Inherited
    constructor chains are therefore `discard create(Parent(self), v)`.
26. **Property accessors are templates in the parsed dialect**:
    `(template name x . . (params ...) ret . . (stmts ...))` with
    `(quoted name =)` for setters — dropping them makes property
    reads/writes fail with "undeclared field".
27. **Delphi 1-based strings**: the parser rewrites string index
    expressions (`s[i]` -> `s[i-1]`, literal 1 folds to 0) using three
    type tables: `paramTypes` (innermost routine, saved/restored like
    `outerParams`), `varTypes` (module vars), `fieldTypes`
    ("class.field", incl. bare self-fields inside methods). Nimony's
    `find` returns **-1 when absent**, so `find(s, sub) + 1` maps
    Delphi's `Pos` exactly; `Copy` maps to `substr(s, a-1, a-1+b-1)`
    (inclusive 0-based, clamped); `Delete`/`Insert` have no nimony
    equivalent — 1-based `strDelete`/`strInsert` shims in `systempas`
    (clamped, not raising).
28. **Probe naming pitfall**: a probe file named like a local variable
    (`s.nim` with `var s`) makes nimsem resolve the ident to the *own
    module* ("got: (module)") and fail with misleading type errors.
    Name probes something neutral.
29. **nimony `echo` varargs quirk**: expression-position calls are not
    statement calls — builtin rewrites that change shape (`Pos`,
    `Copy`, `Delete`, `Insert`) must hook the *primary* call builder,
    not only the statement-level call site.
30. **Never truth-test a Node sentinel with `!= nil`**: `emptyNode(info)`
    returns a valid object, so `if x != nil:` is always true — the asgn
    rewrite silently replaced statements with the empty node and the
    module loop turned them into `#` comments. Sentinel checks must be
    `.kind != nkEmpty`.
31. **Property templates only inline in the `.nim` path**: nimsem on
    parsed NIF does not resolve `(dot obj Prop)` through the accessor
    template (`undeclared field: 'Prop'`). Lowerings that must work in
    both paths (method-pointer asgns/calls/nil tests) map a property
    base to its backing field first.
32. **`procedure of object` lowering**: nimony closures segfault and
    anonymous thunks fail nil-proofs; the working shape is a two-field
    record (`evProc`/`evObj`) plus *named module-level thunks* that
    `cast` the `RootRef` self back to the handler's class. The same
    record needs an inline `(proctype …)` in the NIF fld slot — a dot
    type loses the field entirely.
33. **Inherited constructors**: `Third.create(…)` on a ctor-less class
    resolves through the *ancestor's* ctor and needs the cast-back;
    synthesize a default no-arg `create` only when no ancestor
    declares one either, or the synthesis hijacks inherited-ctor
    overload resolution.
34. **nimony reserves `Exception`**: the system module predefines it, so
    a prelude class of that name is unreachable — spell the runtime
    type `PasException` and alias the Pascal name through the
    registry (`names["exception"]`, plus a `classes` entry so lookups
    by the Pascal name resolve).
35. **`T(x)` cast-calls trip the nil-proof across modules**: an
    inherited-ctor call lowered as `Parent(self)` fails with
    "cannot prove expression is not nil" when the parent lives in an
    imported module — `cast[Parent](self)` is the reliable form.
36. **Ref upcast assignments need provably-non-nil sources**: assigning
    a routine-call result to a base-typed var fails the nil-proof
    (`cannot prove expression is not nil`) — wrap the RHS in
    `cast[Base](...)`.
37. **A local declaration does not hide an imported overloadable one**
    (nimony 0.6.x: `import` is not a shadowing boundary). A proc/func/
    template/iterator/enum field in the same module joins the *same*
    symbol choice as an imported one of that name; the choice is then
    resolved by expected type first, then scope distance. So a subclass's
    own `create` no longer hides the prelude constructor — verified on
    both 0.4.0 and 0.6.3 for a cross-module `Create(cast[Base](self), m)`
    call. What *does* still break is the **same-module** case: when the
    user's subclass and the base constructor live in one generated module
    (as in `test/except.pas`), the translator's own `MyError.Create` is a
    genuine candidate for its own body's `Create(...)` call, and the
    argument is cast to the base so the derived overload matches first:
    `expected: EMyError.Obj but got: PasException.Obj`. Give the prelude
    routine a non-colliding name (`pasExcCreate` in `systempas.nim`) -
    the discipline is still required, just for a narrower reason than this
    entry originally claimed.
38. **nimony's build cache is now graph-driven, not mtime-guesswork**:
    `cachedconfigfile.txt` stopped being an input of every sem node and a
    new `nifmake --rerun` flag forces a re-sem when options change. The
    old advice — `rm -rf nimcache bin/pas2nimony` before debugging a
    "mysterious" persisted error — is still safe, but a same-minute edit
    no longer silently no-ops. **Do not run two toolchain builds
    concurrently against the same nimcache**: with `nifmake -j`, a second
    build reusing the cache can fail with
    `[Error] cannot open: nimcache/<mod>.s.nif`, which is a cache
    collision, not a real error. Clean `nimcache` and re-run alone before
    investigating that message.
39. **`uses <placeholder unit>` needs the module next to the anchor**:
    paspars absorbs a unit that has no real source (`Windows`, `Forms`,
    `Controls`, …) and the anchor then says `import Windows`. The
    `runtime/placeholders` directory is deliberately *not* on `--path`
    (it only ever supplies declarations, and a real unit of the same name
    must win), so `nimony` reports `file not found`. Both drivers mirror
    the placeholder into nimcache — `pasler.mirrorPlaceholders` and the
    `cp runtime/placeholders/*.nim` in `test/run.sh` — and never overwrite
    an existing file of that name.
40. **Indexed collection properties are accessor calls in both emitters**:
    Pascal's `List.Strings[i]`, `.Objects[i]`, `.Values[k]`, `.Names[i]`
    have no nimony equivalent, so `pasnimout`/`pasnifout` lower them to
    `Get`/`Put`, `GetObject`/`PutObject`, `GetValue`, `GetName`. nimsem
    does *not* resolve `(dot obj Prop)` through the shim's accessors on
    parsed NIF, so the NIF emitter must do the same lowering as the `.nim`
    emitter or the pasler path disagrees with it. Each of those accessors
    needs a **concrete override on `TStringList`**: the `TStrings` base is
    abstract and its accessors are stubs, so an inherited `Get` silently
    returns `""` and an inherited `Put` discards the write.
41. **A method call resolves in the *static* receiver's vtable, and
    `RootRef`'s vtable is the builtin `RootObj`'s — empty and not
    extensible.** `cast[RootRef](p)` followed by a method call is
    therefore a hard error, even though the call would dispatch fine on a
    typed receiver:
    ```
    [Error] method `Destroy` not found in class RootObj
    ```
    Declaring `method Destroy*(self: TList)` (and on every other shim
    root, and on `TPersistent` for the `TComponent` branch) does **not**
    fix it — verified. Consequence for the WIP lowering of
    `TObject(Ptr).Free` / `TObject(Ptr).FreeNotification(x)` in
    `paspars.mapStringBuiltins`: `pasFreeObj` / `pasFreeNotification`
    cannot be implemented as shims until the translator can name the
    receiver's real class, and a module that merely *contains* such a
    helper fails to build even when the call never executes — so there is
    no partial version to ship. Do not reintroduce those two names
    without solving that first; Contnrs' `TObjectList.Notify` and
    `TComponentList.Notify` are the only corpus callers.
    `pasPtrToObj`, which sat next to them, was never needed at all: a
    plain `cast[TargetTy](ptr)` works (the translator emits it directly).
42. **`p.outerResultTy` is stored lowercased, and nimsem's `cast` check is
    case sensitive.** A cast built from it emits `cast[rootref]`, which
    fails with `cannot cast between types pointer and rootref`; the
    spelling has to come back through `passym.rtlSpelling`
    (`tobject`/`tclass` → `RootRef`) the way the other cast sites do.
