# Toolchain

pas2nimony is built and tested **with the nimony toolchain itself**:

```
../nimony/bin/nimony   (nimony 0.4.0 at time of writing)
```

## Pinned nimony commit

- pas2nim repo HEAD at the time this subfolder was created:
  `cbd3e432403e1d6e10d0d54903f7c301aaa015b1` (pas2nim)
- nimony checkout at `/home/adrian/dev/nimony`, git HEAD at creation:
  see `git -C ../nimony log -1 --format=%H` output recorded in `nimony.commit`
  next to this file.

## Build

```
./build.sh            # builds bin/pas2nimony with ../nimony/bin/nimony
```

The produced binary lands in `bin/pas2nimony`.

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
