# pasler/pas2nimony - bringing worlds together

pasler is a pascal compiler using the https://github.com/nim-lang/nimony compiler toolchain via .nif files. pas2nimony translates pascal to .nim source code.
The project is on an early state but can already translate a reasonable subset of the Delphi/Freepascal language. Because pasler is using the nimony toolchain it can also
use .nim modules in its `uses` clause.

## What is working

**Language subset** (each locked in by a sample under `pas2nimony/test/`):

- units with interface/implementation, multiple units per program, `uses` clauses
- procedures, functions, nested routines, `var`/`const`/typed parameters, default parameters
- classes: inheritance, virtual/override methods, constructors/destructors, class methods and
  class vars, properties (read/write), `is`/`as` with runtime-checked casts, `self`
- interfaces (with method resolution through the class), `reference to procedure` anonymous
  methods, generics (classes and specialized aliases)
- operator overloading for records and classes
- sets (`in`, `include`/`exclude`, set literals), subranges, static arrays, records with
  `case` variants, pointers and `^` types
- exceptions: `try/except/finally`, `raise` of exception classes with instances, `on E do`
- `case/of`, `with` (incl. nesting and Delphi scope semantics), `goto`/`label`, `repeat`,
  `while`, `for` (incl. `downto`, `break`/`continue` semantics), `exit`, `inherited`
- string helpers mapped to nimony builtins (`Pos`, `Copy`, `Length`, `SetLength`, ...)
- `uses nim.<pkg>.<mod>` - direct use of nimony std modules from Pascal

**RTL shim units** (hand-written pure nimony, `pas2nimony/runtime/`):

- `SysUtils` core: `IntToStr`, `FloatToStr`, `StrToInt/Def`, `StrToFloat/Def`, `IntToHex`,
  `Trim` family, `UpperCase`/`LowerCase`, `CompareStr`/`CompareText` and the `Ansi*` family,
  `StringOfChar`, **`Format` with Delphi array-of-const** (`%d %s %x %e %f %g %n`, index,
  width, precision), the path family (`ExtractFilePath/FileDir/FileName/FileExt`,
  `ChangeFileExt`, ...), file ops (`FileExists`, `DeleteFile`, `DirectoryExists`, `CreateDir`)
- `StrUtils`: `LeftStr`/`RightStr`/`MidStr`, `PosEx`, `AnsiReplaceStr/Text`,
  `ReverseString`, `DupeString`, `SplitString`, ...
- `Math`: `Floor`/`Ceil`, `Power`, `Hypot`, `RoundTo` (real Delphi banker's rounding),
  `SimpleRoundTo`, `CompareValue`, `Sign`, `SameValue`, ...
- `TDateTime` core + `DateUtils`: encode/decode cascade, `DayOfTheWeek`, `Now` (UTC),
  unix timestamps, ISO week numbers, `FormatDateTime` (common token subset), the `Inc*`/
  `Between*`/`Start*`/`End*`/`Recode*`/`IsValid*` naming layer
- `Classes`: `TStringList` (`Add`, default property `List[i]` and `List['key'] := v`,
  `IndexOf`, `Sort`, `SaveToFile`/`LoadFromFile`, name=value handling)

Documented v1 gaps live in `pas2nimony/doc/nimony-compat.md` (e.g. `Now` is UTC, no local
timezone; indexed property forms like `List.Strings[i]` need `List[i]`).

## Building and running

```sh
# needs a nimony checkout beside this repo (build.sh expects ../../nimony)
cd pas2nimony
./build.sh                 # produces bin/pas2nimony and bin/pasler

# two ways to run a Pascal program:
bin/pas2nimony hello.pas -o:hello.nim     # translate to readable .nim ...
../../nimony/bin/nimony c --path:runtime hello.nim   # ... compile with nimony
bin/pasler --nimony:../../nimony/bin/nimony --run hello.pas   # one-shot driver
```

`pas2nimony` is the translator (inspect its `.nim` output to see exactly what was
generated); `pasler` parses to NIF and drives the nimony compiler directly, like any
other language frontend would.

## Using it for testing

1. **Self-test suite** - `cd pas2nimony && ./test/run.sh` runs every sample through
   **both** pipelines (the `.nim` path and the pasler NIF path) and reports pass/fail;
   currently 33 sections, all green. New language semantics get locked in as a sample the
   moment they work, so the suite doubles as a regression net and an executable
   feature list.

2. **Differential testing against FPC** - the test programs are plain Delphi-dialect
   Pascal, so the same file can be compiled with Free Pascal Compiler and with pasler and
   the outputs compared:

   ```sh
   fpc -Mdelphi -oOut hello.pas && ./Out > fpc.txt
   bin/pasler --nimony:../../nimony/bin/nimony --run hello.pas > pasler.txt
   diff fpc.txt pasler.txt
   ```

   The shim units make this meaningful: `uses SysUtils, StrUtils, Math, DateUtils,
   Classes` resolve in both worlds (natively in FPC, as our shims here), so a sample that
   exercises `Format`, `TStringList`, `EncodeDate` or `RoundTo` produces comparable
   output. Byte-identical output on both paths is the bar every shim currently meets.
   Where a shim should ever diverge from FPC behavior, porting from the FPC RTL is an
   option but stays behind an opt-in flag for testing only (LGPL - the shipped shims are
   our own code).

3. **Nimony chain testing** - the shim units and the generated `.nim` are ordinary nimony
   sources; they exercise nimony's strictness gaps constantly (each quirk found is
   documented in the skill checklist), which makes the project a useful stress test for
   the nimony toolchain itself.