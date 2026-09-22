#!/bin/bash
# pas2nimony test runner
#
# Translates every sample in this directory, compiles the generated Nim
# with the nimony compiler chain and runs it. A sample passes when both
# the translation and the nimony build succeed and the binary runs.
#
# Usage: ./run.sh [sample.pas ...]   (default: all samples)

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
NIMONY="${NIMONY:-/home/adrian/dev/nimony/bin/nimony}"

if [ ! -x "$ROOT/bin/pas2nimony" ]; then
  echo "bin/pas2nimony missing - run ./build.sh first" >&2
  exit 1
fi

TMP="$HERE/tmp"
mkdir -p "$TMP"
# the runtime shim units must be importable from the generated modules.
# The placeholder stubs (Delphi units we do not shim: Forms, Graphics, ...)
# go alongside them too: a Pascal `uses Forms` becomes `import Forms`, and
# the placeholders directory is deliberately not on --path, so the module
# has to be visible where the .nim anchor is compiled from. That directory
# is generated and gitignored (make-placeholders.sh) - its file names are
# unit names of the closed-source trees, so absence is fine here: the public
# samples only `uses` units the runtime itself provides.
cp -f "$ROOT"/runtime/*.nim "$TMP/"
cp -f "$ROOT"/runtime/placeholders/*.nim "$TMP/" 2>/dev/null || true

if [ $# -gt 0 ]; then
  SAMPLES=("$@")
else
  SAMPLES=("$HERE"/*.pas)
fi

fail=0

# Run pasler in a directory, echo its output without the nifmake noise, and
# return PASLER'S status. The old shape piped pasler through `grep ... || true`,
# so every pasler build failure exited 0 and the runner printed "-- ok": the
# exact false positive this helper removes.
pasler_run() {
  local dir="$1"; shift
  local log="$TMP/pasler-run.log"
  ( cd "$dir" && "$ROOT/bin/pasler" --nimony:"$NIMONY" "$@" ) > "$log" 2>&1
  local st=$?
  grep -v nifmake "$log" || true
  rm -f "$log"
  return $st
}

for pas in "${SAMPLES[@]}"; do
  name="$(basename "$pas" .pas)"
  echo "== $name"
  if ! "$ROOT/bin/pas2nimony" "$pas" -o:"$TMP/$name.nim"; then
    echo "   TRANSLATE FAILED"
    fail=1
    continue
  fi
  # capture the backend's REAL exit status; piping through grep (with the
  # customary `|| true`) hides it, turning a build failure into a later
  # "binary not found" or a spurious "-- ok"
  (cd "$TMP" && "$NIMONY" c --path:. "$name.nim") > "$TMP/$name.build.log" 2>&1
  build_st=$?
  grep -v nifmake "$TMP/$name.build.log" || true
  if [ "$build_st" -ne 0 ]; then
    echo "   NIMONY BUILD FAILED"
    fail=1
    continue
  fi
  bin="$(ls "$TMP"/nimcache/*/"$name" 2>/dev/null | head -1)"
  if [ -z "$bin" ]; then
    echo "   BINARY NOT FOUND"
    fail=1
    continue
  fi
  echo "-- output:"
  "$bin"
  echo "-- ok"
done

# ---- negative samples: the translator must FAIL, with a named reason ----
# A `goto` into a deeper block cannot be lowered to Nim. pas2nimony has to
# say so; emitting broken Nim or hanging is the failure this guards.
if [ $# -eq 0 ] && [ -f "$HERE/negative/goto_nested.pas" ]; then
  echo "== negative-goto-nested"
  neglog="$TMP/negative-goto-nested.log"
  if "$ROOT/bin/pas2nimony" "$HERE/negative/goto_nested.pas" \
      -o:"$TMP/negative_goto_nested.nim" > "$neglog" 2>&1; then
    echo "   GOTO NESTED WAS ACCEPTED (want a refusal)"; fail=1
  elif ! grep -q "jumping into a nested block" "$neglog"; then
    echo "   GOTO NESTED FAILED FOR THE WRONG REASON"
    sed 's/^/   /' "$neglog" | head -3
    fail=1
  else
    echo "-- ok (refused: $(grep -m1 -o 'jumping into a nested block' "$neglog"))"
  fi
fi

# A real `{$I file}` whose file does not exist must be refused: silently
# skipping it drops the included declarations, and the mistake only shows
# up later as an undeclared identifier on the first use.
if [ $# -eq 0 ] && [ -f "$HERE/negative/missing_include.pas" ]; then
  echo "== negative-missing-include"
  neglog="$TMP/negative-missing-include.log"
  if "$ROOT/bin/pas2nimony" "$HERE/negative/missing_include.pas" \
      -o:"$TMP/negative_missing_include.nim" > "$neglog" 2>&1; then
    echo "   MISSING INCLUDE WAS ACCEPTED (want a refusal)"; fail=1
  elif ! grep -q "include file not found" "$neglog"; then
    echo "   MISSING INCLUDE FAILED FOR THE WRONG REASON"
    sed 's/^/   /' "$neglog" | head -3
    fail=1
  else
    echo "-- ok (refused: $(grep -m1 -o 'include file not found: [^ ]*' "$neglog"))"
  fi
fi

# ---- multi-unit project: counter unit + program using it ----
if [ $# -eq 0 ] && [ -d "$HERE/twounit" ]; then
  echo "== twounit"
  mkdir -p "$TMP/twounit"
  for pas in "$HERE"/twounit/*.pas; do
    n="$(basename "$pas" .pas)"
    "$ROOT/bin/pas2nimony" "$pas" -o:"$TMP/twounit/$n.nim" || {
      echo "   TRANSLATE FAILED"; fail=1; continue; }
  done
  (cd "$TMP/twounit" && cp "$ROOT"/runtime/*.nim . &&
      "$NIMONY" c --path:. usecounter.nim) > "$TMP/twounit.build.log" 2>&1
  build_st=$?
  grep -v nifmake "$TMP/twounit.build.log" || true
  if [ "$build_st" -eq 0 ]; then
    bin="$(find "$TMP/twounit/nimcache" -name usecounter -type f | head -1)"
    if [ -n "$bin" ]; then
      echo "-- output:"
      "$bin"
      echo "-- ok"
    else
      echo "   BINARY NOT FOUND"; fail=1
    fi
  else
    echo "   NIMONY BUILD FAILED"; fail=1
  fi
fi

# ---- pasler: the .p.nif front-end path (no nifler for Pascal sources) ----
if [ $# -eq 0 ] && [ -x "$ROOT/bin/pasler" ] && [ -d "$HERE/twounit" ]; then
  echo "== pasler-twounit"
  mkdir -p "$TMP/pasler"
  cp "$HERE"/twounit/*.pas "$TMP/pasler/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler/"
  rm -f "$TMP/pasler"/nimcache/*.nim
  if pasler_run "$TMP/pasler" --run usecounter.pas; then
    # nifler must not have touched the Pascal-derived NIFs
    if grep -lq 'vendor "pasler"' "$TMP/pasler"/nimcache/*.p.nif 2>/dev/null; then
      echo "-- ok"
    else
      echo "   NIF VENDOR CHECK FAILED"; fail=1
    fi
  else
    echo "   PASLER BUILD FAILED"; fail=1
  fi
  # the NIF front-end must agree with the .nim path on semantics
  if [ -f "$HERE/events.pas" ]; then
    echo "== pasler-events"
    mkdir -p "$TMP/pasler-events"
    cp "$HERE/events.pas" "$TMP/pasler-events/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-events/"
    rm -f "$TMP/pasler-events"/nimcache/*.nim
    if pasler_run "$TMP/pasler-events" --run events.pas; then
      echo "-- ok"
    else
      echo "   PASLER EVENTS FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/anonmeth.pas" ]; then
    echo "== pasler-anon"
    mkdir -p "$TMP/pasler-anon"
    cp "$HERE/anonmeth.pas" "$TMP/pasler-anon/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-anon/"
    rm -f "$TMP/pasler-anon"/nimcache/*.nim
    if pasler_run "$TMP/pasler-anon" --run anonmeth.pas; then
      echo "-- ok"
    else
      echo "   PASLER ANON FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/usesnim.pas" ]; then
    echo "== pasler-usesnim"
    mkdir -p "$TMP/pasler-usesnim"
    cp "$HERE/usesnim.pas" "$TMP/pasler-usesnim/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-usesnim/"
    rm -f "$TMP/pasler-usesnim"/nimcache/*.nim
    if pasler_run "$TMP/pasler-usesnim" --run usesnim.pas; then
      echo "-- ok"
    else
      echo "   PASLER USESNIM FAILED"; fail=1
    fi
  fi
  for extra in shims datetime sysutils missing \
               absolute bodywhen ifexpr condfield dirlabel iftypesec inctest \
               localif msgwhen uscore dotparent inharith uscmethod \
               addrparam multidim mathfn withalias withptr sysmem; do
    if [ -f "$HERE/$extra.pas" ]; then
      echo "== pasler-$extra"
      mkdir -p "$TMP/pasler-$extra"
      cp "$HERE/$extra.pas" "$TMP/pasler-$extra/"
      # a sample may pull in .inc files (see inctest.pas); they are
      # resolved next to the source that includes them
      cp -f "$HERE"/*.inc "$TMP/pasler-$extra/" 2>/dev/null || true
      # pasler resolves shim imports from its own directory; the
      # nimcache's stale module copies must not shadow them
      cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-$extra/"
      rm -f "$TMP/pasler-$extra"/nimcache/*.nim
      if pasler_run "$TMP/pasler-$extra" --run $extra.pas; then
        echo "-- ok"
      else
        echo "   PASLER $extra FAILED"; fail=1
      fi
    fi
  done
  if [ -f "$HERE/generics.pas" ]; then
    echo "== pasler-generics"
    mkdir -p "$TMP/pasler-generics"
    cp "$HERE/generics.pas" "$TMP/pasler-generics/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-generics/"
    rm -f "$TMP/pasler-generics"/nimcache/*.nim
    if pasler_run "$TMP/pasler-generics" --run generics.pas; then
      echo "-- ok"
    else
      echo "   PASLER GENERICS FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/opover.pas" ]; then
    echo "== pasler-op"
    mkdir -p "$TMP/pasler-op"
    cp "$HERE/opover.pas" "$TMP/pasler-op/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-op/"
    rm -f "$TMP/pasler-op"/nimcache/*.nim
    if pasler_run "$TMP/pasler-op" --run opover.pas; then
      echo "-- ok"
    else
      echo "   PASLER OP FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/classmeth.pas" ]; then
    echo "== pasler-cm"
    mkdir -p "$TMP/pasler-cm"
    cp "$HERE/classmeth.pas" "$TMP/pasler-cm/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-cm/"
    rm -f "$TMP/pasler-cm"/nimcache/*.nim
    if pasler_run "$TMP/pasler-cm" --run classmeth.pas; then
      echo "-- ok"
    else
      echo "   PASLER CM FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/intf.pas" ]; then
    echo "== pasler-intf"
    mkdir -p "$TMP/pasler-intf"
    cp "$HERE/intf.pas" "$TMP/pasler-intf/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-intf/"
    rm -f "$TMP/pasler-intf"/nimcache/*.nim
    if pasler_run "$TMP/pasler-intf" --run intf.pas; then
      echo "-- ok"
    else
      echo "   PASLER INTF FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/cast.pas" ]; then
    echo "== pasler-cast"
    mkdir -p "$TMP/pasler-cast"
    cp "$HERE/cast.pas" "$TMP/pasler-cast/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-cast/"
    rm -f "$TMP/pasler-cast"/nimcache/*.nim
    if pasler_run "$TMP/pasler-cast" --run cast.pas; then
      echo "-- ok"
    else
      echo "   PASLER CAST FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/goto.pas" ]; then
    echo "== pasler-goto"
    mkdir -p "$TMP/pasler-goto"
    cp "$HERE/goto.pas" "$TMP/pasler-goto/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-goto/"
    rm -f "$TMP/pasler-goto"/nimcache/*.nim
    if pasler_run "$TMP/pasler-goto" --run goto.pas; then
      echo "-- ok"
    else
      echo "   PASLER GOTO FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/with.pas" ]; then
    echo "== pasler-with"
    mkdir -p "$TMP/pasler-with"
    cp "$HERE/with.pas" "$TMP/pasler-with/"
    # pasler resolves shim imports from its own directory
    cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-with/"
    rm -f "$TMP/pasler-with"/nimcache/*.nim
    if pasler_run "$TMP/pasler-with" --run with.pas; then
      echo "-- ok"
    else
      echo "   PASLER WITH FAILED"; fail=1
    fi
  fi
fi

# ---- pasler CLI: nimony-compatible commands, -d: defines, --path: ----
if [ -x "$ROOT/bin/pasler" ] && [ -d "$HERE/clitest" ]; then
  echo "== pasler-cli"
  mkdir -p "$TMP/pasler-cli"
  cp "$HERE"/clitest/*.pas "$TMP/pasler-cli/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-cli/"
  rm -f "$TMP/pasler-cli"/nimcache/*.nim
  cp -r "$HERE"/clitest/units "$TMP/pasler-cli/units"
  out="$(cd "$TMP/pasler-cli" && timeout 300 "$ROOT/bin/pasler" \
      --nimony:"$NIMONY" --path:units -d:CLI_FLAG --run cond.pas 2>&1 |
      grep -v nifmake || true)"
  echo "$out"
  if [ "$out" = "3235
local on" ]; then
    echo "-- ok"
  else
    echo "   CLI DEFINE RUN MISMATCH (want 3235/local on)"; fail=1
  fi
  out2="$(cd "$TMP/pasler-cli" && rm -rf nimcache && timeout 300 \
      "$ROOT/bin/pasler" --path:units --run cond.pas 2>&1 |
      grep -v nifmake)"
  if [ "$out2" = "3236
local on" ]; then
    echo "-- ok2"
  else
    echo "   CLI NO-DEFINE RUN MISMATCH (want 3236/local on)"; fail=1
  fi
  if (cd "$TMP/pasler-cli" && timeout 300 "$ROOT/bin/pasler" check \
      --path:units -d:CLI_FLAG cond.pas > /dev/null 2>&1); then
    echo "-- ok3 (check)"
  else
    echo "   CLI CHECK FAILED"; fail=1
  fi
fi

# ---- happy.pas: real-world sample (nested loops, set membership,
# paren-less calls, DateUtils) ----
if [ -x "$ROOT/bin/pasler" ] && [ -f "$HERE/happy.pas" ]; then
  echo "== pasler-happy"
  mkdir -p "$TMP/pasler-happy"
  cp "$HERE/happy.pas" "$TMP/pasler-happy/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-happy/"
  rm -f "$TMP/pasler-happy"/nimcache/*.nim
  out="$(cd "$TMP/pasler-happy" && timeout 300 "$ROOT/bin/pasler" \
      --nimony:"$NIMONY" --run happy.pas 2>&1 | grep -v nifmake |
      grep 'Found' || true)"
  echo "$out"
  if [ "$out" = "Found 4816030 tickets. Elapsed time, msec: 0" ] ||
     echo "$out" | grep -q "Found 4816030 tickets"; then
    echo "-- ok"
  else
    echo "   HAPPY TICKET COUNT MISMATCH"; fail=1
  fi
fi

# ---- shlshr.pas: real-world sample (shift/div semantics, widths,
# Format, GetTickCount timing) - expected output verified against
# FPC 3.2.2 (-Mdelphi); the timing line is filtered
if [ -x "$ROOT/bin/pasler" ] && [ -f "$HERE/shlshr.pas" ]; then
  echo "== pasler-shlshr"
  mkdir -p "$TMP/pasler-shlshr"
  cp "$HERE/shlshr.pas" "$TMP/pasler-shlshr/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-shlshr/"
  rm -f "$TMP/pasler-shlshr"/nimcache/*.nim
  timeout 300 bash -c "cd '$TMP/pasler-shlshr' &&
      '$ROOT/bin/pasler' --nimony:'$NIMONY' --run shlshr.pas 2>&1 |
      grep -v nifmake | grep -v 'time = ' > shlshr.out" || true
  if diff -q "$HERE/shlshr.expected" "$TMP/pasler-shlshr/shlshr.out" \
      > /dev/null 2>&1; then
    echo "-- ok"
  else
    echo "   SHLSHR OUTPUT MISMATCH"; fail=1
  fi
fi

# ---- ccprobe.pas: directive forwarding (inline/cdecl/stdcall as
# pragmas; register/pascal/safecall warn, error under --strict) ----
if [ -x "$ROOT/bin/pasler" ] && [ -f "$HERE/ccprobe.pas" ]; then
  echo "== pasler-cc"
  mkdir -p "$TMP/pasler-cc"
  cp "$HERE/ccprobe.pas" "$TMP/pasler-cc/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-cc/"
  rm -f "$TMP/pasler-cc"/nimcache/*.nim
  out="$(cd "$TMP/pasler-cc" && timeout 300 "$ROOT/bin/pasler" \
      --nimony:"$NIMONY" --run ccprobe.pas 2>&1 | grep -v nifmake)"
  echo "$out" | tail -1
  warncount="$(echo "$out" | grep -c "Warning: calling convention" || true)"
  if echo "$out" | grep -q "2468" && [ "$warncount" = "1" ]; then
    echo "-- ok"
  else
    echo "   CC WARNING/RUN MISMATCH (want one warning + 2468)"; fail=1
  fi
  rm -rf "$TMP/pasler-cc/nimcache"
  if (cd "$TMP/pasler-cc" && timeout 300 "$ROOT/bin/pasler" --strict \
      --run ccprobe.pas > /dev/null 2>&1); then
    echo "   STRICT MODE DID NOT FAIL"; fail=1
  elif (cd "$TMP/pasler-cc" && timeout 300 "$ROOT/bin/pasler" --strict \
      --run ccprobe.pas 2>&1 | grep -q "Error: calling convention"); then
    echo "-- ok2 (strict errors)"
  else
    echo "   STRICT MODE MISMATCH"; fail=1
  fi
fi

# ---- shadow.pas: method hiding semantics (reintroduce vs override;
# a descendant virtual over an ancestor method is a NEW slot, a
# base-typed reference keeps dispatching to the ancestor) ----
if [ -x "$ROOT/bin/pasler" ] && [ -f "$HERE/shadow.pas" ]; then
  echo "== pasler-shadow"
  mkdir -p "$TMP/pasler-shadow"
  cp "$HERE/shadow.pas" "$TMP/pasler-shadow/"
  # pasler resolves shim imports from its own directory
  cp -f "$ROOT"/runtime/*.nim "$TMP/pasler-shadow/"
  rm -f "$TMP/pasler-shadow"/nimcache/*.nim
  out="$(cd "$TMP/pasler-shadow" && timeout 300 "$ROOT/bin/pasler" \
      --nimony:"$NIMONY" --run shadow.pas 2>&1 | grep -v nifmake)"
  echo "$out" | head -3
  if echo "$out" | grep -q "child-newslot" && echo "$out" | grep -q "^base$" \
      && echo "$out" | grep -q "child2"; then
    echo "-- ok"
  else
    echo "   SHADOW DISPATCH MISMATCH (want child-newslot, base, child2)"
    fail=1
  fi
fi

# ---- private corpus sweep (closed-source compiler food; skipped
# when test/private/corpus is absent) - reports parse health, only
# a missing sweep counts as failure ----
if [ -f "$HERE/private/run.sh" ]; then
  echo "== private-corpus"
  sweep="$("$HERE/private/run.sh" 2>&1)"
  okcount="$(echo "$sweep" | grep -c '=> OK' || true)"
  errcount="$(echo "$sweep" | grep -c '=> corpus/' || true)"
  echo "   private corpus: $okcount ok, $errcount with parse errors"
  if [ "$okcount" -gt 0 ]; then
    echo "-- ok"
  else
    echo "   PRIVATE SWEEP BROKEN (no unit translates)"; fail=1
  fi
fi

# M5 oracle: differential testing against real FPC (skips without fpc)
if ! sh "$HERE/oracle.sh"; then
  echo "   ORACLE FAILED"; fail=1
fi

# AnsiString conformance tier: direct model test under nimony plus the
# FPC/Delphi differential over test/ansi/*.pas. The shim leg is the gate
# that must stay green before the ansistring -> string mapping is split.
if ! sh "$HERE/ansi/ansi-oracle.sh"; then
  echo "   ANSI ORACLE FAILED"; fail=1
fi

# The same tier on the toolchain these sources actually target: every leg a
# Windows binary in the wine win64 prefix (FPC 3.2.2 x86_64-win64, Delphi
# 2007 dcc32, our own bin/pasler.exe). Skips when the prefix or the Windows
# compilers are absent (build them with build-win64.sh).
if ! sh "$HERE/ansi/win64-oracle.sh"; then
  echo "   WIN64 ORACLE FAILED"; fail=1
fi

# Variant conformance tier: the shim-level checks (vshim.nim) plus the
# FPC/Delphi differential over test/variant/*.pas, on the Windows toolchain
# as well - Delphi 2007 and FPC 3.2.2 x86_64-win64 are the targets whose
# Variant semantics the shim models.
if ! sh "$HERE/variant/win64-oracle.sh"; then
  echo "   WIN64 VARIANT ORACLE FAILED"; fail=1
fi

# keep the generated artifacts for inspection, but drop the build cache
rm -rf "$TMP/nimcache"
exit $fail