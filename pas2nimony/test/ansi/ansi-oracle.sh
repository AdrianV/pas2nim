#!/bin/sh
# AnsiString conformance tier - three-way differential testing.
#
# Every sample under test/ansi/ is compiled and run by
#   1. real FPC                (-Mdelphi, Linux x86-64)
#   2. real Delphi 2007 dcc32  (Win32, under wine)
#   3. our chain               (bin/pasler --run)
# and the outputs are compared. Delphi and FPC disagree in places, so a
# sample may carry <name>.expect naming the oracle we follow ("delphi" or
# "fpc"); the other leg is then reported as INFO instead of FAIL.
#
# The Pascal front end still maps ansistring onto nimony's string, so the
# .pas samples pin the *observable* AnsiString semantics the new model must
# preserve - they become the mapping-split gate. test/ansi/ashim.nim checks
# the model directly under nimony (no Pascal front end) and is the gate
# that runs today.
#
# Findings-only where the two oracles themselves disagree; the ours-vs-fpc
# and shim verdicts gate. Skips gracefully when fpc/dcc32 are missing.
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT="$HERE/../.."
NIMONY="${NIMONY:-$ROOT/../../nimony/bin/nimony}"

DELPHI_PREFIX="${DELPHI_PREFIX:-$ROOT/../.wine/delphi2007}"
WINE="${DELPHI_WINE:-$HOME/.PlayOnLinux/wine/linux-x86/7.11/bin/wine}"
DCC32="${DELPHI_DCC32:-$DELPHI_PREFIX/drive_c/Program Files/CodeGear/RAD Studio/5.0/bin/DCC32.EXE}"
export WINEPREFIX="$DELPHI_PREFIX"
export WINEDEBUG=-all
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-/tmp/xdg-cache}"

have_fpc=0
command -v fpc >/dev/null 2>&1 && have_fpc=1
have_delphi=0
[ -x "$WINE" ] && [ -f "$DCC32" ] && have_delphi=1

fail=0

if [ "$have_fpc" = 0 ] && [ "$have_delphi" = 0 ]; then
  echo "ansi-oracle: neither fpc nor wine+dcc32 available - skipped"
  exit 0
fi

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

total=0
for pas in "$HERE"/*.pas; do
  name=$(basename "$pas" .pas)
  total=$((total + 1))
  w="$WORK/$name"
  mkdir -p "$w"
  cp "$HERE/$name.pas" "$w/"
  expect=""
  [ -f "$HERE/$name.expect" ] && expect=$(tr -d ' \r\n' < "$HERE/$name.expect")

  if [ "$have_fpc" = 1 ]; then
    (cd "$w" && fpc -Mdelphi -o"$name.fpc" "$name.pas" >fpc.log 2>&1 &&
      "./$name.fpc" >fpc.out 2>&1) || echo "   (fpc leg failed for $name)"
  else
    : > "$w/fpc.out"
  fi
  if [ "$have_delphi" = 1 ]; then
    (cd "$w" && "$WINE" "$DCC32" -B -Q "$name.pas" >dcc.log 2>&1)
    if [ -f "$w/$name.exe" ]; then
      (cd "$w" && "$WINE" "./$name.exe" 2>/dev/null | tr -d '\r' >delphi.out)
    else
      : > "$w/delphi.out"
      echo "   (dcc32 leg failed for $name)"
      tr -d '\r' < "$w/dcc.log" | grep -iE "fehler|error" | head -3 | sed 's/^/   /'
    fi
  else
    : > "$w/delphi.out"
  fi
  (cd "$w" && "$ROOT/bin/pasler" --nimony:"$NIMONY" --run       -d:MSWINDOWS -d:WIN32 -d:WINDOWS "$name.pas" >ours.out 2>&1)
  tr -d '\r' < "$w/ours.out" > "$w/ours.norm"

  echo "== $name"
  if [ "$have_fpc" = 1 ] && [ "$have_delphi" = 1 ]; then
    n=$(diff "$w/delphi.out" "$w/fpc.out" | grep -c '^[<>]')
    echo "   oracles: $n differing line(s)$([ -n "$expect" ] && echo " (we follow: $expect)")"
  fi
  if diff -u "$w/fpc.out" "$w/ours.norm" > "$w/d.fpc" 2>&1; then
    echo "   ours-vs-fpc: PASS"
  elif [ "$expect" = "delphi" ] || [ "$expect" = "none" ]; then
    echo "   ours-vs-fpc: INFO"
  else
    echo "   ours-vs-fpc: FAIL"
    sed 's/^/      /' "$w/d.fpc" | head -8
    fail=1
  fi
  if [ "$have_delphi" = 1 ]; then
    if diff -u "$w/delphi.out" "$w/ours.norm" > "$w/d.delphi" 2>&1; then
      echo "   ours-vs-delphi: PASS"
    elif [ "$expect" = "fpc" ] || [ "$expect" = "none" ]; then
      echo "   ours-vs-delphi: INFO"
    else
      echo "   ours-vs-delphi: FAIL"
      sed 's/^/      /' "$w/d.delphi" | head -8
    fi
  fi
done

# --- shim-level conformance -------------------------------------------------
# ashim.nim checks the model directly under nimony, with no Pascal front end
# involved. It is the gate that can pass before the mapping split.
echo "== shim (ansi/ashim.nim)"
if [ -x "$NIMONY" ]; then
  sw="$WORK/shim"
  mkdir -p "$sw"
  cp "$HERE/ashim.nim" "$sw/"
  if (cd "$sw" && "$NIMONY" c --path:"$ROOT/runtime" -o:ashim ashim.nim >shim.log 2>&1); then
    if [ -x "$sw/ashim" ]; then
      shout=$("$sw/ashim" 2>&1)
      okc=$(printf '%s\n' "$shout" | grep -c '^ok')
      if printf '%s\n' "$shout" | grep -q 'ALL OK'; then
        echo "   shim: PASS ($okc checks)"
      else
        echo "   shim: FAIL"
        printf '%s\n' "$shout" | grep '^FAIL' | sed 's/^/      /' | head -10
        echo "      ($okc checks passed)"
        fail=1
      fi
    else
      echo "   shim: no binary produced"
      tail -3 "$sw/shim.log" | sed 's/^/      /'
      fail=1
    fi
  else
    echo "   shim: COMPILE FAILED"
    grep -v nifmake "$sw/shim.log" | head -5 | sed 's/^/      /'
    fail=1
  fi
else
  echo "   shim: nimony not found at $NIMONY"
fi

echo "-- ansi-oracle: $total samples"
[ "$fail" -eq 0 ] || exit 1
exit 0
