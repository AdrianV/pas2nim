#!/bin/sh
# Variant conformance tier - three-way differential testing.
#
# Every sample under test/variant/ is compiled and run by
#   1. real FPC                (-Mdelphi, Linux x86-64)
#   2. real Delphi 2007 dcc32  (Win32, under wine)
#   3. our chain            (bin/pasler --run, shim + nimony)
# and the three outputs are compared. Delphi and FPC genuinely disagree
# in places (literal storage types, WideChar, UInt64, error classes,
# locale formatting), so the report has three verdicts per sample:
#
#   ours-vs-fpc / ours-vs-delphi : conformance (must eventually PASS)
#   fpc-vs-delphi                : how much the oracles themselves differ;
#                                  when they differ, our output can match
#                                  only one of them, and the sample should
#                                  say which (see <name>.expect below)
#
# A sample may carry `<name>.expect` naming the oracle we follow
# ("delphi" or "fpc"); the other leg is then reported as INFO instead of
# FAIL. Samples must be deterministic: pin DecimalSeparator, avoid
# Now/Random and locale-dependent date formatting, and prefer reading
# payloads through TVarData over VarToStr for Currency/Double.
#
# Findings only - this tier never gates the suite while the semantics are
# still being pinned down (same philosophy as delphi-oracle.sh).
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT="$HERE/.."
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

if [ "$have_fpc" = 0 ] && [ "$have_delphi" = 0 ]; then
  echo "variant-oracle: neither fpc nor wine+dcc32 available - skipped"
  exit 0
fi

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

total=0
for pas in "$HERE"/variant/*.pas; do
  name=$(basename "$pas" .pas)
  total=$((total + 1))
  w="$WORK/$name"
  mkdir -p "$w"
  cp "$HERE/variant/$name.pas" "$w/"
  expect=""
  [ -f "$HERE/variant/$name.expect" ] && expect=$(tr -d ' \r\n' < "$HERE/variant/$name.expect")

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
  (cd "$w" && "$ROOT/bin/pasler" --nimony:"$NIMONY" --run \
      -d:MSWINDOWS -d:WIN32 -d:WINDOWS "$name.pas" >ours.out 2>&1)
  tr -d '\r' < "$w/ours.out" > "$w/ours.norm"

  echo "== $name"
  # how much do the two oracles disagree?
  if [ "$have_fpc" = 1 ] && [ "$have_delphi" = 1 ]; then
    n=$(diff "$w/delphi.out" "$w/fpc.out" | grep -c '^[<>]')
    echo "   oracles: $n differing line(s)$([ -n "$expect" ] && echo " (we follow: $expect)")"
  fi
  if diff -u "$w/fpc.out" "$w/ours.norm" > "$w/d.fpc" 2>&1; then
    echo "   ours-vs-fpc: PASS"
  elif [ "$expect" = "delphi" ] || [ "$expect" = "none" ]; then
    echo "   ours-vs-fpc: INFO ($([ "$expect" = "none" ] && echo "informational sample" || echo "we follow delphi"))"
  else
    echo "   ours-vs-fpc: FAIL"
    sed 's/^/      /' "$w/d.fpc" | head -8
  fi
  if diff -u "$w/delphi.out" "$w/ours.norm" > "$w/d.delphi" 2>&1; then
    echo "   ours-vs-delphi: PASS"
  elif [ "$expect" = "fpc" ] || [ "$expect" = "none" ]; then
    echo "   ours-vs-delphi: INFO ($([ "$expect" = "none" ] && echo "informational sample" || echo "we follow fpc"))"
  else
    echo "   ours-vs-delphi: FAIL"
    sed 's/^/      /' "$w/d.delphi" | head -8
  fi
done

# --- shim-level conformance -------------------------------------------------
# vshim.nim checks the shim's measured semantics directly under nimony, with
# no Pascal front end involved. It is the leg that can pass while the
# Pascal-side probes still need the emitter to build Variant values.
echo "== shim (variant/vshim.nim)"
if [ -x "$NIMONY" ]; then
  sw="$WORK/shim"
  mkdir -p "$sw"
  cp "$HERE/variant/vshim.nim" "$sw/"
  if (cd "$sw" && "$NIMONY" c --path:"$ROOT/runtime" vshim.nim >shim.log 2>&1); then
    shimbin=$(ls "$sw"/nimcache/*/vshim 2>/dev/null | head -1)
    if [ -n "$shimbin" ] && [ -x "$shimbin" ]; then
      shout=$("$shimbin" 2>&1)
      okc=$(printf '%s\n' "$shout" | grep -c '^ok')
      if printf '%s\n' "$shout" | grep -q 'ALL OK'; then
        echo "   shim: PASS ($okc checks)"
      else
        echo "   shim: FAIL"
        printf '%s\n' "$shout" | grep '^FAIL' | sed 's/^/      /' | head -10
        echo "      ($okc checks passed)"
      fi
    else
      echo "   shim: no binary produced"
      tail -3 "$sw/shim.log" | sed 's/^/      /'
    fi
  else
    echo "   shim: COMPILE FAILED"
    grep -v nifmake "$sw/shim.log" | head -5 | sed 's/^/      /'
  fi
else
  echo "   shim: nimony not found at $NIMONY"
fi

echo "-- variant-oracle: $total samples"
exit 0
