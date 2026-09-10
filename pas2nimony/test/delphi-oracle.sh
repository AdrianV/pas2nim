#!/bin/sh
# M11 delphi oracle - differential testing against real Delphi 2007.
#
# Every sample under test/oracle/ is compiled and run by dcc32.exe
# (Delphi 2007, Win32) under wine and by pasler; the outputs must be
# byte-identical after stripping the CRLF the Windows console emits.
# dcc32 compiles with the genuine Delphi RTL, so its output is the
# ground truth for the Delphi dialect.
#
# Skips gracefully when wine, the wineprefix or dcc32.exe are missing.
# FAILs are census findings (the chains are verified against FPC in
# oracle.sh; Delphi-vs-ours divergences get fixed in later rounds), so
# this census never gates the suite.
#
# Environment overrides:
#   DELPHI_WINE      wine binary (default: PlayOnLinux 32-bit wine 7.11)
#   DELPHI_PREFIX    WINEPREFIX to use (default: the delphi2007 prefix)
#   DELPHI_DCC32     path to DCC32.EXE (default: inside DELPHI_PREFIX)
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

if [ ! -x "$WINE" ] || [ ! -f "$DCC32" ]; then
  echo "delphi-oracle: wine or dcc32.exe not available - skipped"
  exit 0
fi

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

fail=0
total=0
for pas in "$HERE"/oracle/*.pas; do
  name=$(basename "$pas" .pas)
  total=$((total + 1))
  w="$WORK/$name"
  mkdir -p "$w"
  cp "$pas" "$w/"
  # dcc32 writes .dcu/.drf/.exe into its working directory; the DOS cwd
  # is this scratch dir (mapped through the z: drive)
  (cd "$w" && "$WINE" "$DCC32" -B -Q "$name.pas" >dcc.log 2>&1)
  if [ ! -f "$w/$name.exe" ]; then
    echo "FAIL $name (dcc32 compile)"
    tr -d '\r' < "$w/dcc.log" | grep -v Fontconfig | head -4 | sed 's/^/   /'
    fail=1
    continue
  fi
  (cd "$w" && "$WINE" "./$name.exe" >run.raw 2>/dev/null)
  tr -d '\r' < "$w/run.raw" > "$w/dcc32.out"
  # The Delphi contrast leg models the Windows deployment of our chain:
  # the shims compile with the MSW tier's defines so pasLineEnding is
  # CRLF (Delphi's Windows RTL), while oracle.sh's FPC leg stays LF
  (cd "$w" && "$ROOT/bin/pasler" --nimony:"$NIMONY" --run \
      -d:MSWINDOWS -d:WIN32 -d:WINDOWS "$name.pas" >pasler.out 2>&1)
  if diff -u "$w/dcc32.out" "$w/pasler.out" > "$w/diff.txt" 2>&1; then
    echo "PASS $name"
  else
    echo "FAIL $name"
    sed 's/^/   /' "$w/diff.txt" | head -10
    fail=1
  fi
done
echo "-- delphi-oracle: $total samples"
# findings only: a non-zero exit would gate the suite, so report but
# always succeed when the harness itself ran to the end
[ "$fail" -eq 0 ] || true
exit 0