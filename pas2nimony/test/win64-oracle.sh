#!/bin/sh
# Windows-toolchain differential oracle engine.
#
# Every leg is a Windows binary running in the wine win64 prefix
# (./.wine/win64), which is where the real targets live:
#   1. real FPC 3.2.2           (x86_64-win64, c:\lazarus\fpc\3.2.2)
#   2. real Delphi 2007 dcc32   (Win32, CodeGear RAD Studio 5.0)
#   3. our chain                (bin/pasler.exe, built by build-win64.sh)
# plus the shim leg: the tier's own vshim/ashim.nim compiled by the prefix's
# nimony.exe, which is the gate that can pass before the Pascal front end is
# involved.
#
# Usage: win64-oracle.sh <tier> <sample-dir> <shim.nim>
# Thin per-tier wrappers live next to the samples (test/ansi/win64-oracle.sh,
# test/variant/win64-oracle.sh).
#
# The work directory is test/tmp/win64/<tier> (NOT mktemp): the prefix maps the
# repository as e:, so this directory is `e:\test\tmp\win64\<tier>` and every
# leg can be run with wine's cwd inside the checkout. A path under /tmp is
# invisible to wine (the prefix has no z: drive).
#
# A sample may carry <name>.expect naming the oracle to follow ("delphi",
# "fpc" or "none") where the two oracles disagree. Skips gracefully when
# wine, the prefix or bin/pasler.exe are missing.
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT="$(cd "$HERE/.." && pwd)"
REPO="$(cd "$ROOT/.." && pwd)"

TIER="${1:?usage: win64-oracle.sh <tier> <sample-dir> <shim.nim>}"
TIERDIR="${2:?usage: win64-oracle.sh <tier> <sample-dir> <shim.nim>}"
SHIM="${3:?usage: win64-oracle.sh <tier> <sample-dir> <shim.nim>}"

WINEPREFIX="${WINEPREFIX:-$REPO/.wine/win64}"
export WINEPREFIX
export WINEDEBUG="${WINEDEBUG:--all}"
WINE="${WINE:-wine}"
FPC_WIN="${FPC_WIN:-c:\\lazarus\\fpc\\3.2.2\\bin\\x86_64-win64\\fpc.exe}"
DCC32_WIN="${DCC32_WIN:-c:\\program files (x86)\\codegear\\rad studio\\5.0\\bin\\dcc32.exe}"
NIMONY_WIN="${NIMONY_WIN:-c:\\nimony\\bin\\nimony.exe}"
PASLER="$ROOT/bin/pasler.exe"
RUNTIME_WIN='e:\runtime'

if ! command -v "$WINE" >/dev/null 2>&1; then
  echo "win64-oracle[$TIER]: wine not found - skipped"
  exit 0
fi
if [ ! -d "$WINEPREFIX/drive_c" ]; then
  echo "win64-oracle[$TIER]: no wine prefix at $WINEPREFIX - skipped"
  exit 0
fi
# one wine invocation per tool tells us whether the toolset is there at all
have_fpc=0
"$WINE" "$FPC_WIN" -iV >/dev/null 2>&1 && have_fpc=1
have_delphi=0
"$WINE" "$DCC32_WIN" >/dev/null 2>&1 && have_delphi=1
have_ours=0
[ -f "$PASLER" ] && have_ours=1

if [ "$have_ours" = 0 ]; then
  echo "win64-oracle[$TIER]: $PASLER missing (run build-win64.sh) - skipped"
  exit 0
fi
if [ "$have_fpc" = 0 ] && [ "$have_delphi" = 0 ]; then
  echo "win64-oracle[$TIER]: neither fpc nor dcc32 usable in $WINEPREFIX - skipped"
  exit 0
fi

WORK="$ROOT/test/tmp/win64/$TIER"
rm -rf "$WORK"
mkdir -p "$WORK"
fail=0
total=0

for pas in "$TIERDIR"/*.pas; do
  name=$(basename "$pas" .pas)
  total=$((total + 1))
  w="$WORK/$name"
  mkdir -p "$w"
  cp "$TIERDIR/$name.pas" "$w/"
  expect=""
  [ -f "$TIERDIR/$name.expect" ] && expect=$(tr -d ' \r\n' < "$TIERDIR/$name.expect")

  # --- 1. FPC x86_64-win64 ------------------------------------------------
  if [ "$have_fpc" = 1 ]; then
    (cd "$w" && "$WINE" "$FPC_WIN" -Mdelphi -o"$name.fpc.exe" "$name.pas" \
        >fpc.log 2>fpc.err && "$WINE" "./$name.fpc.exe" >fpc.out 2>>fpc.err)
    [ -f "$w/$name.fpc.exe" ] || echo "   (fpc leg failed for $name)"
  fi
  [ -f "$w/fpc.out" ] || : > "$w/fpc.out"
  tr -d '\r' < "$w/fpc.out" > "$w/fpc.norm"

  # --- 2. Delphi 2007 dcc32 (Win32) --------------------------------------
  if [ "$have_delphi" = 1 ]; then
    (cd "$w" && "$WINE" "$DCC32_WIN" -B -Q "$name.pas" >dcc.log 2>dcc.err)
    if [ -f "$w/$name.exe" ]; then
      (cd "$w" && "$WINE" "./$name.exe" >delphi.out 2>>dcc.err)
    else
      echo "   (dcc32 leg failed for $name)"
      tr -d '\r' < "$w/dcc.log" | grep -iE "fehler|error" | head -3 | sed 's/^/   /'
    fi
  fi
  [ -f "$w/delphi.out" ] || : > "$w/delphi.out"
  tr -d '\r' < "$w/delphi.out" > "$w/delphi.norm"

  # --- 3. our chain (bin/pasler.exe under wine) --------------------------
  (cd "$w" && "$WINE" "$PASLER" --nimony:"$NIMONY_WIN" --run \
      -d:MSWINDOWS -d:WIN32 -d:WINDOWS "$name.pas" \
      >ours.out 2>ours.err)
  tr -d '\r' < "$w/ours.out" > "$w/ours.norm"

  echo "== $name"
  if [ "$have_fpc" = 1 ] && [ "$have_delphi" = 1 ]; then
    n=$(diff "$w/delphi.norm" "$w/fpc.norm" | grep -c '^[<>]')
    echo "   oracles: $n differing line(s)$([ -n "$expect" ] && echo " (we follow: $expect)")"
  fi
  if diff -u "$w/fpc.norm" "$w/ours.norm" > "$w/d.fpc" 2>&1; then
    echo "   ours-vs-fpc(win64): PASS"
  elif [ "$expect" = "delphi" ] || [ "$expect" = "none" ]; then
    echo "   ours-vs-fpc(win64): INFO"
  else
    echo "   ours-vs-fpc(win64): FAIL"
    sed 's/^/      /' "$w/d.fpc" | head -8
    fail=1
  fi
  if [ "$have_delphi" = 1 ]; then
    if diff -u "$w/delphi.norm" "$w/ours.norm" > "$w/d.delphi" 2>&1; then
      echo "   ours-vs-dcc32: PASS"
    elif [ "$expect" = "fpc" ] || [ "$expect" = "none" ]; then
      echo "   ours-vs-dcc32: INFO"
    else
      echo "   ours-vs-dcc32: FAIL"
      sed 's/^/      /' "$w/d.delphi" | head -8
      fail=1
    fi
  fi
  # the compiled program must have produced *something* - an empty ours.out
  # with a silent pasler failure is the failure mode this guards against
  if [ ! -s "$w/ours.norm" ]; then
    echo "   ours: NO OUTPUT"
    grep -v -iE "mesa|egl|amdgpu|fixme" "$w/ours.err" | head -5 | sed 's/^/      /'
    fail=1
  fi
done

# --- shim-level conformance under the Windows toolchain ---------------------
stem=$(basename "$SHIM" .nim)
echo "== shim ($TIER/$SHIM, win64)"
sw="$WORK/shim"
mkdir -p "$sw"
cp "$TIERDIR/$SHIM" "$sw/"
if (cd "$sw" && "$WINE" "$NIMONY_WIN" c --path:"$RUNTIME_WIN" -o:$stem.exe \
      "$SHIM" >shim.log 2>shim.err); then
  if [ -f "$sw/$stem.exe" ]; then
    shout=$(cd "$sw" && "$WINE" ./$stem.exe 2>/dev/null | tr -d '\r')
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
  grep -v -i "nifmake" "$sw/shim.log" | head -5 | sed 's/^/      /'
  fail=1
fi

echo "-- win64-oracle[$TIER]: $total samples"
[ "$fail" -eq 0 ] || exit 1
exit 0
