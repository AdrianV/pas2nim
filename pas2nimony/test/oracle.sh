#!/bin/sh
# M5 oracle - differential testing against real FPC.
#
# Every sample under test/oracle/ is compiled and run by real FPC
# (-Mdelphi) and by pasler; the outputs must be byte-identical.
# Samples must be deterministic (no Now/Random) and stick to the
# Delphi API both worlds support. Skips gracefully when fpc is not
# installed.
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT="$HERE/.."
NIMONY="${NIMONY:-$ROOT/../../nimony/bin/nimony}"

if ! command -v fpc >/dev/null 2>&1; then
  echo "oracle: fpc not installed - skipped"
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
  (cd "$w" && fpc -Mdelphi -o"$name.fpc" "$name.pas" >fpc.log 2>&1 &&
    "./$name.fpc" >fpc.out 2>&1)
  (cd "$w" && "$ROOT/bin/pasler" --nimony:"$NIMONY" --run "$name.pas" \
      >pasler.out 2>&1)
  if diff -u "$w/fpc.out" "$w/pasler.out" > "$w/diff.txt" 2>&1; then
    echo "PASS $name"
  else
    echo "FAIL $name"
    sed 's/^/   /' "$w/diff.txt" | head -10
    fail=1
  fi
done
echo "-- oracle: $total samples"
exit $fail