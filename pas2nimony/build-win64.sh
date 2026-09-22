#!/bin/sh
# Build pas2nimony and pasler as Windows binaries with the nimony toolchain
# installed in the wine win64 prefix. See doc/toolchain.md.
#
# No source is copied or moved: the prefix maps the repository itself, so
# pas2nimony is `e:\` and its parent (the private tree) is `d:\`. Launching
# wine from this directory makes wine's cwd `e:\`, so every relative path
# below lands in the real checkout.
#
# The toolchain is found on the prefix PATH (it already carries
# C:\mingw64\bin, C:\nimony\bin, C:\Nim\bin, C:\lazarus\fpc\3.2.2\bin\x86_64-win64
# and the CodeGear bin), so no WINEPATH is needed: `gcc` for niflink and
# `nimony` for pasler's default --nimony both resolve.
set -e
cd "$(dirname "$0")"

REPO=$(cd .. && pwd)
export WINEPREFIX="${WINEPREFIX:-$REPO/.wine/win64}"
export WINEDEBUG=-all
WINE="${WINE:-wine}"
NIMONY="${NIMONY:-c:/nimony/bin/nimony.exe}"
NIMCACHE="${NIMCACHE:-nimcache-win}"

mkdir -p bin
"$WINE" "$NIMONY" c --nimcache:"$NIMCACHE" -o:bin/pas2nimony.exe src/pas2nimony.nim
"$WINE" "$NIMONY" c --nimcache:"$NIMCACHE" -o:bin/pasler.exe src/pasler.nim

for f in bin/pas2nimony.exe bin/pasler.exe; do
  if [ ! -f "$f" ]; then
    echo "build-win64: missing $f" >&2
    exit 1
  fi
  echo "build-win64: $f  $(file -b "$f" | cut -c1-40)"
done
