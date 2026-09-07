#!/bin/sh
# Build pas2nimony with the nimony toolchain.
# See doc/toolchain.md for the pinned nimony version.
set -e
cd "$(dirname "$0")"
NIMONY=../../nimony/bin/nimony
mkdir -p bin
"$NIMONY" c -o:bin/pas2nimony src/pas2nimony.nim
"$NIMONY" c -o:bin/pasler src/pasler.nim