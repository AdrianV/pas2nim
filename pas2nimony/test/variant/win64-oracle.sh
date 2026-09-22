#!/bin/sh
# Variant conformance tier for the WINDOWS toolchain (Delphi 2007 dcc32 +
# FPC 3.2.2 x86_64-win64 + our chain), driven by the shared engine
# test/win64-oracle.sh. Samples carry a <name>.expect file naming the oracle
# we follow where the two disagree; vshim.nim is the shim-level leg.
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec "$HERE/../win64-oracle.sh" variant "$HERE" vshim.nim
