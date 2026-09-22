#!/bin/sh
# AnsiString / pointer conformance tier for the WINDOWS toolchain.
# The shared engine (test/win64-oracle.sh) runs every sample through FPC
# x86_64-win64, dcc32 and bin/pasler.exe inside ./.wine/win64, plus the
# ashim.nim shim leg compiled by the prefix's own nimony.exe.
set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec "$HERE/../win64-oracle.sh" ansi "$HERE" ashim.nim
