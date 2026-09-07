#!/bin/bash
# pasler proof of concept (M2 step 1): run a .pas through the nimony
# chain WITHOUT the Nim renderer's output being the build input.
#
#   mini.pas ─pas2nimony→ mini.nim ─nifler→ mini.p.nif (+ .p.deps.nif)
#   nimony s --nimcache:nimcache --path:. nimcache/mini.p.nif
#   → nimcache/mini/mini.p   (binary, no .nim involved in the build)
#
# Verified findings (see doc/nimony-compat.md):
# - `nimony s <file.p.nif>` skips nifler for .nif inputs (deps.nim:toPair)
# - the driver needs `<mod>.p.deps.nif` next to it, mirroring the import
#   statements in plain (unhashed) module names
# - module suffixes are PATH-dependent hashes: keep the translated
#   module's imports resolvable from ONE location and pass --path
# - the .p.nif is the single source of truth; hand edits flow through

set -eu
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
NIMONY_DIR="${NIMONY_DIR:-/home/adrian/dev/nimony}"
NIMONY="${NIMONY:-$NIMONY_DIR/bin/nimony}"
P2N="$ROOT/bin/pas2nimony"

D="$(mktemp -d)"
trap 'rm -rf "$D"' EXIT
cd "$D"

cat > mini.pas << 'EOF'
interface
implementation
var x: Integer;
begin
  x := 21 * 2;
  write('the answer is ', x);
end.
EOF

# 1. Pascal -> Nim (stage 1 translator)
"$P2N" mini.pas -o:mini.nim

# 2. Nim -> parsed NIF (pasler phase 2 replaces this with direct NIF emission)
mkdir -p nimcache
"$NIMONY_DIR/bin/nifler" --portablePaths --deps parse mini.nim nimcache/mini.p.nif

# 3. the shim must resolve from one consistent location
cp "$ROOT/runtime/systempas.nim" .

# 4. nimsem -> hexer -> lengc -> cc, starting FROM the parsed NIF
"$NIMONY" s --nimcache:nimcache --path:. nimcache/mini.p.nif

# 5. run
out="$(./nimcache/mini/mini.p)"
echo "output: $out"
[ "$out" = "the answer is 42" ] && echo "PASLER POC: PASS" || { echo "PASLER POC: FAIL"; exit 1; }