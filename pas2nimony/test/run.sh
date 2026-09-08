#!/bin/bash
# pas2nimony test runner
#
# Translates every sample in this directory, compiles the generated Nim
# with the nimony compiler chain and runs it. A sample passes when both
# the translation and the nimony build succeed and the binary runs.
#
# Usage: ./run.sh [sample.pas ...]   (default: all samples)

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
NIMONY="${NIMONY:-/home/adrian/dev/nimony/bin/nimony}"

if [ ! -x "$ROOT/bin/pas2nimony" ]; then
  echo "bin/pas2nimony missing - run ./build.sh first" >&2
  exit 1
fi

TMP="$HERE/tmp"
mkdir -p "$TMP"
# the runtime shim units must be importable from the generated modules
cp -f "$ROOT"/runtime/*.nim "$TMP/"

if [ $# -gt 0 ]; then
  SAMPLES=("$@")
else
  SAMPLES=("$HERE"/*.pas)
fi

fail=0
for pas in "${SAMPLES[@]}"; do
  name="$(basename "$pas" .pas)"
  echo "== $name"
  if ! "$ROOT/bin/pas2nimony" "$pas" -o:"$TMP/$name.nim"; then
    echo "   TRANSLATE FAILED"
    fail=1
    continue
  fi
  # grep exits 1 when it filters out everything; treat that as success
  if ! (cd "$TMP" && "$NIMONY" c --path:. "$name.nim" 2>&1 | grep -v nifmake || true); then
    echo "   NIMONY BUILD FAILED"
    fail=1
    continue
  fi
  bin="$(ls "$TMP"/nimcache/*/"$name" 2>/dev/null | head -1)"
  if [ -z "$bin" ]; then
    echo "   BINARY NOT FOUND"
    fail=1
    continue
  fi
  echo "-- output:"
  "$bin"
  echo "-- ok"
done

# ---- multi-unit project: counter unit + program using it ----
if [ $# -eq 0 ] && [ -d "$HERE/twounit" ]; then
  echo "== twounit"
  mkdir -p "$TMP/twounit"
  for pas in "$HERE"/twounit/*.pas; do
    n="$(basename "$pas" .pas)"
    "$ROOT/bin/pas2nimony" "$pas" -o:"$TMP/twounit/$n.nim" || {
      echo "   TRANSLATE FAILED"; fail=1; continue; }
  done
  if (cd "$TMP/twounit" && cp "$ROOT"/runtime/*.nim . &&
      "$NIMONY" c --path:. usecounter.nim 2>&1 | grep -v nifmake || true); then
    bin="$(find "$TMP/twounit/nimcache" -name usecounter -type f | head -1)"
    if [ -n "$bin" ]; then
      echo "-- output:"
      "$bin"
      echo "-- ok"
    else
      echo "   BINARY NOT FOUND"; fail=1
    fi
  else
    echo "   NIMONY BUILD FAILED"; fail=1
  fi
fi

# ---- pasler: the .p.nif front-end path (no nifler for Pascal sources) ----
if [ $# -eq 0 ] && [ -x "$ROOT/bin/pasler" ] && [ -d "$HERE/twounit" ]; then
  echo "== pasler-twounit"
  mkdir -p "$TMP/pasler"
  cp "$HERE"/twounit/*.pas "$TMP/pasler/"
  if (cd "$TMP/pasler" && "$ROOT/bin/pasler" --nimony:"$NIMONY" --run usecounter.pas 2>&1 | grep -v nifmake || true); then
    # nifler must not have touched the Pascal-derived NIFs
    if grep -lq 'vendor "pasler"' "$TMP/pasler"/nimcache/*.p.nif 2>/dev/null; then
      echo "-- ok"
    else
      echo "   NIF VENDOR CHECK FAILED"; fail=1
    fi
  else
    echo "   PASLER BUILD FAILED"; fail=1
  fi
  # the NIF front-end must agree with the .nim path on semantics
  if [ -f "$HERE/events.pas" ]; then
    echo "== pasler-events"
    mkdir -p "$TMP/pasler-events"
    cp "$HERE/events.pas" "$TMP/pasler-events/"
    if (cd "$TMP/pasler-events" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run events.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER EVENTS FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/anonmeth.pas" ]; then
    echo "== pasler-anon"
    mkdir -p "$TMP/pasler-anon"
    cp "$HERE/anonmeth.pas" "$TMP/pasler-anon/"
    if (cd "$TMP/pasler-anon" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run anonmeth.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER ANON FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/usesnim.pas" ]; then
    echo "== pasler-usesnim"
    mkdir -p "$TMP/pasler-usesnim"
    cp "$HERE/usesnim.pas" "$TMP/pasler-usesnim/"
    if (cd "$TMP/pasler-usesnim" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run usesnim.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER USESNIM FAILED"; fail=1
    fi
  fi
  for extra in shims datetime sysutils missing; do
    if [ -f "$HERE/$extra.pas" ]; then
      echo "== pasler-$extra"
      mkdir -p "$TMP/pasler-$extra"
      cp "$HERE/$extra.pas" "$TMP/pasler-$extra/"
      if (cd "$TMP/pasler-$extra" &&
          "$ROOT/bin/pasler" --nimony:"$NIMONY" --run $extra.pas 2>&1 |
          grep -v nifmake || true); then
        echo "-- ok"
      else
        echo "   PASLER $extra FAILED"; fail=1
      fi
    fi
  done
  if [ -f "$HERE/generics.pas" ]; then
    echo "== pasler-generics"
    mkdir -p "$TMP/pasler-generics"
    cp "$HERE/generics.pas" "$TMP/pasler-generics/"
    if (cd "$TMP/pasler-generics" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run generics.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER GENERICS FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/opover.pas" ]; then
    echo "== pasler-op"
    mkdir -p "$TMP/pasler-op"
    cp "$HERE/opover.pas" "$TMP/pasler-op/"
    if (cd "$TMP/pasler-op" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run opover.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER OP FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/classmeth.pas" ]; then
    echo "== pasler-cm"
    mkdir -p "$TMP/pasler-cm"
    cp "$HERE/classmeth.pas" "$TMP/pasler-cm/"
    if (cd "$TMP/pasler-cm" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run classmeth.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER CM FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/intf.pas" ]; then
    echo "== pasler-intf"
    mkdir -p "$TMP/pasler-intf"
    cp "$HERE/intf.pas" "$TMP/pasler-intf/"
    if (cd "$TMP/pasler-intf" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run intf.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER INTF FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/cast.pas" ]; then
    echo "== pasler-cast"
    mkdir -p "$TMP/pasler-cast"
    cp "$HERE/cast.pas" "$TMP/pasler-cast/"
    if (cd "$TMP/pasler-cast" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run cast.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER CAST FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/goto.pas" ]; then
    echo "== pasler-goto"
    mkdir -p "$TMP/pasler-goto"
    cp "$HERE/goto.pas" "$TMP/pasler-goto/"
    if (cd "$TMP/pasler-goto" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run goto.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER GOTO FAILED"; fail=1
    fi
  fi
  if [ -f "$HERE/with.pas" ]; then
    echo "== pasler-with"
    mkdir -p "$TMP/pasler-with"
    cp "$HERE/with.pas" "$TMP/pasler-with/"
    if (cd "$TMP/pasler-with" &&
        "$ROOT/bin/pasler" --nimony:"$NIMONY" --run with.pas 2>&1 |
        grep -v nifmake || true); then
      echo "-- ok"
    else
      echo "   PASLER WITH FAILED"; fail=1
    fi
  fi
fi

# M5 oracle: differential testing against real FPC (skips without fpc)
if ! sh "$HERE/oracle.sh"; then
  echo "   ORACLE FAILED"; fail=1
fi

# keep the generated artifacts for inspection, but drop the build cache
rm -rf "$TMP/nimcache"
exit $fail