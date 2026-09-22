#!/bin/sh
# Rebuild runtime/placeholders/ from the local Pascal sources.
#
# A placeholder makes `uses X` for a unit that is deliberately *not* shimmed
# resolve to an empty Nim module, so a symbol use fails loudly at semcheck
# instead of the module failing to resolve. The names are unit names of the
# closed-source Delphi trees this front end translates, so neither the stubs
# nor the name list are published: runtime/placeholders/, its .list, the
# .extra additions and the .paths config are gitignored and this script
# recreates them from the local trees.
#
# The set is *derived*, not curated: read every `uses` clause in the local
# Pascal trees the corpus tiers build from and stub exactly those
# units that resolve to nothing - no .pas source, no .nim shim, not a unit
# paspars maps to a runtime shim, not a nimony stdlib module. A name that
# resolves anywhere is never stubbed, so a stub can never shadow a real
# unit, and the derived set is a superset of what any single build needs.
#
# Usage:
#   make-placeholders.sh              derive from runtime/placeholders.paths
#                                     (a gitignored local config, one search
#                                     directory per line) and write the stubs
#   make-placeholders.sh --main F     add a source to read (repeatable)
#   make-placeholders.sh --scan-dir D add every *.pas under D (repeatable)
#   make-placeholders.sh --path D     add a search directory (repeatable)
#   make-placeholders.sh --add NAME   remember a name the scan cannot see
#   make-placeholders.sh --list       print the names (local only)
#   make-placeholders.sh --check      report drift, write nothing
set -eu

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$HERE                                   # pas2nimony
REPO=$(cd "$ROOT/.." && pwd)
PH=$ROOT/runtime/placeholders
LIST=$ROOT/runtime/placeholders.list
EXTRA=$ROOT/runtime/placeholders.extra
NIMONY_LIB=$REPO/nimony/lib

# paspars maps these to a runtime/*.nim module of its own instead of a
# placeholder (see its uses-clause dispatch)
SHIMMED="strutils math dateutils classes sysutils si_strings system variants"

die() { echo "make-placeholders: $*" >&2; exit 1; }
note() { echo "make-placeholders: $*"; }

TMPF=${TMPDIR:-/tmp}/mkph.$$
trap 'rm -f "$TMPF".*' EXIT INT TERM
export LC_ALL=C

# --- the uses-clause scanner -----------------------------------------------
# one pass over every source: strip {..} / (*..*) / // comments, then print
# `file<TAB>unit` for every identifier in a uses clause. Conditional
# directives are comments too, so a guarded clause contributes both arms -
# harmless, an unresolvable name is stubbed either way.
cat > "$TMPF.scan.awk" <<'AWK'
BEGIN { blk = 0; inuses = 0 }
{
  line = $0; i = 1; n = length(line); out = ""
  while (i <= n) {
    c = substr(line, i, 1); c2 = substr(line, i, 2)
    if (blk == 1) { if (c == "}") blk = 0; i++ }
    else if (blk == 2) { if (c2 == "*)") { blk = 0; i += 2 } else i++ }
    else if (c2 == "//") break
    else if (c2 == "(*") { blk = 2; i += 2 }
    else if (c == "{") { blk = 1; i++ }
    else if (c == "'") { i++; while (i <= n && substr(line, i, 1) != "'") i++; i++ }
    else { out = out c; i++ }
  }
  i = 1; n = length(out)
  while (i <= n) {
    c = substr(out, i, 1)
    if (c ~ /[A-Za-z_]/) {
      j = i
      while (j <= n && substr(out, j, 1) ~ /[A-Za-z0-9_.]/) j++
      tok = substr(out, i, j - i); i = j
      if (inuses) { if (tok != "in") print FILENAME "\t" tok }
      else if (tok == "uses") inuses = 1
    } else {
      if (c == ";") inuses = 0
      i++
    }
  }
}
AWK

# the closure walk: `file<TAB>unit` edges, the module map, and the excluded
# sets are read here, so the whole traversal is one process
# the resolver: `file<TAB>unit` edges plus the module map and the excluded
# sets. A name in no map entry needs a stub.
cat > "$TMPF.resolve.awk" <<'AWK'
BEGIN {
  while ((getline l < mapf) > 0) { split(l, a, "\t"); if (a[1] != "") def[a[1]] = 1 }
  while ((getline l < stdf) > 0) if (l != "") std[l] = 1
  while ((getline l < shimf) > 0) if (l != "") shim[l] = 1
}
{
  split($0, a, "\t")
  u = a[2]
  if (u == "" || u ~ /[^A-Za-z0-9_]/) next
  lu = tolower(u)
  if (shim[lu] || std[lu] || def[lu]) next
  stubs[u] = 1
}
END { for (s in stubs) print s }
AWK

# --- arguments -------------------------------------------------------------

MAINS=""
SCANDIRS=""
PATHS=""
ADDS=""
ACTION=build
while [ $# -gt 0 ]; do
  case $1 in
    --main) shift; [ $# -gt 0 ] || die "--main needs a file"; MAINS="$MAINS $1"; shift ;;
    --scan-dir) shift; [ $# -gt 0 ] || die "--scan-dir needs a directory"; SCANDIRS="$SCANDIRS $1"; shift ;;
    --path) shift; [ $# -gt 0 ] || die "--path needs a directory"; PATHS="$PATHS $1"; shift ;;
    --add) shift; [ $# -gt 0 ] || die "--add needs a name"; ADDS="$ADDS $1"; shift ;;
    --list) ACTION=list; shift ;;
    --check) ACTION=check; shift ;;
    -h|--help) sed -n '2,28p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) die "unknown argument: $1" ;;
  esac
done

# the runtime's own shims always resolve; everything else comes from the
# local config (its lines are directory names of the private trees, so it is
# gitignored) or from the arguments
PATHS="$ROOT/runtime"
CONFIG=$ROOT/runtime/placeholders.paths
if [ -f "$CONFIG" ]; then
  while read -r d; do
    case $d in ''|'#'*) continue ;; esac
    [ -d "$d" ] && PATHS="$PATHS $d"
  done < "$CONFIG"
fi

# --- inputs: sources, module map, exclusions -------------------------------

: > "$TMPF.srcs"
for d in $PATHS; do
  find "$d" \( -iname '*.pas' -o -iname '*.dpr' \) -print >> "$TMPF.srcs"
done
for d in $SCANDIRS; do
  find "$d" -iname '*.pas' -print >> "$TMPF.srcs"
done
for f in $MAINS; do [ -f "$f" ] && echo "$f" >> "$TMPF.srcs"; done
sort -u "$TMPF.srcs" -o "$TMPF.srcs"

# lowercased unit name -> file, a .pas source winning over a .nim shim.
# Every .nim module on the path counts, not only the ones we scan: a name
# that a runtime shim provides must never be stubbed.
: > "$TMPF.modules"
for d in $PATHS; do
  find "$d" \( -iname '*.pas' -o -iname '*.dpr' -o -iname '*.nim' \) -print >> "$TMPF.modules"
done
sort -u "$TMPF.modules" -o "$TMPF.modules"
: > "$TMPF.map"
while read -r f; do
  [ -n "$f" ] || continue
  # a copy of a generated stub (the build trees flatten runtime/placeholders
  # into their own directories) must not count as a resolvable module - both
  # the current and the earlier stub wording
  case $f in
    *.nim) grep -qE 'companion unit placeholder|placeholder companion unit' \
             "$f" 2>/dev/null && continue ;;
  esac
  b=$(basename "$f"); b=${b%.*}; b=$(echo "$b" | tr 'A-Z' 'a-z')
  case $f in
    *.nim) echo "$b	$f	2" >> "$TMPF.map" ;;
    *)     echo "$b	$f	1" >> "$TMPF.map" ;;
  esac
done < "$TMPF.modules"
sort -t"	" -k1,1 -k3,3 "$TMPF.map" | awk -F'\t' '!seen[$1]++ { print $1 "\t" $2 }' > "$TMPF.map.uniq"

if [ -d "$NIMONY_LIB" ]; then
  find "$NIMONY_LIB" -maxdepth 2 -name '*.nim' | sed 's|.*/||; s|\.nim$||' \
    | tr 'A-Z' 'a-z' | sort -u > "$TMPF.std"
else
  : > "$TMPF.std"
fi
for s in $SHIMMED; do echo "$s"; done | sort -u > "$TMPF.shim"

# --- derive ----------------------------------------------------------------

# shellcheck disable=SC2046
awk -f "$TMPF.scan.awk" $(cat "$TMPF.srcs") > "$TMPF.edges"
awk -v mapf="$TMPF.map.uniq" -v stdf="$TMPF.std" -v shimf="$TMPF.shim" \
    -f "$TMPF.resolve.awk" "$TMPF.edges" | sort -u > "$TMPF.derived"

if [ -n "$ADDS" ]; then
  for n in $ADDS; do
    case $n in *[!A-Za-z0-9_]*) die "not a unit name: $n" ;; esac
    echo "$n" >> "$EXTRA"
  done
  sort -uf "$EXTRA" -o "$EXTRA"
fi
[ -f "$EXTRA" ] && cat "$EXTRA" >> "$TMPF.derived"
sort -uf "$TMPF.derived" | grep . > "$TMPF.list"
mv "$TMPF.list" "$LIST"

if [ "$ACTION" = list ]; then
  grep -v '^#' "$LIST" | grep . || true
  exit 0
fi

if [ ! -s "$LIST" ]; then
  note "nothing derived - no local Pascal tree found"
  note "point the script at one: --main FILE / --scan-dir DIR / --path DIR"
  note "(or list them in $CONFIG, one directory per line - it is gitignored)"
  exit 0
fi

write_stub() {
  # $1 = unit name; writes only when the content differs, so the build's
  # mtime-based staleness checks stay quiet
  f=$PH/$1.nim
  {
    echo '{.feature: "lenientnils".}'
    echo '# placeholder companion unit - the API is not shimmed yet; symbol'
    echo '# uses fail at semcheck (honest loud failure)'
  } > "$TMPF.stub"
  { [ -f "$f" ] && cmp -s "$f" "$TMPF.stub"; } && return 0
  mkdir -p "$PH"
  cp "$TMPF.stub" "$f"
  return 0
}

if [ "$ACTION" = check ]; then
  drift=0
  while read -r n; do
    case $n in ''|'#'*) continue ;; esac
    [ -f "$PH/$n.nim" ] || { note "missing stub: $n"; drift=1; }
  done < "$LIST"
  for f in "$PH"/*.nim; do
    [ -e "$f" ] || continue
    b=$(basename "$f" .nim)
    grep -qix -- "$b" "$LIST" || { note "unlisted stub: $b"; drift=1; }
  done
  [ "$drift" -eq 0 ] && note "placeholders in sync ($(grep -c . "$LIST") names)"
  exit "$drift"
fi

mkdir -p "$PH"
count=0
while read -r n; do
  case $n in ''|'#'*) continue ;; esac
  write_stub "$n"
  count=$((count + 1))
done < "$LIST"
# drop stubs the list no longer names: this directory is a generated mirror
for f in "$PH"/*.nim; do
  [ -e "$f" ] || continue
  b=$(basename "$f" .nim)
  grep -qix -- "$b" "$LIST" || { note "removing stale stub $b"; rm -f "$f"; }
done
note "placeholders: $count stubs in $PH"
