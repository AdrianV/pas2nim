#
#           pasler - the Pascal front-end driver for the nimony chain
#
# Translates a Pascal program and its full unit closure directly to
# parsed NIF (.p.nif) with the TokenBuf writer (no nifler involved for
# Pascal sources), then hands the result to `nimony s`.
#
# How it plugs into the nimony chain: the driver's import resolver only
# maps a module name to a `<module>.nim` source on the search path, and
# nifler is skipped for a module when its cached .p.nif / .p.deps.nif
# are newer than that .nim (whole-second mtime granularity). pasler
# exploits exactly that contract:
#
#  1. every unit in the uses-closure is translated to a .nim anchor in
#     the nimcache (phase A, all anchors first),
#  2. after a short wait so the anchors are a whole second in the past,
#     pasler emits each unit's TokenBuf NIF under the *hashed* module
#     name the nimony driver computes for that anchor (`moduleSuffix`
#     from gear2/modnames, imported here for exact parity) (phase B),
#  3. nifler therefore never runs on Pascal-derived units; our NIF
#     flows through nimsem/hexer/lengc untouched. If timestamps ever
#     get confused, nifler parses the real .nim anchor and the build
#     stays correct - graceful degradation.
#
# The main module is passed to `nimony s` by file path (no resolution
# needed), so it keeps its plain stem name in the cache.

import std/[syncio, os, strutils]
import std/algorithm
import std/dirs
import std/paths
import pasast, paslex, paspars, pasnimout, pasnifout
import ../../nimony/src/gear2/modnames

when defined(posix):
  proc usleep(usecs: cuint) {.importc: "usleep", header: "<unistd.h>".}

proc usage =
  const Usage = """
pasler - Pascal front-end driver for the nimony compiler chain
Usage: pasler [options] program.pas
Options:
  --nimcache:DIR     cache directory (default: nimcache)
  --path:DIR         extra search path, repeatable (default: nimcache + .)
  --nimony:PATH      nimony driver binary (default: nimony on PATH)
  --run              execute the produced binary after the build
  --ref              use 'ref' instead of 'ptr' for Pascal ^type
  --no-self-qualify  disable the self-qualification pass
  --no-case-canon    disable case canonicalization
  --no-init          do not add initializers to Pascal locals
  --v2               emit {.feature: "v2".} instead of lenientnils
  -h, --help         show this help
"""
  write(stdout, Usage)
  quit(0)

proc findSystemPas: string =
  ## locate the pas2nimony runtime module relative to this executable
  let exeDir = splitFile(getAppFilename()).dir
  for cand in [exeDir & "/../runtime/systempas.nim",
               exeDir & "/../../runtime/systempas.nim",
               "runtime/systempas.nim"]:
    if fileExists(cand):
      return cand
  write(stderr, "pasler: cannot find runtime/systempas.nim\n")
  quit(1)

proc fileMtimeNs(f: string): int64 =
  try:
    result = getLastModificationTime(f)
  except ErrorCode:
    result = 0

proc parseAndRender(srcfile, nimcache: string; flags: set[TParserFlag]): int64 =
  ## phase A: parse one Pascal file and write its .nim anchor; returns
  ## the anchor's mtime in nanoseconds - the staleness reference nimony
  ## compares against
  var p = default(TParser)
  openParser(p, srcfile, flags)
  var module = parseUnit(p)
  closeParser(p)
  let nimPath = nimcache / splitFile(srcfile).name & ".nim"
  renderModule(module, p.syms, srcfile, nimPath, flags)
  result = fileMtimeNs(nimPath)

proc parseAndEmit(srcfile, nimcache: string; paths: seq[string];
                  flags: set[TParserFlag]; isMain: bool): string =
  ## phase B: parse one Pascal file and emit its TokenBuf NIF pair;
  ## returns the .p.nif path the nimony chain will consume
  var p = default(TParser)
  openParser(p, srcfile, flags)
  var module = parseUnit(p)
  closeParser(p)
  let stem = splitFile(srcfile).name
  let nimPath = nimcache / stem & ".nim"
  let base = if isMain: nimcache / stem
             else: nimcache / moduleSuffix(nimPath, paths)
  let nifPath = base & ".p.nif"
  emitNifModule(module, p.syms, srcfile, nifPath, flags)
  result = nifPath

proc main =
  var infile = ""
  var nimcache = "nimcache"
  var nimonyBin = "nimony"
  var doRun = false
  var flags: set[TParserFlag] = {}
  var args: seq[string] = @[]
  var i = 1
  while i <= paramCount():
    let a = paramStr(i)
    if a == "-h" or a == "--help":
      usage()
    elif a == "--run":
      doRun = true
    elif a.startsWith("--nimcache:"):
      nimcache = a[11..^1]
    elif a.startsWith("--nimony:"):
      nimonyBin = a[9..^1]
    elif a.startsWith("--path:"):
      args.add(a)  # forwarded verbatim, order matters for moduleSuffix
    elif a == "--ref":
      flags.incl(pfRefs)
    elif a == "--no-self-qualify":
      flags.incl(pfNoSelfQualify)
    elif a == "--no-case-canon":
      flags.incl(pfNoCaseCanon)
    elif a == "--no-init":
      flags.incl(pfNoInit)
    elif a == "--v2":
      flags.incl(pfV2)
    else:
      args.add(a)
    inc i
  if args.len != 1:
    usage()
  infile = args[0]
  if not fileExists(infile):
    write(stderr, "pasler: cannot open " & infile & "\n")
    quit(1)

  # the search paths nimony will use, in the order pasler passes them:
  # nimcache first so unit anchors resolve there
  var paths: seq[string] = @[nimcache]
  for a in args:
    if a.startsWith("--path:"):
      let v = a[7..^1]
      if v != nimcache: paths.add(v)

  try:
    createDir(path(nimcache))
  except ErrorCode:
    discard

  # 1. parse the main program; the parser absorbs the whole uses-closure
  #    transitively (shared UnitSet), so afterwards p.absorbed.files holds
  #    every Pascal unit file the project needs
  var p = default(TParser)
  openParser(p, infile, flags)
  var module = parseUnit(p)
  closeParser(p)

  var units: seq[string] = @[]
  for unitFile in p.absorbed.files.keys:
    units.add(unitFile)
  units.sort(proc(x, y: string): int = cmp(x, y))  # deterministic order

  # 2. phase A: write every .nim anchor first (units, then the main)
  var lastAnchorNs: int64 = 0
  for u in units:
    let t = parseAndRender(u, nimcache, flags)
    if t > lastAnchorNs: lastAnchorNs = t
  let mainStem = splitFile(infile).name
  let mainNif = nimcache / mainStem & ".p.nif"
  block:
    let t = parseAndRender(infile, nimcache, flags)
    if t > lastAnchorNs: lastAnchorNs = t

  # the nimony driver's staleness comparison has whole-second
  # granularity, so make sure the TokenBuf NIFs land in a later second
  # than every anchor; a probe file gives "now" in file-mtime units
  let probe = nimcache / ".pasler.probe"
  try:
    writeFile(probe, "t")
    let nowNs = fileMtimeNs(probe)
    let needNs = lastAnchorNs + 1_100_000_000 - nowNs
    if needNs > 0:
      when defined(posix):
        usleep(cuint(needNs div 1000 + 1))
  except ErrorCode:
    discard

  # 3. phase B: emit the TokenBuf NIFs (units at the hashed names the
  #    driver will resolve them to, main under its plain stem)
  for u in units:
    discard parseAndEmit(u, nimcache, paths, flags, false)
  discard parseAndEmit(infile, nimcache, paths, flags, true)

  # 4. the runtime module must be resolvable as a plain .nim
  let syspas = nimcache / "systempas.nim"
  let runtime = findSystemPas()
  var needCopy = not fileExists(syspas)
  if not needCopy:
    try:
      needCopy = getLastModificationTime(syspas) < getLastModificationTime(runtime)
    except ErrorCode:
      needCopy = true
  if needCopy:
    try:
      writeFile(syspas, readFile(runtime))
    except ErrorCode:
      write(stderr, "pasler: cannot write " & syspas & "\n")
      quit(1)

  # 5. hand the main module to the nimony chain
  var cmd = quoteShell(nimonyBin) & " s --nimcache:" & quoteShell(nimcache)
  cmd.add(" --path:" & quoteShell(nimcache))
  for a in args:
    if a.startsWith("--path:"):
      cmd.add(" " & a)
  if not paths.contains("."):
    cmd.add(" --path:.")
  cmd.add(" " & quoteShell(mainNif))
  if execShellCmd(cmd) != 0:
    write(stderr, "pasler: nimony build failed\n")
    quit(1)

  let binary = nimcache / mainStem / mainStem & ".p"
  if not fileExists(binary):
    write(stderr, "pasler: binary not found: " & binary & "\n")
    quit(1)
  if doRun:
    quit(execShellCmd(quoteShell(binary)))
  else:
    echo binary

main()