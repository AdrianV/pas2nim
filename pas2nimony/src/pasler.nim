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
Usage: pasler [command] [options] program.pas [-- program-args]
Commands (nimony-compatible):
  c                  build via the C backend (default; fast NIF pipeline)
  n                  build via the native (libc-free) backend
  w                  build via the wasm backend (needs freestanding-safe
                     units - the Pascal shims are not there yet)
  check              type-check the project, no codegen
  m, s, doc          single-module sem / NIF build / docs
Options:
  -d:SYM             define a conditional symbol ({$ifdef} + nimsem);
                     FPC-style -dSYM accepted too, repeatable
  --path:DIR, -p DIR extra search path, repeatable (Pascal unit
                     resolution + nimony import paths)
  -o:FILE            output path for the produced binary (forwarded)
  --run, -r          execute the produced binary after the build;
                     arguments after `--` go to the program
  --nimcache:DIR     cache directory (default: nimcache)
  --nimony:PATH      nimony driver binary (default: nimony on PATH)
  --ref              use 'ref' instead of 'ptr' for Pascal ^type
  --no-self-qualify  disable the self-qualification pass
  --no-case-canon    disable case canonicalization
  --no-init          do not add initializers to Pascal locals
  --v2               emit {.feature: "v2".} instead of lenientnils
  --strict           unfulfillable calling conventions (register,
                     pascal, safecall) error out instead of warning
  --                 everything after this goes to the built program
  -h, --help         show this help
Other nimony options (-o:, --cc:, --opt:, --passC:, ...) are
forwarded to the nimony chain unchanged.
"""
  write(stdout, Usage)
  quit(0)

const paslerVerbs = ["c", "n", "w", "l", "check", "s", "m", "doc"]

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

proc parseAndRender(srcfile, nimcache: string; flags: set[TParserFlag],
                    defines: seq[string] = @[],
                    searchPaths: seq[string] = @[]): int64 =
  ## phase A: parse one Pascal file and write its .nim anchor; returns
  ## the anchor's mtime in nanoseconds - the staleness reference nimony
  ## compares against
  var p = default(TParser)
  openParser(p, srcfile, flags, defines, searchPaths)
  var module = parseUnit(p)
  closeParser(p)
  let nimPath = nimcache / splitFile(srcfile).name & ".nim"
  renderModule(module, p.syms, srcfile, nimPath, flags)
  result = fileMtimeNs(nimPath)

proc parseAndEmit(srcfile, nimcache: string; paths: seq[string];
                  flags: set[TParserFlag]; isMain: bool;
                  defines: seq[string] = @[],
                  searchPaths: seq[string] = @[]): string =
  ## phase B: parse one Pascal file and emit its TokenBuf NIF pair;
  ## returns the .p.nif path the nimony chain will consume
  var p = default(TParser)
  openParser(p, srcfile, flags, defines, searchPaths)
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
  var command = "c"
  var haveCommand = false
  var defines: seq[string] = @[]
  var searchPaths: seq[string] = @[]
  var forwardOpts: seq[string] = @[]
  var progArgs: seq[string] = @[]
  var outOpt = ""
  var i = 1
  while i <= paramCount():
    let a = paramStr(i)
    if a == "--":
      # everything after `--` belongs to the built program
      inc i
      while i <= paramCount():
        progArgs.add(paramStr(i))
        inc i
      break
    if a == "-h" or a == "--help":
      usage()
    elif a == "--run" or a == "-r":
      doRun = true
    elif a.startsWith("--nimcache:"):
      nimcache = a[11..^1]
    elif a.startsWith("--nimony:"):
      nimonyBin = a[9..^1]
    elif a.startsWith("--path:"):
      let v = a[7..^1]
      if v != nimcache: searchPaths.add(v)
      forwardOpts.add("--path:" & v)
    elif a == "-p":
      inc i
      if i > paramCount(): usage()
      let v = paramStr(i)
      if v != nimcache: searchPaths.add(v)
      forwardOpts.add("--path:" & v)
    elif a.startsWith("-d:"):
      defines.add(a[3..^1])
      forwardOpts.add(a)
    elif a.startsWith("--define:"):
      defines.add(a[9..^1])
      forwardOpts.add("-d:" & a[9..^1])
    elif a.startsWith("-d") and a.len > 2 and a[2] != ':':
      # FPC-style `-dSYM`
      defines.add(a[2..^1])
      forwardOpts.add("-d:" & a[2..^1])
    elif a.startsWith("-o:"):
      outOpt = a[3..^1]
      forwardOpts.add(a)
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
    elif a == "--strict":
      # unfulfillable directives/calling conventions become errors
      flags.incl(pfStrictDirectives)
    elif a.len > 0 and a[0] == '-':
      # any other nimony-shaped option: forward verbatim
      forwardOpts.add(a)
    else:
      # bare argument: a command verb (before the input file) or the
      # input file itself
      if not haveCommand and infile.len == 0 and
          a.toLowerAscii in paslerVerbs:
        command = a.toLowerAscii
        haveCommand = true
      elif infile.len == 0:
        infile = a
      else:
        usage()
    inc i
  if infile.len == 0:
    usage()
  if not fileExists(infile):
    write(stderr, "pasler: cannot open " & infile & "\n")
    quit(1)
  # the search paths nimony will use, in the order pasler passes them:
  # nimcache first so unit anchors resolve there
  var paths: seq[string] = @[nimcache]
  for sp in searchPaths:
    if sp notin paths: paths.add(sp)

  try:
    createDir(path(nimcache))
  except ErrorCode:
    discard

  # the runtime module must be resolvable as a plain .nim
  let runtime = findSystemPas()
  let runtimeDir = splitFile(runtime).dir
  if runtimeDir.len == 0:
    write(stderr, "pasler: cannot locate the runtime directory\n")
    quit(1)

  var module: Node
  var units: seq[string] = @[]
  block:
    # 1. parse the main program; the parser absorbs the whole
    #    uses-closure transitively (shared UnitSet), so afterwards
    #    p.absorbed.files holds every Pascal unit file the project needs
    var p = default(TParser)
    openParser(p, infile, flags, defines, searchPaths)
    module = parseUnit(p)
    for unitFile in p.absorbed.files.keys:
      units.add(unitFile)
    closeParser(p)
  units.sort(proc(x, y: string): int = cmp(x, y))  # deterministic order

  if command in ["n", "w", "check", "m", "doc", "l"]:
    # delegate to nimony's own project graph over the rendered .nim
    # anchors (phase A is enough - no TokenBuf NIFs needed); this is
    # how the native/wasm/check/doc backends are reached
    var lastAnchorNs: int64 = 0
    for u in units:
      let t = parseAndRender(u, nimcache, flags, defines, searchPaths)
      if t > lastAnchorNs: lastAnchorNs = t
    let mainStem = splitFile(infile).name
    let mainNim = nimcache / mainStem & ".nim"
    block:
      let t = parseAndRender(infile, nimcache, flags, defines, searchPaths)
      if t > lastAnchorNs: lastAnchorNs = t
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
    var cmd = quoteShell(nimonyBin) & " " & command &
        " --nimcache:" & quoteShell(nimcache)
    cmd.add(" --path:" & quoteShell(nimcache))
    cmd.add(" --path:" & quoteShell(runtimeDir))
    for fo in forwardOpts:
      cmd.add(" " & fo)
    if not paths.contains("."):
      cmd.add(" --path:.")
    cmd.add(" " & quoteShell(mainNim))
    if doRun:
      cmd.add(" -r")
      for pa in progArgs:
        cmd.add(" " & quoteShell(pa))
    quit(execShellCmd(cmd))

  # 2. phase A: write every .nim anchor first (units, then the main)
  var lastAnchorNs: int64 = 0
  for u in units:
    let t = parseAndRender(u, nimcache, flags, defines, searchPaths)
    if t > lastAnchorNs: lastAnchorNs = t
  let mainStem = splitFile(infile).name
  let mainNif = nimcache / mainStem & ".p.nif"
  block:
    let t = parseAndRender(infile, nimcache, flags, defines, searchPaths)
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
    discard parseAndEmit(u, nimcache, paths, flags, false, defines,
                         searchPaths)
  discard parseAndEmit(infile, nimcache, paths, flags, true, defines,
                       searchPaths)

  # 5. hand the main module to the nimony chain
  var cmd = quoteShell(nimonyBin) & " s --nimcache:" & quoteShell(nimcache)
  cmd.add(" --path:" & quoteShell(nimcache))
  cmd.add(" --path:" & quoteShell(runtimeDir))
  for fo in forwardOpts:
    cmd.add(" " & fo)
  if not paths.contains("."):
    cmd.add(" --path:.")
  cmd.add(" " & quoteShell(mainNif))
  if execShellCmd(cmd) != 0:
    write(stderr, "pasler: nimony build failed\n")
    quit(1)

  var binary = nimcache / mainStem / mainStem & ".p"
  if outOpt.len > 0 and fileExists(outOpt):
    binary = outOpt
  if not fileExists(binary):
    write(stderr, "pasler: binary not found: " & binary & "\n")
    quit(1)
  if doRun:
    var runcmd = quoteShell(binary)
    for pa in progArgs:
      runcmd.add(" " & quoteShell(pa))
    quit(execShellCmd(runcmd))
  else:
    echo binary

main()
