#
#           pas2nimony - Pascal to Nimony translator
#
# A Pascal (Delphi dialect) to Nim source translator built with and for
# the nimony toolchain. Ported from the old pas2nim, but without any
# dependency on the old Nim 1.x compiler internals.
#
# Key nimony-compatibility features of the generated code:
#  - no {.this.} pragma: member access is qualified with `self.` explicitly
#  - case preservation: identifiers use the spelling of their declaration
#  - explicit initializers for Pascal locals (nimony strict definitions)
#  - {.feature: "lenientnils".} module pragma by default

import std/[syncio, os, strutils]
import pasast, paslex, paspars, pasnimout, pasnifout

proc translateFile(infile, outfile: string, flags: set[TParserFlag]) =
  var p = default(TParser)
  openParser(p, infile, flags)
  var module = parseUnit(p)
  closeParser(p)
  renderModule(module, p.syms, infile, outfile, flags)

proc emitNif(infile, outPath: string, flags: set[TParserFlag]) =
  ## emit the parsed-NIF pair (<outPath>.p.nif + <outPath>.p.deps.nif)
  var p = default(TParser)
  openParser(p, infile, flags)
  var module = parseUnit(p)
  closeParser(p)
  emitNifModule(module, p.syms, infile, outPath, flags)

proc usage =
  const Usage = """
pas2nimony - Pascal to Nimony translator
Usage: pas2nimony [options] inputfile
Options:
  -o, --out:FILE     set output filename
  --ref              use 'ref' instead of 'ptr' for Pascal ^type
  --no-self-qualify  disable the self-qualification pass
  --no-case-canon    disable case canonicalization
  --no-init          do not add initializers to Pascal locals
  --v2               emit {.feature: "v2".} instead of lenientnils
  --emit-nif:FILE    emit parsed NIF (<FILE>.p.nif + .p.deps.nif) directly
                     from the Pascal AST, no Nim renderer involved
  -h, --help         show this help
"""
  write(stdout, Usage)
  quit(0)

proc main =
  var infile = ""
  var outfile = ""
  var emitNifPath = ""
  var flags: set[TParserFlag] = {}
  var args: seq[string] = @[]
  var i = 1
  while i <= paramCount():
    let a = paramStr(i)
    if a == "-h" or a == "--help":
      usage()
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
    elif a.startsWith("--emit-nif:"):
      emitNifPath = a[11..^1]
    elif a.startsWith("-o:"):
      outfile = a[3..^1]
    elif a == "-o":
      inc i
      if i <= paramCount(): outfile = paramStr(i)
    else:
      args.add(a)
    inc i
  if args.len != 1:
    usage()
  infile = args[0]
  if emitNifPath.len > 0:
    if not emitNifPath.endsWith(".p.nif"):
      emitNifPath.add(".p.nif")
    emitNif(infile, emitNifPath, flags)
    echo "wrote " & emitNifPath & " (+ deps)"
    quit(0)
  if outfile.len == 0:
    outfile = changeFileExt(infile, "nim")
  translateFile(infile, outfile, flags)

main()