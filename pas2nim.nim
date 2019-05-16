#
#
#      Pas2nim - Pascal to Nim source converter
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  strutils, os, parseopt, llstream, ast, renderer, options, msgs,
  paslex, pasparse, pathutils, lineinfos

const
  Version = "0.9"
  Usage = """
pas2nim - Pascal to Nim source converter
  (c) 2012 Andreas Rumpf
Usage: pas2nim [options] inputfile [options]
Options:
  -o, --out:FILE         set output filename
  --ref                  convert ^typ to ref typ (default: ptr typ)
  --boot                 use special translation rules for the Nim compiler
  -v, --version          write pas2nim's version
  -h, --help             show this help
"""

proc main(infile, outfile: string, flags: set[TParserFlag]) =
  var stream = llStreamOpen(AbsoluteFile infile, fmRead)
  if stream == nil: rawMessage(gConfig, errGenerated, "cannot open file" & infile)
  var p: TParser
  openParser(p, infile, stream, flags)
  var module = parseUnit(p)
  closeParser(p)
  renderModule(module, infile, outfile)

var
  infile = ""
  outfile = ""
  flags: set[TParserFlag] = {}
for kind, key, val in getopt():
  case kind
  of cmdArgument: infile = key
  of cmdLongOption, cmdShortOption:
    case key
    of "help", "h":
      stdout.write(Usage)
      quit(0)
    of "version", "v":
      stdout.write(Version & "\n")
      quit(0)
    of "o", "out": outfile = val
    of "ref": incl(flags, pfRefs)
    of "boot": flags = flags + {pfRefs, pfMoreReplacements, pfImportBlackList}
    else: stdout.writeLine("[Error] unknown option: " & key)
  of cmdEnd: assert(false)
if infile.len == 0:
  # no filename has been given, so we show the help:
  stdout.write(Usage)
else:
  if outfile.len == 0:
    outfile = changeFileExt(infile, "nim")
  infile = addFileExt(infile, "pas")
  main(infile, outfile, flags)
