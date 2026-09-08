#
#           pas2nimony - Pascal to Nimony translator
#        (c) Copyright 2025 Adrian  (based on pas2nim by A. Rumpf)
#
# This module implements a Delphi/Pascal scanner. It is a port of the old
# pas2nim lexer onto nimony's std/lexbase. Unlike the original it keeps the
# raw spelling of identifiers (case preservation) and does not use an ident
# cache: identifiers are plain strings, keyword lookup is case-insensitive.

import std/[lexbase, streams, strutils, syncio, parseutils]
import pasast

const
  numChars* = {'0'..'9', 'a'..'z', 'A'..'Z'}
  SymChars* = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF'}
  SymStartChars* = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}
  OpChars* = {'+', '-', '*', '/', '<', '>', '!', '?', '^', '.', '|',
    '=', ':', '%', '&', '$', '@', '~', '\x80'..'\xFF'}

type
  TTokKind* = enum
    pxInvalid, pxEof,
    pxAnd, pxArray, pxAs, pxAsm, pxBegin, pxCase, pxClass, pxConst,
    pxConstructor, pxDestructor, pxDiv, pxDo, pxDownto, pxElse, pxEnd, pxExcept,
    pxExports, pxFinalization, pxFinally, pxFor, pxFunction, pxGoto, pxIf,
    pxImplementation, pxIn, pxInherited, pxInitialization, pxInline,
    pxInterface, pxIs, pxLabel, pxLibrary, pxMod, pxNil, pxNot, pxObject, pxOf,
    pxOperator, pxOr, pxOut, pxPacked, pxPrivate, pxProcedure, pxProgram,
    pxProperty,
    pxProtected,
    pxPublic, pxPublished, pxRaise,
    pxRecord, pxRepeat, pxResourcestring, pxSet, pxShl, pxShr, pxThen,
    pxThreadvar, pxTo, pxTry, pxType, pxUnit, pxUntil, pxUses, pxVar, pxWhile,
    pxWith, pxXor,
    pxComment,                # ordinary comment
    pxCommand,                # {@}
    pxAmp,                    # {&}
    pxPer,                    # {%}
    pxStrLit, pxSymbol,       # a symbol
    pxIntLit, pxInt64Lit,     # long constant or out of int32 range
    pxFloatLit, pxParLe, pxParRi, pxBracketLe, pxBracketRi, pxComma,
    pxSemiColon, pxColon,     # operators
    pxAsgn, pxEquals, pxDot, pxDotDot, pxHat, pxPlus, pxMinus, pxStar, pxSlash,
    pxLe, pxLt, pxGe, pxGt, pxNeq, pxAt, pxStarDirLe, pxStarDirRi,
    pxCurlyDirLe, pxCurlyDirRi
  TTokKinds* = set[TTokKind]

const
  # keywords are sorted!
  Keywords = ["and", "array", "as", "asm", "begin", "case", "class", "const",
    "constructor", "destructor", "div", "do", "downto", "else", "end", "except",
    "exports", "finalization", "finally", "for", "function", "goto", "if",
    "implementation", "in", "inherited", "initialization", "inline",
    "interface", "is", "label", "library", "mod", "nil", "not", "object", "of",
    "operator", "or", "out", "packed", "private", "procedure", "program",
    "property",
    "protected",
    "public", "published", "raise",
    "record", "repeat", "resourcestring", "set", "shl", "shr", "then",
    "threadvar", "to", "try", "type", "unit", "until", "uses", "var", "while",
    "with", "xor"]

  firstKeyword* = pxAnd
  lastKeyword* = pxXor

type
  TNumericalBase* = enum base10, base2, base8, base16
  TToken* = object
    xkind*: TTokKind          # the type of the token
    ident*: string            # the parsed identifier (raw spelling!)
    iNumber*: int64           # the parsed integer literal
    fNumber*: float64         # the parsed floating point literal
    base*: TNumericalBase     # the numerical base; only valid for int
                              # or float literals
    literal*: string          # the parsed (string) literal
    info*: TLineInfo          # line/col where the token starts

  TLexer* = object of BaseLexer
    filename*: string

proc getTok*(L: var TLexer, tok: var TToken)

proc openLexer*(lex: var TLexer, filename: string) =
  lex.filename = filename
  try:
    var s = openFileStream(filename)
    # pass the stream directly; lexbase takes ownership on close()
    open(lex, s)
  except ErrorCode as e:
    write(stderr, "cannot open file: " & filename & "\n")
    quit(1)

proc closeLexer*(lex: var TLexer) =
  close(lex)

proc curInfo*(L: TLexer): TLineInfo =
  TLineInfo(line: L.lineNumber, col: getColNumber(L, L.bufpos),
            file: L.filename)

proc lexError*(L: TLexer, msg: string) =
  # fatal lex error: report and stop. A translator CLI does not need to
  # recover; keep the model simple (no {.raises.} cascade).
  write(stderr, L.filename & "(" & $L.lineNumber & "," &
        $getColNumber(L, L.bufpos) & ") Error: " & msg & "\n")
  quit(1)

proc tokKindToStr*(k: TTokKind): string =
  case k
  of pxEof: result = "[EOF]"
  of firstKeyword..lastKeyword:
    result = Keywords[ord(k)-ord(firstKeyword)]
  of pxInvalid, pxComment, pxStrLit: result = "string literal"
  of pxCommand: result = "{@"
  of pxAmp: result = "{&"
  of pxPer: result = "{%"
  of pxSymbol: result = "identifier"
  of pxIntLit, pxInt64Lit: result = "integer literal"
  of pxFloatLit: result = "floating point literal"
  of pxParLe: result = "("
  of pxParRi: result = ")"
  of pxBracketLe: result = "["
  of pxBracketRi: result = "]"
  of pxComma: result = ","
  of pxSemiColon: result = ";"
  of pxColon: result = ":"
  of pxAsgn: result = ":="
  of pxEquals: result = "="
  of pxDot: result = "."
  of pxDotDot: result = ".."
  of pxHat: result = "^"
  of pxPlus: result = "+"
  of pxMinus: result = "-"
  of pxStar: result = "*"
  of pxSlash: result = "/"
  of pxLe: result = "<="
  of pxLt: result = "<"
  of pxGe: result = ">="
  of pxGt: result = ">"
  of pxNeq: result = "<>"
  of pxAt: result = "@"
  of pxStarDirLe: result = "(*$"
  of pxStarDirRi: result = "*)"
  of pxCurlyDirLe: result = "{$"
  of pxCurlyDirRi: result = "}"

proc `$`*(tok: TToken): string =
  case tok.xkind
  of pxInvalid, pxComment, pxStrLit: result = tok.literal
  of pxSymbol: result = tok.ident
  of pxIntLit, pxInt64Lit: result = $tok.iNumber
  of pxFloatLit: result = $tok.fNumber
  else: result = tokKindToStr(tok.xkind)

proc printTok*(tok: TToken) =
  writeLine(stdout, $tok)

proc setKeyword(L: var TLexer, tok: var TToken) =
  let lower = tok.ident.toLowerAscii
  var x = -1
  var lo = 0
  var hi = Keywords.len - 1
  while lo <= hi:
    let mid = (lo + hi) div 2
    if Keywords[mid] == lower:
      x = mid
      break
    elif Keywords[mid] < lower:
      lo = mid + 1
    else:
      hi = mid - 1
  if x < 0: tok.xkind = pxSymbol
  else: tok.xkind = TTokKind(x + ord(firstKeyword))

proc matchUnderscoreChars(L: var TLexer, tok: var TToken, chars: set[char]) =
  # matches ([chars]_)*
  var pos = L.bufpos              # use registers for pos, buf
  var buf = L.buf
  while true:
    if buf[pos] in chars:
      tok.literal.add(buf[pos])
      inc(pos)
    else:
      break
    if buf[pos] == '_':
      tok.literal.add('_')
      inc(pos)
  L.bufpos = pos

proc isFloatLiteral(s: string): bool =
  # nimony: an unset `result` is garbage; initialize explicitly
  result = false
  for i in countup(0, len(s)-1):
    if s[i] in {'.', 'e', 'E'}:
      return true

proc getNumber2(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos + 1 # skip %
  if not (L.buf[pos] in {'0'..'1'}):
    # BUGFIX for %date%
    tok.xkind = pxInvalid
    tok.literal.add('%')
    inc(L.bufpos)
    return
  tok.base = base2
  var xi: int64 = 0
  var bits = 0
  while true:
    case L.buf[pos]
    of 'A'..'Z', 'a'..'z', '2'..'9', '.':
      lexError(L, "invalid number")
      inc(pos)
    of '_':
      inc(pos)
    of '0', '1':
      xi = (xi shl 1) or (ord(L.buf[pos]) - ord('0'))
      inc(pos)
      inc(bits)
    else: break
  tok.iNumber = xi
  if bits > 32: tok.xkind = pxInt64Lit
  else: tok.xkind = pxIntLit
  L.bufpos = pos

proc getNumber16(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos + 1          # skip $
  tok.base = base16
  var xi: int64 = 0
  var bits = 0
  while true:
    case L.buf[pos]
    of 'G'..'Z', 'g'..'z', '.':
      lexError(L, "invalid number")
      inc(pos)
    of '_': inc(pos)
    of '0'..'9':
      xi = (xi shl 4) or (ord(L.buf[pos]) - ord('0'))
      inc(pos)
      inc(bits, 4)
    of 'a'..'f':
      xi = (xi shl 4) or (ord(L.buf[pos]) - ord('a') + 10)
      inc(pos)
      inc(bits, 4)
    of 'A'..'F':
      xi = (xi shl 4) or (ord(L.buf[pos]) - ord('A') + 10)
      inc(pos)
      inc(bits, 4)
    else: break
  tok.iNumber = xi
  if bits > 32:
    tok.xkind = pxInt64Lit
  else:
    tok.xkind = pxIntLit
  L.bufpos = pos

proc getNumber10(L: var TLexer, tok: var TToken) =
  tok.base = base10
  matchUnderscoreChars(L, tok, {'0'..'9'})
  if (L.buf[L.bufpos] == '.') and (L.buf[L.bufpos + 1] in {'0'..'9'}):
    tok.literal.add('.')
    inc(L.bufpos)
    matchUnderscoreChars(L, tok, {'e', 'E', '+', '-', '0'..'9'})
  if isFloatLiteral(tok.literal):
    var f: float = 0.0
    if parseBiggestFloat(tok.literal, f) > 0:
      tok.fNumber = f
      tok.xkind = pxFloatLit
    else:
      lexError(L, "invalid number: " & tok.literal)
  else:
    try:
      tok.iNumber = parseInt(tok.literal)
    except:
      lexError(L, "invalid number: " & tok.literal)
    if (tok.iNumber < low(int32)) or (tok.iNumber > high(int32)):
      tok.xkind = pxInt64Lit
    else:
      tok.xkind = pxIntLit

proc handleCRLF(L: var TLexer, pos: int): int =
  case L.buf[pos]
  of '\c': result = lexbase.handleCR(L, pos)
  of '\l': result = lexbase.handleLF(L, pos)
  else: result = pos

proc getString(L: var TLexer, tok: var TToken) =
  var xi: int
  var pos = L.bufpos
  var buf = L.buf
  while true:
    if buf[pos] == '\'':
      inc(pos)
      while true:
        case buf[pos]
        of '\c', '\l', lexbase.EndOfFile:
          lexError(L, "closing quote expected, but end of file reached")
          break
        of '\'':
          inc(pos)
          if buf[pos] == '\'':
            inc(pos)
            tok.literal.add('\'')
          else:
            break
        else:
          tok.literal.add(buf[pos])
          inc(pos)
    elif buf[pos] == '#':
      inc(pos)
      xi = 0
      case buf[pos]
      of '$':
        inc(pos)
        xi = 0
        while true:
          case buf[pos]
          of '0'..'9': xi = (xi shl 4) or (ord(buf[pos]) - ord('0'))
          of 'a'..'f': xi = (xi shl 4) or (ord(buf[pos]) - ord('a') + 10)
          of 'A'..'F': xi = (xi shl 4) or (ord(buf[pos]) - ord('A') + 10)
          else: break
          inc(pos)
      of '0'..'9':
        xi = 0
        while buf[pos] in {'0'..'9'}:
          xi = (xi * 10) + (ord(buf[pos]) - ord('0'))
          inc(pos)
      else:
        lexError(L, "invalid character constant")
      if xi <= 255: tok.literal.add(chr(xi))
      else: lexError(L, "invalid character constant")
    else:
      break
  tok.xkind = pxStrLit
  L.bufpos = pos

proc getSymbol(L: var TLexer, tok: var TToken) =
  # Delphi identifiers: letter followed by letters/digits/underscores.
  # We keep the RAW spelling; keywords are matched case-insensitively.
  var pos = L.bufpos
  var buf = L.buf
  while true:
    var c = buf[pos]
    if c in {'a'..'z', '0'..'9', '_', '\x80'..'\xFF'}:
      inc(pos)
    elif c in {'A'..'Z'}:
      inc(pos)
    else:
      break
  let start = L.bufpos
  tok.ident = newString(pos - start)
  for i in 0 ..< pos - start:
    tok.ident[i] = buf[start + i]
  L.bufpos = pos
  tok.info = curInfo(L)
  setKeyword(L, tok)

proc scanLineComment(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos
  var buf = L.buf
  # a comment ends if the next line does not start with the // on the same
  # column after only whitespace
  tok.xkind = pxComment
  var col = getColNumber(L, pos)
  while true:
    inc(pos, 2)               # skip //
    tok.literal.add('#')
    while not (buf[pos] in {'\c', '\l', lexbase.EndOfFile}):
      tok.literal.add(buf[pos])
      inc(pos)
    pos = handleCRLF(L, pos)
    buf = L.buf
    var indent = 0
    while buf[pos] == ' ':
      inc(pos)
      inc(indent)
    if (col == indent) and (buf[pos] == '/') and (buf[pos + 1] == '/'):
      tok.literal.add("\n")
    else:
      break
  L.bufpos = pos

proc scanCurlyComment(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos
  var buf = L.buf
  tok.literal = "#"
  tok.xkind = pxComment
  while true:
    case buf[pos]
    of '\c', '\l':
      pos = handleCRLF(L, pos)
      buf = L.buf
      tok.literal.add("\n#")
    of '}':
      inc(pos)
      break
    of lexbase.EndOfFile:
      lexError(L, "comment is never closed, expected }")
    else:
      tok.literal.add(buf[pos])
      inc(pos)
  L.bufpos = pos

proc scanStarComment(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos
  var buf = L.buf
  tok.literal = "#"
  tok.xkind = pxComment
  while true:
    case buf[pos]
    of '\c', '\l':
      pos = handleCRLF(L, pos)
      buf = L.buf
      tok.literal.add("\n#")
    of '*':
      inc(pos)
      if buf[pos] == ')':
        inc(pos)
        break
      else:
        tok.literal.add('*')
    of lexbase.EndOfFile:
      lexError(L, "comment is never closed, expected *)")
    else:
      tok.literal.add(buf[pos])
      inc(pos)
  L.bufpos = pos

proc skip(L: var TLexer, tok: var TToken) =
  var pos = L.bufpos
  var buf = L.buf
  while true:
    case buf[pos]
    of ' ', '\t':
      inc(pos)                # newline is special:
    of '\c', '\l':
      pos = handleCRLF(L, pos)
      buf = L.buf
    else:
      break                   # EndOfFile also leaves the loop
  L.bufpos = pos

proc getTok*(L: var TLexer, tok: var TToken) =
  tok.xkind = pxInvalid
  tok.iNumber = 0
  tok.fNumber = 0.0
  tok.literal = ""
  tok.ident = ""
  tok.base = base10
  skip(L, tok)
  tok.info = curInfo(L)
  var c = L.buf[L.bufpos]
  if c in SymStartChars:
    getSymbol(L, tok)
  elif c in {'0'..'9'}:
    getNumber10(L, tok)
  else:
    case c
    of ';':
      tok.xkind = pxSemiColon
      inc(L.bufpos)
    of '/':
      if L.buf[L.bufpos + 1] == '/':
        scanLineComment(L, tok)
      else:
        tok.xkind = pxSlash
        inc(L.bufpos)
    of ',':
      tok.xkind = pxComma
      inc(L.bufpos)
    of '(':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '*':
        if L.buf[L.bufpos + 1] == '$':
          inc(L.bufpos, 2)
          skip(L, tok)
          getSymbol(L, tok)
          tok.xkind = pxStarDirLe
        else:
          inc(L.bufpos)
          scanStarComment(L, tok)
      else:
        tok.xkind = pxParLe
    of '*':
      inc(L.bufpos)
      if L.buf[L.bufpos] == ')':
        inc(L.bufpos)
        tok.xkind = pxStarDirRi
      else:
        tok.xkind = pxStar
    of ')':
      tok.xkind = pxParRi
      inc(L.bufpos)
    of '[':
      inc(L.bufpos)
      tok.xkind = pxBracketLe
    of ']':
      inc(L.bufpos)
      tok.xkind = pxBracketRi
    of '.':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '.':
        tok.xkind = pxDotDot
        inc(L.bufpos)
      else:
        tok.xkind = pxDot
    of '{':
      inc(L.bufpos)
      case L.buf[L.bufpos]
      of '$':
        inc(L.bufpos)
        skip(L, tok)
        getSymbol(L, tok)
        tok.xkind = pxCurlyDirLe
      of '&':
        inc(L.bufpos)
        tok.xkind = pxAmp
      of '%':
        inc(L.bufpos)
        tok.xkind = pxPer
      of '@':
        inc(L.bufpos)
        tok.xkind = pxCommand
      else: scanCurlyComment(L, tok)
    of '+':
      tok.xkind = pxPlus
      inc(L.bufpos)
    of '-':
      tok.xkind = pxMinus
      inc(L.bufpos)
    of ':':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '=':
        inc(L.bufpos)
        tok.xkind = pxAsgn
      else:
        tok.xkind = pxColon
    of '<':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '>':
        inc(L.bufpos)
        tok.xkind = pxNeq
      elif L.buf[L.bufpos] == '=':
        inc(L.bufpos)
        tok.xkind = pxLe
      else:
        tok.xkind = pxLt
    of '>':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '=':
        inc(L.bufpos)
        tok.xkind = pxGe
      else:
        tok.xkind = pxGt
    of '=':
      tok.xkind = pxEquals
      inc(L.bufpos)
    of '@':
      tok.xkind = pxAt
      inc(L.bufpos)
    of '^':
      tok.xkind = pxHat
      inc(L.bufpos)
    of '}':
      tok.xkind = pxCurlyDirRi
      inc(L.bufpos)
    of '\'', '#':
      getString(L, tok)
    of '$':
      getNumber16(L, tok)
    of '%':
      getNumber2(L, tok)
    of lexbase.EndOfFile:
      tok.xkind = pxEof
    else:
      lexError(L, "invalid token " & c & " (\\" & $(ord(c)) & ')')
      inc(L.bufpos)