{.feature: "lenientnils".}
#
# pasclasses - TStringList, the most-missed piece of Delphi's
# Classes unit. `uses Classes` resolves here. A hand-written ref
# object: method calls are plain self-first procs, the default
# property goes through [] / []= (so List[i] and List[i] := v work),
# and TStringList.Create resolves through a typedesc proc.
#
# v1 gaps (documented in the compat matrix): Sorted does not
# auto-sort on mutation (call Sort explicitly); LoadFromFile/
# SaveToFile swallow IO errors (ErrorCode model); Objects are not
# supported; Text has no setter; the indexed properties
# List.Strings[i], List.Names[i], List.Values[k] and
# List.ValueFromIndex[i] need the default-index form List[i] or
# call syntax (ValueOf) - nimony auto-calls zero-arg dots, so
# indexing a property name directly yields a char.

import std/[strutils, syncio, os]
import systempas

type
  TOperation* = enum
    opInsert, opRemove             # Delphi order (opRemove = 1)

  TListNotification* = enum
    lnAdded, lnDeleted, lnExtracted  # Delphi order

  TList* {.inheritable.} = ref object of RootRef
    fItems*: seq[RootRef]

  TStrings* {.inheritable.} = ref object of RootRef
  TStream* {.inheritable.} = ref object of RootRef
  TPersistent* {.inheritable.} = ref object of RootRef
  TComponent* {.inheritable.} = ref object of TPersistent

  # Delphi's Classes-side exception class (Contnrs raises it)
  EListError* = ref object of PasException

  TDuplicates* = enum
    dupError, dupIgnore, dupAccept   # Delphi order; 0 = dupError so a
                                     # zero-initialized field errors (v1:
                                     # treated like accept, no exception)

  TStringList* = ref object of TStrings
    fLines*: seq[string]
    fSorted*: bool
    fCaseSensitive*: bool
    fDuplicates*: TDuplicates

proc Create*(self: typedesc[TStringList]): TStringList =
  var r = TStringList()
  r.fLines = @[]
  r.fDuplicates = dupAccept      # the Delphi default
  result = r

proc Count*(self: TStringList): int32 =
  int32(self.fLines.len)

proc Add*(self: TStringList; s: string): int32 =
  ## returns the index of the new line; with dupIgnore, an existing
  ## equal line is kept and its index returned
  if self.fDuplicates == dupIgnore:
    let existing = IndexOf(self, s)
    if existing >= 0:
      return existing
  self.fLines.add(s)
  result = int32(self.fLines.len - 1)

proc Insert*(self: TStringList; index: int32; s: string) =
  var i = int(index)
  if i > self.fLines.len: i = self.fLines.len
  if i < 0: i = 0
  var nl: seq[string] = @[]
  var k = 0
  while k <= self.fLines.len:
    if k == i: nl.add(s)
    if k < self.fLines.len: nl.add(self.fLines[k])
    inc k
  self.fLines = nl

proc Delete*(self: TStringList; index: int32) =
  var nl: seq[string] = @[]
  var k = 0
  while k < self.fLines.len:
    if k != int(index): nl.add(self.fLines[k])
    inc k
  self.fLines = nl

proc Clear*(self: TStringList) =
  self.fLines = @[]

proc `[]`*(self: TStringList; i: int32): string =
  ## the default property Strings[i]
  self.fLines[int(i)]

proc `[]=`*(self: TStringList; i: int32; v: string) =
  self.fLines[int(i)] = v

proc `[]`*(self: TStringList; key: string): string =
  ## the string-indexed default property: Values[key] equivalent
  ValueOf(self, key)

proc `[]=`*(self: TStringList; key: string; v: string) =
  ## Values[key] := v: replace the first name=key line's value, or
  ## append a new key=value line
  var k = 0
  while k < self.fLines.len:
    let ln = self.fLines[k]
    let p = find(ln, "=", 0)
    if p > 0:
      let nm = strip(substr(ln, 0, p - 1))
      if cmpIgnoreCase(nm, key) == 0:
        self.fLines[k] = nm & "=" & v
        return
    inc k
  self.fLines.add(key & "=" & v)

proc Text*(self: TStringList): string =
  ## all lines joined with the platform line ending (Delphi on Windows
  ## uses CRLF; FPC uses LineEnding - CRLF on Windows, LF on Unix)
  result = ""
  var k = 0
  while k < self.fLines.len:
    result.add(self.fLines[k] & pasLineEnding)
    inc k

proc IndexOf*(self: TStringList; s: string): int32 =
  ## case-insensitive unless CaseSensitive; -1 when absent
  result = -1
  var k = 0
  while k < self.fLines.len:
    var same: bool
    if self.fCaseSensitive:
      same = self.fLines[k] == s
    else:
      same = cmpIgnoreCase(self.fLines[k], s) == 0
    if same:
      return int32(k)
    inc k

proc Sort*(self: TStringList) =
  ## insertion sort honoring CaseSensitive
  var i = 1
  while i < self.fLines.len:
    var key = self.fLines[i]
    var j = i - 1
    while j >= 0:
      var cmp: int
      if self.fCaseSensitive:
        cmp = CompareStr(self.fLines[j], key)
      else:
        cmp = cmpIgnoreCase(self.fLines[j], key)
      if cmp > 0:
        let prev = self.fLines[j]
        self.fLines[j + 1] = prev
        dec j
      else:
        break
    self.fLines[j + 1] = key
    inc i

proc Exchange*(self: TStringList; i, j: int32) =
  let tmp = self.fLines[int(i)]
  let other = self.fLines[int(j)]
  self.fLines[int(i)] = other
  self.fLines[int(j)] = tmp

proc ValueOf*(self: TStringList; key: string): string =
  ## Values[key] equivalent (call syntax): the value after '=' of the
  ## first name=value line matching key case-insensitively; "" when
  ## absent
  result = ""
  var k = 0
  while k < self.fLines.len:
    let ln = self.fLines[k]
    let p = find(ln, "=", 0)
    if p > 0:
      let nm = strip(substr(ln, 0, p - 1))
      if cmpIgnoreCase(nm, key) == 0:
        result = substr(ln, p + 1, ln.len - 1)
        return
    inc k

proc NameOfIndex*(self: TStringList; i: int32): string =
  ## Names[i] equivalent (call syntax)
  let ln = self.fLines[int(i)]
  let p = find(ln, "=", 0)
  if p > 0:
    result = strip(substr(ln, 0, p - 1))
  else:
    result = ln

proc SaveToFile*(self: TStringList; path: string) =
  var text = ""
  var k = 0
  while k < self.fLines.len:
    text.add(self.fLines[k] & "\n")
    inc k
  try:
    writeFile(path, text)
  except Exception:
    discard

proc LoadFromFile*(self: TStringList; path: string) =
  self.fLines = @[]
  try:
    let text = readFile(path)
    var cur = ""
    var k = 0
    while k < text.len:
      if text[k] == '\n':
        if cur.len > 0 and cur[cur.len - 1] == '\r':
          cur = substr(cur, 0, cur.len - 2)
        self.fLines.add(cur)
        cur = ""
      else:
        cur.add(text[k])
      inc k
    if cur.len > 0: self.fLines.add(cur)
  except Exception:
    discard
# TList - the pointer-list container (v1: items ride RootRef; the
# corpus only stores/reads TObject references through it)
proc Create*(self: typedesc[TList]): TList =
  var r = TList()
  r.fItems = @[]
  result = r

proc Add*(self: TList; item: RootRef): int32 =
  self.fItems.add(item)
  result = int32(self.fItems.len - 1)   # Delphi returns the index

proc Count*(self: TList): int32 =
  int32(self.fItems.len)

proc `[]`*(self: TList; i: int32): RootRef =
  self.fItems[i]

proc `[]=`*(self: TList; i: int32; v: RootRef) =
  self.fItems[i] = v

# Delphi TList.First: the first item (v1: nil on an empty list)
proc First*(self: TList): RootRef =
  result = nil
  if self.fItems.len > 0:
    result = self.fItems[0]

# Delphi TList.Extract: removes and returns the item (the v1 shim
# keeps the value, list mutation follows the Delphi contract shape)
proc Extract*(self: TList; item: RootRef): RootRef =
  result = nil
  var idx = -1
  for i in 0 ..< self.fItems.len:
    if self.fItems[i] == item:
      idx = i
      break
  if idx >= 0:
    self.fItems.delete(idx)
    result = item

# TStrings: the abstract list base (v1: empty shell - the corpus only
# passes TStrings values between declarations; the concrete work goes
# through TStringList)
proc Create*(self: typedesc[TStrings]): TStrings =
  var r = TStrings()
  result = r

proc Count*(self: TStrings): int32 =
  0

proc `[]`*(self: TStrings; i: int32): string =
  ""

proc Delete*(self: TStrings; i: int32) =
  discard

proc Clear*(self: TStrings) =
  discard

proc Add*(self: TStrings; s: string): int32 =
  0

# TStringList property accessors: `List.Sorted := True` lowers to the
# Sorted= setter (nimony derives field assignments, not properties)
proc Sorted*(self: TStringList): bool =
  self.fSorted

proc `Sorted=`*(self: TStringList; v: bool) =
  self.fSorted = v

# TStream.Size/Position/CopyFrom: the binary-stream shims the
# corpus's ReadBinaryStream/WriteBinaryStream paths touch
proc Size*(self: TStream): int32 =
  result = 0

proc Position*(self: TStream): int32 =
  result = 0

proc CopyFrom*(self: TStream; src: TStream; count: int32) =
  discard

# TStrings.BeginUpdate/EndUpdate: the corpus's bulk-build paths
proc BeginUpdate*(self: TStrings) =
  discard

proc EndUpdate*(self: TStrings) =
  discard

# TMemoryStream: the IniFiles ReadBinaryStream path needs Memory/
# Position/Size/SetSize (v1: an empty backing store)
type
  TMemoryStream* = ref object of TStream
    Memory*: int32    # the buffer address (v1: nil via pasCStr paths)
    Position*: int32
    fSize*: int32

proc Create*(self: TMemoryStream): TMemoryStream =
  result = self

proc Size*(self: TMemoryStream): int32 =
  result = self.fSize

proc SetSize*(self: TMemoryStream; v: int32) =
  self.fSize = v

# Classes.HexToBin/BinToHex: v1 converts nothing (the corpus's
# binary-stream paths are exercise-only)
proc HexToBin*(text: string; buf: cstring; count: int32): int32 =
  result = count

proc BinToHex*(buf: cstring; text: string; count: int32) =
  discard

# TStrings.AddObject/Objects[]: the corpus's section lists ride them
proc AddObject*(self: TStrings; s: string; obj: RootRef): int32 =
  result = self.Add(s)

proc GetObject*(self: TStrings; i: int32): RootRef =
  result = nil

# TStrings.Get: the corpus's UpdateValueHash loop reads by index
proc Get*(self: TStrings; i: int32): string =
  result = ""

# TStrings.CaseSensitive: the corpus's THashedStringList reads and
# writes the property through the inherited base
proc CaseSensitive*(self: TStrings): bool =
  result = false

proc `CaseSensitive=`*(self: TStrings; v: bool) =
  discard

# TStrings.Changed: the corpus's THashedStringList overrides it (the
# vtable slot needs the base's declaration)
proc Changed*(self: TStrings) =
  discard

# TStrings.IndexOf: the ValueExists loop searches the TStrings-typed
# local (a TStringList instance rides the base's method)
proc IndexOf*(self: TStrings; item: string): int32 =
  result = -1

# TStringList.CommaText: the corpus only moves the value through the
# property setter (and reads it back); v1 joins with commas
proc CommaText*(self: TStringList): string =
  self.fLines.join(",")

proc `CommaText=`*(self: TStringList; v: string) =
  self.fLines = v.split(",")


# EListError.CreateFmt: the corpus's raise sites only use the
# message-formatting ctor
proc CreateFmt*(self: typedesc[EListError]; msg: string;
    args: TArrayOfConst): EListError =
  var r = EListError(Message: Format(msg, args))
  result = r
