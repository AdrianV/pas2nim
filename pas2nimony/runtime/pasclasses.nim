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
    fItems*: seq[pointer]

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

proc Put*(self: TStringList; key: string; v: string) =
  ## the string-indexed setter call: `List.Strings['key'] := v` lowers to
  ## this (pasnimout.stmt / pasnifout.emitStmt map the indexed property to
  ## its accessor call, because nimony has no indexed-property form).
  ## Same body as `[]=`, which the direct `List['key'] := v` spelling uses.
  `[]=`(self, key, v)

proc Put*(self: TStringList; i: int32; s: string) =
  ## the integer-indexed setter call: `List.Strings[i] := s` lowers to
  ## this. The base `TStrings.Put` is only a stub (the abstract base has no
  ## storage), so the concrete override is what makes the property work -
  ## an inherited stub would silently discard the write.
  self.fLines[i] = s

proc Get*(self: TStringList; i: int32): string =
  ## the integer-indexed getter call: `List.Strings[i]` lowers to this.
  ## Same reason as `Put`: without it the base's stub returns "".
  self.fLines[int(i)]

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
# TComponent notification hook: the corpus's TComponentListNexus
# overrides it. The ctor mirrors Delphi's TComponent.Create(AOwner) on
# a preallocated instance so subclass ctor calls dispatch here through
# the ancestor (nimsem rejects a descendant instance against
# typedesc[TComponent]; the instance form upcasts freely)
proc Create*(self: typedesc[TComponent]): TComponent =
  var r = TComponent()
  result = r

proc Create*(self: TComponent; AOwner: TComponent): TComponent =
  result = self

method FreeNotification(self: TComponent; AComponent: TComponent) =
  ## Delphi's TComponent.FreeNotification: the corpus's
  ## TComponentList.Notify registers a free-notification link; the v1
  ## shim discards (no destruction machinery to hook)
  discard

method Notification*(self: TComponent; AComponent: TComponent;
                     Operation: TOperation) =
  ## the ancestor notification hook (the corpus's TComponentListNexus
  ## overrides it)
  discard

# `TObject(Ptr).Free` / `TObject(Ptr).FreeNotification(x)` on a
# POINTER-typed receiver (Contnrs' TObjectList.Notify, TComponentList):
# the WIP translator lowers these to pasFreeObj / pasFreeNotification
# (paspars.mapStringBuiltins), and **neither helper can be written yet**.
#
# nimony resolves a method call in the *static* receiver's vtable, and
# `cast[RootRef](p)` makes that receiver RootRef - whose vtable is the
# builtin RootObj's, with no entries and no way to add one from a shim.
# So `Destroy(cast[RootRef](p))` is a hard error:
#   [Error] method `Destroy` not found in class RootObj
# Declaring method roots on TList/TStrings/TStream/TPersistent does not
# help (verified), and dropping the cast would lose the virtual dispatch
# the whole lowering exists for. A module that merely *contains* such a
# helper fails to build even when it never calls it, so there is no
# partial shim to ship - the lowering needs the receiver's real class.
#
# Until then the translator must not emit these two names; Contnrs'
# TObjectList.Notify and TComponentList.Notify are the only corpus
# callers. See doc/toolchain.md lesson 41.
proc fItemsSame(p: pointer; r: RootRef): bool =

  cast[pointer](r) == p

proc Create*(self: typedesc[TList]): TList =
  var r = TList()
  r.fItems = @[]
  result = r

proc Add*(self: TList; item: pointer): int32 =
  # Delphi's TList is a pointer list; the corpus's object wrappers cast
  self.fItems.add(item)
  result = int32(self.fItems.len - 1)   # Delphi returns the index

proc Add*(self: TList; item: RootRef): int32 =
  ## the corpus's TList.Add(TObject) wrapper argument
  self.fItems.add(cast[pointer](item))
  result = int32(self.fItems.len - 1)

proc Delete*(self: TList; index: int32) =
  # Delphi TList.Delete: the corpus's TOrderedList.Delete forwards here
  self.fItems.delete(int(index))

proc Count*(self: TList): int32 =
  int32(self.fItems.len)

proc `[]`*(self: TList; i: int32): pointer =
  result = cast[pointer](self.fItems[i])

proc `[]=`*(self: TList; i: int32; v: RootRef) =
  self.fItems[i] = cast[pointer](v)

proc Last*(self: TList): pointer =
  # Delphi TList.Last: the last item (v1: nil on an empty list)
  if self.fItems.len > 0:
    self.fItems[self.fItems.len - 1]
  else:
    nil

proc Notify*(self: TList; pv: pointer; action: TListNotification) =
  ## the ancestor notification hook (the corpus's TObjectList overrides
  ## it; the shim's TList stores objects directly, nothing to do here)
  discard

proc Remove*(self: TList; item: RootRef): int32 =
  var k = 0
  while k < self.fItems.len:
    if fItemsSame(self.fItems[k], item):
      self.fItems.delete(k)
      return int32(k)
    inc k
  result = -1

proc Insert*(self: TList; i: int32; item: pointer) =
  # the pointer overload mirrors the RootRef insert (no nimony seq insert)
  var old = self.fItems
  if i >= old.len:
    self.fItems.add(item)
  else:
    self.fItems = @[]
    var k = 0
    while k < i:
      self.fItems.add(old[k])
      inc k
    self.fItems.add(item)
    k = i
    while k < old.len:
      self.fItems.add(old[k])
      inc k

proc IndexOf*(self: TList; item: RootRef): int32 =
  for k in 0 ..< self.fItems.len:
    if self.fItems[k] == cast[pointer](item):
      return int32(k)
  result = -1

proc Insert*(self: TList; i: int32; item: RootRef) =
  # a middle insert shifts the tail entries (while loops: nimony's
  # slice iteration over mixed int widths resolves to the set op)
  var old = self.fItems
  if i >= old.len:
    self.fItems.add(cast[pointer](item))
  else:
    self.fItems = @[]
    var k = 0
    while k < i:
      self.fItems.add(old[k])
      inc k
    self.fItems.add(cast[pointer](item))
    k = i
    while k < old.len:
      self.fItems.add(old[k])
      inc k

# Delphi TList.First: the first item (v1: nil on an empty list)
proc First*(self: TList): pointer =
  result = nil
  if self.fItems.len > 0:
    result = self.fItems[0]

# Delphi TList.Extract: removes and returns the item (the v1 shim
# keeps the value, list mutation follows the Delphi contract shape)
proc Extract*(self: TList; item: RootRef): RootRef =
  result = nil
  var idx = -1
  for i in 0 ..< self.fItems.len:
    if self.fItems[i] == cast[pointer](item):
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
proc Assign*(self: TStrings; src: TStrings) =
  ## v1 stub on the abstract TStrings base
  discard

proc Put*(self: TStrings; i: int32; s: string) =
  ## v1 stub on the abstract TStrings base
  discard

proc PutObject*(self: TStrings; i: int32; obj: RootRef) =
  ## v1 stub on the abstract TStrings base
  discard

proc GetName*(self: TStrings; i: int32): string =
  ## v1 stub on the abstract TStrings base (the concrete split of the
  ## `name=value` line lives on TStringList)
  result = ""

proc GetText*(self: TStrings): string =
  ## v1 stub: the Text read on the abstract base
  result = ""

proc IndexOfName*(self: TStrings; s: string): int32 =
  ## v1 stub on the abstract TStrings base (the concrete list storage
  ## lives on TStringList, mirroring the IndexOf stub above)
  result = -1

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
