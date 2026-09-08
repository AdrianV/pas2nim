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
  TDuplicates* = enum
    dupError, dupIgnore, dupAccept   # Delphi order; 0 = dupError so a
                                     # zero-initialized field errors (v1:
                                     # treated like accept, no exception)

  TStringList* = ref object
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
  ## all lines joined with newlines (no setter in v1)
  result = ""
  var k = 0
  while k < self.fLines.len:
    result.add(self.fLines[k] & "\n")
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