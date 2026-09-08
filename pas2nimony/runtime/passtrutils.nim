{.feature: "lenientnils".}
#
# passtrutils - Delphi StrUtils unit shim for pas2nimony
#
# A thin layer over std/strutils plus small re-implementations where
# the Delphi semantics differ (1-based MidStr, case-insensitive ops).
#
# Nimony notes baked in (same as systempas):
#  - `proc` (not `func`): helpers here mutate locals
#  - no {.raises.}: the ErrorCode exception model
#  - `result = ""` before building strings incrementally

import std/strutils

proc LeftStr*(s: string; count: int32): string =
  ## first count characters of s
  result = ""
  var n = int(count)
  if n > s.len: n = s.len
  if n > 0: result = substr(s, 0, n - 1)

proc RightStr*(s: string; count: int32): string =
  ## last count characters of s
  result = ""
  var n = int(count)
  if n > s.len: n = s.len
  if n > 0: result = substr(s, s.len - n, s.len - 1)

proc MidStr*(s: string; start, count: int32): string =
  ## Delphi MidStr: 1-based start, count characters
  result = ""
  var b = int(start) - 1
  var n = int(count)
  if b < 0: b = 0
  if b + n > s.len: n = s.len - b
  if n > 0: result = substr(s, b, b + n - 1)

proc PosEx*(sub, s: string; offset: int64 = 1): int32 =
  ## Delphi PosEx: Pos starting at the 1-based offset; 0 when absent
  ## (int64 param: nimony does not narrow negative int literals)
  if offset < 1: return 0
  result = int32(find(s, sub, int(offset) - 1) + 1)

proc AnsiReplaceStr*(s, fromText, toText: string): string =
  ## case-sensitive replace (Delphi AnsiReplaceStr)
  replace(s, fromText, toText)

proc AnsiReplaceStr*(s, fromText: string; toText: char): string =
  ## char overloads: Pascal 1-char literals are chars and nimony does
  ## not convert char -> string implicitly
  replace(s, fromText, toText & "")

proc AnsiReplaceStr*(s: string; fromText: char; toText: string): string =
  replace(s, fromText & "", toText)

proc AnsiReplaceStr*(s: string; fromText, toText: char): string =
  replace(s, fromText & "", toText & "")

proc ReplaceStr*(s, fromText, toText: string): string =
  AnsiReplaceStr(s, fromText, toText)

proc AnsiReplaceText*(s, fromText, toText: string): string =
  ## case-insensitive replace
  result = ""
  if fromText.len == 0:
    result = s
    return
  var i = 0
  while i < s.len:
    if i + fromText.len <= s.len and
        cmpIgnoreCase(substr(s, i, i + fromText.len - 1), fromText) == 0:
      result.add(toText)
      i = i + fromText.len
    else:
      result.add(s[i])
      inc i

proc AnsiReplaceText*(s: string; fromText, toText: char): string =
  AnsiReplaceText(s, fromText & "", toText & "")

proc AnsiReplaceText*(s, fromText: string; toText: char): string =
  AnsiReplaceText(s, fromText, toText & "")

proc AnsiReplaceText*(s: string; fromText: char; toText: string): string =
  AnsiReplaceText(s, fromText & "", toText)

proc ReplaceText*(s, fromText, toText: string): string =
  AnsiReplaceText(s, fromText, toText)

proc ReplaceText*(s: string; fromText, toText: char): string =
  AnsiReplaceText(s, fromText & "", toText & "")

proc ReverseString*(s: string): string =
  result = ""
  var i = s.len
  while i > 0:
    dec i
    result.add(s[i])

proc DupeString*(s: string; count: int64): string =
  result = ""
  if count > 0: result = repeat(s, int(count))

proc AnsiSameText*(a, b: string): bool =
  cmpIgnoreCase(a, b) == 0

proc StartsText*(a, s: string): bool =
  ## true when s starts with a, ignoring case
  if a.len > s.len: return false
  result = cmpIgnoreCase(substr(s, 0, a.len - 1), a) == 0

proc EndsText*(a, s: string): bool =
  ## true when s ends with a, ignoring case
  if a.len > s.len: return false
  result = cmpIgnoreCase(substr(s, s.len - a.len, s.len - 1), a) == 0

proc IfThen*(val: bool; trueVal, falseVal: string): string =
  result = ""
  if val: result = trueVal else: result = falseVal

proc IfThen*(val: bool; trueVal, falseVal: int32): int32 =
  if val: trueVal else: falseVal

type
  TStringArray* = seq[string]   # FPC's SplitString result type

proc SplitString*(s: string; delimiters: char): seq[string] =
  ## 1-char Pascal literal
  SplitString(s, $delimiters)

proc SplitString*(s, delimiters: string): seq[string] =
  ## Delphi SplitString: every character of delimiters is a separator
  result = @[]
  var cur = ""
  var i = 0
  while i < s.len:
    var isSep = false
    var j = 0
    while j < delimiters.len:
      if s[i] == delimiters[j]:
        isSep = true
        break
      inc j
    if isSep:
      result.add(cur)
      cur = ""
    else:
      cur.add(s[i])
    inc i
  result.add(cur)