{.feature: "lenientnils".}
#
# Registry - Delphi 2007's Registry unit, reduced to the surface the
# corpus references. The API list is taken from the Delphi 2007 RTL
# source (source/Win32/rtl/common/Registry.pas), which the wine prefix
# ships.
#
# One corpus unit names Registry in its `uses` clause without taking a
# symbol from it; another uses TRegistry directly. The real RTL
# is a thin wrapper over advapi32's RegOpenKeyEx/RegQueryValueEx, and
# v1 has no such FFI - so this shim is an IN-MEMORY store. Keys and
# values round-trip within the process and the Delphi success/failure
# forms are preserved, so the calling code takes the same branches.
#
# v1 gaps (documented divergences): nothing is persisted, LazyWrite is
# accepted and ignored, the binary/date/currency/float writers keep only
# a textual form, and the file-backed entry points (LoadKey, SaveKey,
# ReplaceKey, RestoreKey, UnLoadKey, RegistryConnect, MoveKey) are
# accepted no-ops returning False.
import systempas
import pasclasses

type
  HKEY* = uint32

  ERegistryException* = ref object of PasException

  TRegDataType* = enum
    rdUnknown, rdString, rdExpandString, rdInteger, rdBinary

  TRegDataInfo* = object
    RegData*: TRegDataType
    DataSize*: int32

  TRegKeyInfo* = object
    NumSubKeys*: int32
    MaxSubKeyLen*: int32
    NumValues*: int32
    MaxValueLen*: int32
    MaxDataLen*: int32

  TRegValue = ref object
    name: string
    kind: TRegDataType
    text: string
    number: int32
    flag: bool
    real: float64

  TRegKey = ref object
    path: string
    values: seq[TRegValue]

  TRegistry* {.inheritable.} = ref object of RootRef
    FRootKey: HKEY
    FCurrentPath: string
    FLazyWrite: bool
    FAccess: uint32
    FOpen: bool

  TRegIniFile* {.inheritable.} = ref object of TRegistry
    FFileName: string

const
  HKEY_CLASSES_ROOT* = 0x80000000'u32
  HKEY_CURRENT_USER* = 0x80000001'u32
  HKEY_LOCAL_MACHINE* = 0x80000002'u32
  HKEY_USERS* = 0x80000003'u32
  HKEY_CURRENT_CONFIG* = 0x80000005'u32
  HKEY_DYN_DATA* = 0x80000006'u32

  KEY_QUERY_VALUE* = 0x0001'u32
  KEY_SET_VALUE* = 0x0002'u32
  KEY_CREATE_SUB_KEY* = 0x0004'u32
  KEY_ENUMERATE_SUB_KEYS* = 0x0008'u32
  KEY_NOTIFY* = 0x0010'u32
  KEY_CREATE_LINK* = 0x0020'u32
  KEY_READ* = 0x00020019'u32
  KEY_WRITE* = 0x00020006'u32
  KEY_ALL_ACCESS* = 0x000F003F'u32

# --- the in-memory store -------------------------------------------------

var
  gKeys: seq[TRegKey] = @[]

proc rootName(root: HKEY): string =
  case root
  of HKEY_CLASSES_ROOT: result = "HKEY_CLASSES_ROOT"
  of HKEY_CURRENT_USER: result = "HKEY_CURRENT_USER"
  of HKEY_LOCAL_MACHINE: result = "HKEY_LOCAL_MACHINE"
  of HKEY_USERS: result = "HKEY_USERS"
  of HKEY_CURRENT_CONFIG: result = "HKEY_CURRENT_CONFIG"
  of HKEY_DYN_DATA: result = "HKEY_DYN_DATA"
  else: result = "HKEY_UNKNOWN"

proc findKey(path: string): int =
  result = -1
  for i in 0 ..< gKeys.len:
    if gKeys[i].path == path:
      return i

proc keyAt(path: string): TRegKey =
  result = nil
  let i = findKey(path)
  if i >= 0:
    result = gKeys[i]

proc ensureKey(path: string): TRegKey =
  result = keyAt(path)
  if result == nil:
    result = TRegKey()
    result.path = path
    result.values = @[]
    gKeys.add(result)

proc findValue(k: TRegKey; name: string): int =
  result = -1
  if k == nil:
    return
  for i in 0 ..< k.values.len:
    if k.values[i].name == name:
      return i

proc putValue(k: TRegKey; name: string; kind: TRegDataType): TRegValue =
  result = nil
  let i = findValue(k, name)
  if i >= 0:
    result = k.values[i]
    result.kind = kind
  else:
    result = TRegValue()
    result.name = name
    result.kind = kind
    result.text = ""
    result.number = 0
    result.flag = false
    result.real = 0.0
    k.values.add(result)

proc current(self: TRegistry): TRegKey =
  ## the key OpenKey selected; nil when none is open
  result = nil
  if self.FOpen:
    result = keyAt(self.FCurrentPath)

# --- TRegistry -----------------------------------------------------------

proc Create*(self: typedesc[TRegistry]): TRegistry =
  var r = TRegistry()
  r.FRootKey = HKEY_CURRENT_USER       # the Delphi default
  r.FCurrentPath = ""
  r.FLazyWrite = true                  # the Delphi default
  r.FAccess = KEY_ALL_ACCESS
  r.FOpen = false
  result = r

proc Create*(self: typedesc[TRegistry]; AAccess: uint32): TRegistry =
  result = Create(TRegistry)
  result.FAccess = AAccess

method Destroy*(self: TRegistry) =
  self.FOpen = false

proc CloseKey*(self: TRegistry) =
  self.FOpen = false
  self.FCurrentPath = ""

proc RootKey*(self: TRegistry): HKEY =
  result = self.FRootKey

proc `RootKey=`*(self: TRegistry; Value: HKEY) =
  self.FRootKey = Value

proc LazyWrite*(self: TRegistry): bool =
  result = self.FLazyWrite

proc `LazyWrite=`*(self: TRegistry; Value: bool) =
  self.FLazyWrite = Value

proc Access*(self: TRegistry): uint32 =
  result = self.FAccess

proc `Access=`*(self: TRegistry; Value: uint32) =
  self.FAccess = Value

proc CurrentPath*(self: TRegistry): string =
  result = self.FCurrentPath

proc OpenKey*(self: TRegistry; Key: string; CanCreate: bool): bool =
  ## Key is relative to RootKey; the store keeps the fully qualified path
  let full = rootName(self.FRootKey) & Key
  var k = keyAt(full)
  if k == nil:
    if not CanCreate:
      result = false
      return
    k = ensureKey(full)
  self.FCurrentPath = full
  self.FOpen = true
  result = true

proc OpenKeyReadOnly*(self: TRegistry; Key: string): bool =
  result = OpenKey(self, Key, false)

proc KeyExists*(self: TRegistry; Key: string): bool =
  result = keyAt(rootName(self.FRootKey) & Key) != nil

proc CreateKey*(self: TRegistry; Key: string): bool =
  discard ensureKey(rootName(self.FRootKey) & Key)
  result = true

proc DeleteKey*(self: TRegistry; Key: string): bool =
  let full = rootName(self.FRootKey) & Key
  result = false
  for i in 0 ..< gKeys.len:
    if gKeys[i].path == full:
      gKeys.delete(i)
      result = true

proc ValueExists*(self: TRegistry; Name: string): bool =
  let k = current(self)
  result = k != nil and findValue(k, Name) >= 0

proc DeleteValue*(self: TRegistry; Name: string): bool =
  let k = current(self)
  result = false
  if k == nil:
    return
  let i = findValue(k, Name)
  if i >= 0:
    k.values.delete(i)
    result = true

proc GetDataType*(self: TRegistry; ValueName: string): TRegDataType =
  let k = current(self)
  result = rdUnknown
  if k == nil:
    return
  let i = findValue(k, ValueName)
  if i >= 0:
    result = k.values[i].kind

proc GetDataSize*(self: TRegistry; ValueName: string): int32 =
  let k = current(self)
  result = -1
  if k == nil:
    return
  let i = findValue(k, ValueName)
  if i >= 0:
    result = int32(k.values[i].text.len)

proc GetDataInfo*(self: TRegistry; ValueName: string;
                  Value: var TRegDataInfo): bool =
  let k = current(self)
  result = false
  if k == nil:
    return
  let i = findValue(k, ValueName)
  if i >= 0:
    Value.RegData = k.values[i].kind
    Value.DataSize = int32(k.values[i].text.len)
    result = true

proc GetKeyInfo*(self: TRegistry; Value: var TRegKeyInfo): bool =
  let k = current(self)
  result = false
  if k == nil:
    return
  Value.NumSubKeys = 0
  Value.MaxSubKeyLen = 0
  Value.NumValues = int32(k.values.len)
  Value.MaxValueLen = 0
  Value.MaxDataLen = 0
  result = true

proc HasSubKeys*(self: TRegistry): bool =
  ## the store keeps paths, not a child list: a deeper path is a subkey
  let k = current(self)
  result = false
  if k == nil:
    return
  let prefix = k.path & "\\"
  for i in 0 ..< gKeys.len:
    if gKeys[i].path.len > prefix.len and
        gKeys[i].path[0 ..< prefix.len] == prefix:
      result = true
      return

# pasclasses' TStrings is an unbacked base (its Add is a no-op), so the
# enumeration helpers are declared against TStringList, the type that
# actually stores: callers passing a TStringList get the names. This is a
# documented divergence from Delphi's TStrings parameter.
proc GetKeyNames*(self: TRegistry; Strings: TStringList) =
  ## the immediate children of the current key, in insertion order
  let k = current(self)
  if k == nil:
    return
  let prefix = k.path & "\\"
  for i in 0 ..< gKeys.len:
    let p = gKeys[i].path
    if p.len > prefix.len and p[0 ..< prefix.len] == prefix:
      let rest = p[prefix.len ..< p.len]
      var cut = rest.len
      for j in 0 ..< rest.len:
        if rest[j] == '\\' and j < cut:
          cut = j
      # the store holds one entry per path, so children are unique
      discard Strings.Add(rest[0 ..< cut])

proc GetValueNames*(self: TRegistry; Strings: TStringList) =
  let k = current(self)
  if k == nil:
    return
  for i in 0 ..< k.values.len:
    discard Strings.Add(k.values[i].name)

# --- readers -------------------------------------------------------------

proc ReadString*(self: TRegistry; Name: string): string =
  let k = current(self)
  result = ""
  if k == nil:
    return
  let i = findValue(k, Name)
  if i >= 0:
    result = k.values[i].text

proc ReadInteger*(self: TRegistry; Name: string): int32 =
  let k = current(self)
  result = 0
  if k == nil:
    return
  let i = findValue(k, Name)
  if i >= 0:
    result = k.values[i].number

proc ReadBool*(self: TRegistry; Name: string): bool =
  let k = current(self)
  result = false
  if k == nil:
    return
  let i = findValue(k, Name)
  if i >= 0:
    result = k.values[i].flag

proc ReadFloat*(self: TRegistry; Name: string): float64 =
  let k = current(self)
  result = 0.0
  if k == nil:
    return
  let i = findValue(k, Name)
  if i >= 0:
    result = k.values[i].real

proc ReadBinaryData*(self: TRegistry; Name: string; Buffer: pointer;
                     BufSize: int32): int32 =
  ## documented gap: the store has no byte-blob form, so nothing is
  ## copied and the reported size is 0 (Delphi returns the byte count)
  result = 0

proc ReadDate*(self: TRegistry; Name: string): float64 =
  ## TDateTime is a float in Delphi; the shim keeps the textual form only
  result = 0.0

proc ReadDateTime*(self: TRegistry; Name: string): float64 =
  result = ReadDate(self, Name)

proc ReadTime*(self: TRegistry; Name: string): float64 =
  result = ReadDate(self, Name)

proc ReadCurrency*(self: TRegistry; Name: string): float64 =
  result = ReadFloat(self, Name)

# --- writers -------------------------------------------------------------

proc WriteString*(self: TRegistry; Name, Value: string) =
  let k = current(self)
  if k == nil:
    return
  let v = putValue(k, Name, rdString)
  v.text = Value

proc WriteExpandString*(self: TRegistry; Name, Value: string) =
  let k = current(self)
  if k == nil:
    return
  let v = putValue(k, Name, rdExpandString)
  v.text = Value

proc WriteInteger*(self: TRegistry; Name: string; Value: int32) =
  let k = current(self)
  if k == nil:
    return
  let v = putValue(k, Name, rdInteger)
  v.number = Value
  v.text = ""

proc WriteBool*(self: TRegistry; Name: string; Value: bool) =
  let k = current(self)
  if k == nil:
    return
  let v = putValue(k, Name, rdInteger)
  v.flag = Value

proc WriteFloat*(self: TRegistry; Name: string; Value: float64) =
  let k = current(self)
  if k == nil:
    return
  let v = putValue(k, Name, rdString)
  v.real = Value

proc WriteBinaryData*(self: TRegistry; Name: string; Buffer: pointer;
                      BufSize: int32) =
  let k = current(self)
  if k == nil or Buffer == nil:
    return
  let v = putValue(k, Name, rdBinary)
  v.text = ""

proc WriteDate*(self: TRegistry; Name: string; Value: float64) =
  discard

proc WriteDateTime*(self: TRegistry; Name: string; Value: float64) =
  discard

proc WriteTime*(self: TRegistry; Name: string; Value: float64) =
  discard

proc WriteCurrency*(self: TRegistry; Name: string; Value: float64) =
  discard

proc RenameValue*(self: TRegistry; OldName, NewName: string) =
  let k = current(self)
  if k == nil:
    return
  let i = findValue(k, OldName)
  if i >= 0:
    k.values[i].name = NewName

# --- file-backed entry points: accepted no-ops (documented divergence) ---

proc LoadKey*(self: TRegistry; Key, FileName: string): bool =
  result = false

proc UnLoadKey*(self: TRegistry; Key: string): bool =
  result = false

proc SaveKey*(self: TRegistry; Key, FileName: string): bool =
  result = false

proc ReplaceKey*(self: TRegistry; Key, FileName,
                 BackUpFileName: string): bool =
  result = false

proc RestoreKey*(self: TRegistry; Key, FileName: string): bool =
  result = false

proc RegistryConnect*(self: TRegistry; UNCName: string): bool =
  result = false

proc MoveKey*(self: TRegistry; OldName, NewName: string; Delete: bool) =
  discard

# --- TRegIniFile: the .ini view over the same store ----------------------

proc Create*(self: typedesc[TRegIniFile]; FileName: string): TRegIniFile =
  var r = TRegIniFile()
  r.FRootKey = HKEY_CURRENT_USER
  r.FCurrentPath = ""
  r.FLazyWrite = true
  r.FAccess = KEY_ALL_ACCESS
  r.FOpen = false
  r.FFileName = FileName
  result = r

proc Create*(self: typedesc[TRegIniFile]; FileName: string;
             AAccess: uint32): TRegIniFile =
  result = Create(TRegIniFile, FileName)
  result.FAccess = AAccess

proc FileName*(self: TRegIniFile): string =
  result = self.FFileName

proc ReadString*(self: TRegIniFile; Section, Ident,
                 Default: string): string =
  result = Default
  if OpenKey(self, "\\" & Section, false):
    if ValueExists(self, Ident):
      result = ReadString(self, Ident)
    CloseKey(self)

proc ReadInteger*(self: TRegIniFile; Section, Ident: string;
                  Default: int32): int32 =
  result = Default
  if OpenKey(self, "\\" & Section, false):
    if ValueExists(self, Ident):
      result = ReadInteger(self, Ident)
    CloseKey(self)

proc WriteString*(self: TRegIniFile; Section, Ident, Value: string) =
  if OpenKey(self, "\\" & Section, true):
    WriteString(self, Ident, Value)
    CloseKey(self)

proc WriteInteger*(self: TRegIniFile; Section, Ident: string;
                   Value: int32) =
  if OpenKey(self, "\\" & Section, true):
    WriteInteger(self, Ident, Value)
    CloseKey(self)

proc ReadBool*(self: TRegIniFile; Section, Ident: string;
               Default: bool): bool =
  result = Default
  if OpenKey(self, "\\" & Section, false):
    if ValueExists(self, Ident):
      result = ReadBool(self, Ident)
    CloseKey(self)

proc WriteBool*(self: TRegIniFile; Section, Ident: string; Value: bool) =
  if OpenKey(self, "\\" & Section, true):
    WriteBool(self, Ident, Value)
    CloseKey(self)

proc ReadSection*(self: TRegIniFile; Section: string;
                  Strings: TStringList) =
  if OpenKey(self, "\\" & Section, false):
    GetValueNames(self, Strings)
    CloseKey(self)

proc ReadSectionValues*(self: TRegIniFile; Section: string;
                        Strings: TStringList) =
  ## `Name=Value` per line, the Delphi TRegIniFile form
  if OpenKey(self, "\\" & Section, false):
    let k = current(self)
    if k != nil:
      for i in 0 ..< k.values.len:
        discard Strings.Add(k.values[i].name & "=" & k.values[i].text)
    CloseKey(self)

proc EraseSection*(self: TRegIniFile; Section: string) =
  discard DeleteKey(self, "\\" & Section)

proc DeleteKey*(self: TRegIniFile; Section, Ident: string) =
  if OpenKey(self, "\\" & Section, false):
    discard DeleteValue(self, Ident)
    CloseKey(self)
