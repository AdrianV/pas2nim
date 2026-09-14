#
#           pas2nimony - Pascal to Nimony translator
#
# Symbol tables used during translation:
#  - the case registry: lowercase name -> spelling used at the declaration
#    (Pascal is case-insensitive; nimony is case-sensitive, so every use is
#    rewritten to the declared spelling)
#  - the class registry: per class/object: member fields and member routines
#    (for the self-qualification pass) plus the parent class name
#  - the RTL name map: Pascal builtin/RTL names -> nimony or shim spellings

import std/[tables, strutils]
import pasast

type
  ClassInfo* = object
    ## everything known about a Pascal class/object type
    spelling*: string       ## declared spelling of the class name
    parent*: string         ## lowercase parent name ("" if none)
    isRef*: bool            ## true for `class`, false for `object`
    isInterface*: bool      ## Delphi `interface` type (M4)
    classVars*: seq[string] ## declared class var spellings
    classVarSet*: Table[string, bool]
    classProcs*: seq[string]  ## declared class method spellings
    classProcSet*: Table[string, bool]
    fields*: seq[string]    ## declared field spellings (in order)
    routines*: seq[string]  ## member routine spellings (methods, ctors, ...)
    fieldSet*: Table[string, bool]   ## lowercase field name -> true
    routineSet*: Table[string, bool] ## lowercase routine name -> true
    ctorSet*: Table[string, bool]    ## lowercase constructor names
    methodSet*: Table[string, bool]  ## lowercase virtual/override methods
    ## default array property info ("" when none):
    arrName*, arrGetter*, arrSetter*: string
    arrIdxType*, arrValType*: string
    typeParams*: seq[string]  ## generic type params ("" when non-generic)
    genericOf*: string        ## lowercase generic key for specialized
                              ## aliases/instances ("" for real types)

const
  # Delphi exception class -> nimony ErrorCode mapping (lossy!)
  ExcMap* = [
    ("erangeerror", "RangeError"), ("eoverflow", "OverflowError"),
    ("eintoverflow", "OverflowError"), ("eoutofmemory", "Failure"),
    ("einouterror", "IOError"), ("eabort", "Failure"),
    ("econverterror", "ValueError"), ("edivbyzero", "Failure"),
    ("einvalidcast", "Failure"), ("einvalidop", "Failure"),
    ("einvalidpointer", "IndexError"), ("einterror", "Failure"),
    ("ematherror", "Failure"), ("esafecallerror", "Failure"),
    ("egpfault", "Failure"), ("eheapexception", "Failure"),
    ("evarianterror", "ValueError"), ("eassertionfailed", "Failure"),
    ("eexternalexception", "Failure"), ("econtrolderror", "Failure"),
    ("esynerror", "SyntaxError"), ("ezerodivide", "Failure"),
    ("exception", "Failure")
  ]

proc excSpelling*(lowercaseName: string): string =
  ## ErrorCode spelling for a Delphi exception class ("" if unmapped)
  for i in 0..high(ExcMap):
    if ExcMap[i][0] == lowercaseName: return ExcMap[i][1]
  return ""

type
  SymTab* = object
    ## unit-global translation state
    names*: Table[string, string]      ## lowercase -> declared spelling
    exportedNames*: Table[string, bool] ## lowercase -> exported in interface
    classes*: Table[string, ClassInfo] ## lowercase class name -> info
    warned*: Table[string, bool]       ## names we already warned about
    rtlUnits*: seq[string]             ## Pascal unit names from `uses`
    returnsValue*: Table[string, bool] ## functions/constructors by lower name
    returnsBool*: Table[string, bool]  ## shim functions returning Boolean
    memberParams*: Table[string, string] ## cls.member -> ';'-joined param spellings
    convOps*: Table[string, string]
    ## conversion operator registry: key
    ## "conv:<cls>:<kind>:<from>:<to>" -> the lowered proc name
    ## (Delphi class operator Implicit/Explicit/Inc/Dec)
    routineArgs*: Table[string, int]
    ## parameter counts of known routines (lowercase name); a 0-arg
    ## entry enables Delphi's paren-less call in expression position
    ptrAliases*: seq[string]         ## shim `X* = ptr Y` pointer aliases
    defines*: Table[string, bool]
    ## conditional-compilation symbols: CLI -d: defines seed it and
    ## {$define}/{$undef} mutate it; {$ifdef}/{$ifndef} evaluate at
    ## parse time (Delphi model - the dead branch is skipped so its
    ## units never absorb)

proc initSymTab*(): SymTab =
  result = SymTab()
  result.exportedNames = initTable[string, bool]()

# ---------------------------------------------------------------------------
# case registry

proc declareName*(t: var SymTab; spelling: string) =
  ## record the declared spelling of a name; first declaration wins.
  let key = spelling.toLowerAscii
  if key.len > 0 and not t.names.hasKey(key):
    t.names[key] = spelling

const nimonyKeywords = ["addr", "bind", "block", "break", "cast", "concept",
  "continue", "converter", "defer", "discard", "distinct", "elif", "enum",
  "export", "from", "func", "import", "include", "isnot", "iterator", "let",
  "macro", "method", "mixin", "notin", "proc", "ptr", "ref", "return",
  "static", "template", "tuple", "using", "when", "yield"]
  ## nimony keywords that are NOT Pascal reserved words, so they can occur
  ## as Pascal identifiers (`addr` in synsock, `method`/`ptr`/`ref` in the
  ## RTL) and must be escaped. A Pascal reserved word is deliberately
  ## absent: the emitter renders OPERATORS through the same canon
  ## (`and`, `or`, `xor`, `shl`, `shr`, `not`, `div`, `mod`, `in`, `is`),
  ## so escaping them produced the undefined `pasand`/`pasxor`.

proc isNimonyKeyword(s: string): bool =
  let k = s.toLowerAscii
  for w in nimonyKeywords:
    if k == w: return true
  result = false

proc escapeNimonyName*(spelling: string): string =
  ## nimony rejects identifiers that lead or end with `_`, and it rejects
  ## a `__` RUN anywhere - lexing the second underscore of `pas__d` as a
  ## trailing underscore. A leading underscore is escaped with the `pas`
  ## prefix, a trailing one with the `_pas` suffix, but a run needs both
  ## handled at once: without this, the corpus field `__d` became `pas__d`
  ## and nifler refused the unit. Each underscore after the first of a run
  ## is spelled `U`, which no real spelling can collide with (`_d` ->
  ## `pas_d`, `__d` -> `pas_Ud`).
  result = ""
  var i = 0
  while i < spelling.len:
    if spelling[i] != '_':
      result.add(spelling[i])
      inc i
      continue
    var j = i
    while j < spelling.len and spelling[j] == '_': inc j
    let run = j - i
    let atStart = result.len == 0
    let atEnd = j >= spelling.len
    if atStart:
      result.add("pas")
    for k in 0 ..< run:
      if atEnd:
        # trailing run: `_pas` ends the name, further ones become `U`
        result.add(if k == 0: "_pas" else: "U")
      else:
        result.add(if k == 0: "_" else: "U")
    i = j
  if isNimonyKeyword(result):
    # `addr`, `method`, `block`, `ref`, ... are ordinary Pascal
    # identifiers (synsock declares `proc (addr: pointer; ...)`) but
    # keywords to nimony, which refuses them in any name position.
    result = "pas" & result

proc canonical*(t: SymTab; spelling: string): string =
  ## the spelling to use for `spelling`: the RTL map wins, then the
  ## declaration registry, then the input spelling itself.
  let key = spelling.toLowerAscii
  let rtl = rtlSpelling(key)
  if rtl.len > 0: return rtl
  result = escapeNimonyName(t.names.getOrDefault(key, spelling))

proc canonicalMember*(t: SymTab; spelling: string): string =
  ## canon for MEMBER names (fields/properties/methods): the type-ish
  ## RTL map must not win here (`List.Text` is the stringlist
  ## property, not the Text file type) - only the declaration
  ## registry applies
  let key = spelling.toLowerAscii
  result = escapeNimonyName(t.names.getOrDefault(key, spelling))

proc isDeclared*(t: SymTab; spelling: string): bool =
  t.names.hasKey(spelling.toLowerAscii)

# ---------------------------------------------------------------------------
# class registry

proc isGenericClass*(t: SymTab; name: string): bool =
  ## true when `name` declares type parameters (a generic type)
  let ci = t.classes.getOrDefault(name.toLowerAscii)
  result = ci.spelling.len > 0 and ci.typeParams.len > 0

proc typeParamsOf*(t: SymTab; name: string): seq[string] =
  let ci = t.classes.getOrDefault(name.toLowerAscii)
  result = ci.typeParams

proc registerSpecializedAlias*(t: var SymTab; genericName: string;
    instance: string) =
  ## register `instance` (e.g. `TFoo<int32,string>` or a `specialize`
  ## alias) as a class type sharing the generic's member sets so the
  ## ctor/method machinery behaves like for any other class
  let ci = t.classes.getOrDefault(genericName.toLowerAscii)
  if ci.spelling.len == 0:
    return
  let ikey = instance.toLowerAscii
  if t.classes.hasKey(ikey):
    return
  var info = ClassInfo(spelling: instance, isRef: ci.isRef,
                       typeParams: ci.typeParams,
                       genericOf: genericName.toLowerAscii)
  for f, v in ci.fieldSet: info.fieldSet[f] = v
  for r, v in ci.routineSet: info.routineSet[r] = v
  for c, v in ci.ctorSet: info.ctorSet[c] = v
  for m, v in ci.methodSet: info.methodSet[m] = v
  for s, v in ci.classVarSet: info.classVarSet[s] = v
  for s, v in ci.classProcSet: info.classProcSet[s] = v
  t.classes[ikey] = info

proc registerClass*(t: var SymTab; spelling: string; parent: string; isRef: bool) =
  ## register a class/object type; also declares its name
  declareName(t, spelling)
  var ci = ClassInfo(spelling: spelling, parent: parent.toLowerAscii,
                     isRef: isRef)
  t.classes[spelling.toLowerAscii] = ci

proc registerClassShape*(t: var SymTab; spelling: string; parent: string;
                         isRef: bool) =
  ## register a class from a shim source scan: the shim's
  ## `X* = ref object of Y` line proves the SHAPE only. A class the
  ## prelude (or a Pascal unit) already describes keeps its members -
  ## re-registering would drop the exception ctor registration and make
  ## every shim exception subclass look ctorless (its synthesized
  ## `create` then hijacks `raise SomeE.Create(msg)`). The scanned
  ## parent is adopted only when none is known.
  let key = spelling.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  ci.spelling = spelling
  if ci.parent.len == 0:
    ci.parent = parent.toLowerAscii
  ci.isRef = isRef
  t.classes[key] = ci
  declareName(t, spelling)

proc lookupClass*(t: SymTab; name: string): ClassInfo =
  t.classes.getOrDefault(name.toLowerAscii)

proc isClass*(t: SymTab; name: string): bool =
  t.classes.hasKey(name.toLowerAscii)

proc classSpelling*(t: SymTab; name: string): string =
  ## declared spelling of a class type; "" when unknown
  let ci = t.classes.getOrDefault(name.toLowerAscii)
  result = ci.spelling

proc addField*(t: var SymTab; cls, spelling: string) =
  ## record a field declaration of class `cls`
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len == 0 and cls.len > 0:
    # A `record` body is parsed by parseTypeDesc, which publishes the
    # self-class name but does not go through registerClass for every
    # path. Create the entry on demand: silently dropping the field
    # would disable self-qualification for the whole type rather than
    # fail loudly.
    ci = ClassInfo(spelling: cls, parent: "", isRef: false)
  if ci.spelling.len > 0:
    if not ci.fieldSet.hasKey(spelling.toLowerAscii):
      ci.fields.add(spelling)
      ci.fieldSet[spelling.toLowerAscii] = true
      t.classes[key] = ci
    t.declareName(spelling)

proc normTyKey(spelling: string): string =
  ## type keys are normalized through the RTL map so `Integer` and
  ## `int32` agree at declaration and call sites
  let lo = spelling.toLowerAscii
  let rtl = rtlSpelling(lo)
  if rtl.len > 0: result = rtl else: result = lo

proc addConvOp*(t: var SymTab; cls, kind, fromTy, toTy, procName: string) =
  ## record a Delphi class conversion operator; kind is implicit/
  ## explicit/inc/dec
  var key = "conv:" & cls.toLowerAscii & ":" & kind
  if fromTy.len > 0:
    key = key & ":" & normTyKey(fromTy) & ":" & normTyKey(toTy)
  t.convOps[key] = procName
  t.declareName(procName)

proc getConvOp*(t: SymTab; cls, kind, fromTy, toTy: string): string =
  var key = "conv:" & cls.toLowerAscii & ":" & kind
  if fromTy.len > 0:
    key = key & ":" & normTyKey(fromTy) & ":" & normTyKey(toTy)
  result = t.convOps.getOrDefault(key)

proc addRoutine*(t: var SymTab; cls, spelling: string) =
  ## record a member routine of class `cls`
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    if not ci.routineSet.hasKey(spelling.toLowerAscii):
      ci.routines.add(spelling)
      ci.routineSet[spelling.toLowerAscii] = true
      t.classes[key] = ci
    t.declareName(spelling)

proc addMethod*(t: var SymTab; cls, spelling: string) =
  ## record that `spelling` is a virtual method of class `cls`
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    ci.methodSet[spelling.toLowerAscii] = true
    t.classes[key] = ci

proc isMethodDeclared*(t: SymTab; cls, name: string): bool =
  ## true only when THIS class declared `name` virtual (no ancestor
  ## walk - the implementation parser uses it to keep the class body's
  ## virtual/hiding decision)
  let ci = t.classes.getOrDefault(cls.toLowerAscii)
  result = ci.spelling.len > 0 and ci.methodSet.hasKey(name.toLowerAscii)

proc ancestorMethodOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a virtual method of a strict ancestor of `cls`
  let key = cls.toLowerAscii
  let ci = t.classes.getOrDefault(key)
  if ci.spelling.len == 0: return false
  result = t.isMethodOf(ci.parent, name)

proc isMethodOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` was declared virtual in `cls` or an ancestor
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.methodSet.hasKey(name.toLowerAscii): return true
    k = ci.parent
    inc guard
  return false

proc addMemberParams*(t: var SymTab; cls, member: string;
                       tys: string) =
  ## record a member's parameter type spellings (';'-joined, self
  ## excluded) so inherited-call args can coerce pointer/object forms
  t.memberParams[cls.toLowerAscii & "." & member.toLowerAscii] = tys

proc memberParamsOf*(t: SymTab; cls, member: string): string =
  t.memberParams.getOrDefault(cls.toLowerAscii & "." &
                              member.toLowerAscii)

proc addCtor*(t: var SymTab; cls, spelling: string) =
  ## record a constructor of class `cls` (also a routine)
  t.addRoutine(cls, spelling)
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    ci.ctorSet[spelling.toLowerAscii] = true
    t.classes[key] = ci

proc addClassVar*(t: var SymTab; cls, spelling: string) =
  ## record a class var declaration (module-level storage)
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    if not ci.classVarSet.hasKey(spelling.toLowerAscii):
      ci.classVars.add(spelling)
      ci.classVarSet[spelling.toLowerAscii] = true
      t.classes[key] = ci

proc isClassVarOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a class var of `cls` or an ancestor
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.classVarSet.hasKey(name.toLowerAscii): return true
    k = ci.parent
    inc guard
  return false

proc addClassProc*(t: var SymTab; cls, spelling: string) =
  ## record a class method declaration (static, no self)
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    if not ci.classProcSet.hasKey(spelling.toLowerAscii):
      ci.classProcs.add(spelling)
      ci.classProcSet[spelling.toLowerAscii] = true
      t.classes[key] = ci

proc isClassProcOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a class method of `cls` or an ancestor
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.classProcSet.hasKey(name.toLowerAscii): return true
    k = ci.parent
    inc guard
  return false

proc classProcName*(t: SymTab; cls, spelling: string): string =
  ## the module-level name a class method lowers to
  let ci = t.classes.getOrDefault(cls.toLowerAscii)
  let cs = if ci.spelling.len > 0: ci.spelling else: cls
  result = "pasCm_" & cs & "_" & spelling

proc classVarName*(t: SymTab; cls, spelling: string): string =
  ## the module-level name a class var lowers to
  let ci = t.classes.getOrDefault(cls.toLowerAscii)
  let cs = if ci.spelling.len > 0: ci.spelling else: cls
  result = "pasCv_" & cs & "_" & spelling

proc isRoutineOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a declared routine of `cls` or an ancestor
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.routineSet.hasKey(name.toLowerAscii): return true
    k = ci.parent
    inc guard
  return false

proc findCtorClass*(t: SymTab; cls, name: string): string =
  ## return the spelling of the class that declares constructor
  ## `name` in `cls`'s hierarchy ("" when none)
  let key = cls.toLowerAscii
  var guard = 0
  var k = key
  while k.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(k)
    if ci.spelling.len == 0: break
    if ci.ctorSet.hasKey(name.toLowerAscii): return ci.spelling
    k = ci.parent
    inc guard
  return ""

proc isCtorOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a constructor of `cls` or one of its ancestors
  var key = cls.toLowerAscii
  var guard = 0
  while key.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(key)
    if ci.spelling.len == 0: break
    if ci.ctorSet.hasKey(name.toLowerAscii): return true
    if ci.genericOf.len > 0 and ci.genericOf != key:
      key = ci.genericOf
    else:
      key = ci.parent
    inc guard
  return false

proc isMemberOf*(t: SymTab; cls, name: string): bool =
  ## true if `name` is a field or routine of `cls` or one of its ancestors
  var key = cls.toLowerAscii
  var guard = 0
  while key.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(key)
    if ci.spelling.len == 0: break
    if ci.fieldSet.hasKey(name.toLowerAscii): return true
    if ci.routineSet.hasKey(name.toLowerAscii): return true
    if ci.genericOf.len > 0 and ci.genericOf != key:
      key = ci.genericOf
    else:
      key = ci.parent
    inc guard
  return false

proc isFieldOf*(t: SymTab; cls, name: string): bool =
  ## like isMemberOf, but only fields
  var key = cls.toLowerAscii
  var guard = 0
  while key.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(key)
    if ci.spelling.len == 0: break
    if ci.fieldSet.hasKey(name.toLowerAscii): return true
    if ci.genericOf.len > 0 and ci.genericOf != key:
      key = ci.genericOf
    else:
      key = ci.parent
    inc guard
  return false

proc ancestorSpelling*(t: SymTab; cls: string): string =
  ## the declared spelling of `cls`'s parent class ("" if none)
  let ci = t.classes.getOrDefault(cls.toLowerAscii)
  if ci.spelling.len > 0 and ci.parent.len > 0:
    let pci = t.classes.getOrDefault(ci.parent)
    if pci.spelling.len > 0: return pci.spelling
  return ""

proc setArrayProp*(t: var SymTab; cls: string; name, getter, setter,
                   idxType, valType: string) =
  ## record the (default) array property of class `cls`
  let key = cls.toLowerAscii
  var ci = t.classes.getOrDefault(key)
  if ci.spelling.len > 0:
    ci.arrName = name
    ci.arrGetter = getter
    ci.arrSetter = setter
    ci.arrIdxType = idxType
    ci.arrValType = valType
    t.classes[key] = ci

proc getArrayProp*(t: SymTab; cls: string): ClassInfo =
  ## returns the ClassInfo with array property info, walking ancestors
  var key = cls.toLowerAscii
  var guard = 0
  while key.len > 0 and guard < 100:
    let ci = t.classes.getOrDefault(key)
    if ci.spelling.len == 0: break
    if ci.arrName.len > 0: return ci
    key = ci.parent
    inc guard
  return ClassInfo()

# ---------------------------------------------------------------------------
# RTL name map
#
# lowercase Pascal name -> nimony (or shim) spelling. Applied uniformly to
# declarations and uses. This table must only contain PURE RENAMES.
# Constructs that need argument reordering (Pos, Copy, StrToIntDef, ...)
# or extra arguments (write -> write(stdout, ...)) are handled by the
# parser/shim instead.

const RtlNames* = [
  # types (Delphi sizes)
  ("integer", "int32"), ("longint", "int32"), ("longword", "uint32"),
  ("cardinal", "uint32"), ("smallint", "int16"), ("shortint", "int8"),
  ("byte", "uint8"), ("word", "uint16"), ("single", "float32"),
  ("double", "float64"), ("real", "float64"), ("extended", "float64"),
  ("comp", "int64"), ("currency", "float64"), ("boolean", "bool"),
  ("char", "char"), ("ansichar", "char"), ("pchar", "cstring"),
  ("pointer", "pointer"), ("tobject", "RootRef"),
  ("hresult", "int32"), ("dword", "uint32"), ("qword", "uint64"),
  ("nativeint", "int"), ("nativeuint", "uint"),
  ("textfile", "File"), ("text", "File"),
  ("string", "string"), ("ansistring", "string"), ("widestring", "string"),
  ("unicodestring", "string"), ("shortstring", "string"), ("tstring", "string"),
  ("singlefloat", "float32"), ("tclass", "RootRef"),
  ("tdatetime", "TDateTime"),
  ("variant", "Variant"), ("olevariant", "Variant"),
  # nimony has no `UInt64` spelling of its own: the Pascal name must map
  ("uint64", "uint64"), ("uint32", "uint32"), ("uint16", "uint16"),
  ("uint8", "uint8"),
  # builtin routines (pure renames)
  ("pasfind", "pasFind"), ("ord", "ord"), ("chr", "chr"), ("low", "low"),
  ("high", "high"), ("setlength", "setLen"), ("inc", "inc"), ("dec", "dec"),
  ("succ", "succ"), ("pred", "pred"), ("abs", "abs"), ("odd", "Odd"),
  ("round", "pasRound"), ("trunc", "trunc"), ("sizeof", "sizeof"),
  ("sqr", "sqr"), ("sqrt", "sqrt"), ("sin", "sin"), ("cos", "cos"),
  ("min", "min"), ("max", "max"),
  ("arctan", "arctan"), ("arcsin", "arcsin"), ("arccos", "arccos"),
  ("ln", "ln"), ("exp", "exp"), ("pi", "PI"),
  ("include", "incl"), ("exclude", "excl"),
  # memory primitives get Pascal-RTL-only spellings: their generic
  # `var`-parameter overloads would otherwise hijack user methods of
  # the same name (e.g. a TPoint.Move method is misresolved as the
  # memory shim with the misleading 'cannot pass 2 to var/out T')
  ("move", "pasMove"), ("fillchar", "pasFillChar"),
  ("halt", "quit"),
  # reserved spellings (True/TRUE -> true etc.)
  ("true", "true"), ("false", "false"), ("nil", "nil"),
  ("result", "result"), ("self", "self"),
  # shim routines (provided by the emitted preamble / runtime shim)
  ("assigned", "assigned"),
  ("inttostr", "IntToStr"), ("strtoint", "StrToInt"),
  ("strtointdef", "StrToIntDef"), ("floattostr", "FloatToStr"),
  ("strtofloat", "StrToFloat"), ("uppercase", "UpperCase"),
  ("lowercase", "LowerCase"), ("upcase", "UpCase"),
  ("trim", "Trim"), ("trimleft", "TrimLeft"), ("trimright", "TrimRight"),
  ("sameText", "SameText"), ("comparetext", "CompareText"),
  ("stringofchar", "StringOfChar"), ("ansipos", "Pos"),
  ("strtofloatdef", "StrToFloatDef"), ("even", "Even"),
  # POSIX semaphore names from SyncObjs's LINUX branch; the Libc shim
  # registers the Pascal-cased spellings (nimony forbids trailing
  # underscore idents, so the raw spellings cannot survive)
  ("sem_init", "semInit"), ("sem_wait", "semWait"),
  ("sem_post", "semPost"), ("sem_getvalue", "semGetValue"),
  ("sem_trywait", "semTryWait"),
  # the 64-bit integer alias keeps its nimony spelling
  ("int64", "int64"),
]

proc rtlSpelling*(lowercaseName: string): string =
  ## the nimony spelling for a Pascal RTL name ("" if unmapped)
  for i in 0..high(RtlNames):
    if RtlNames[i][0] == lowercaseName: return RtlNames[i][1]
  return ""