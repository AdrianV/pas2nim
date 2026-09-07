# Copyright 2016.

type
  TVarType* = enum
    vtInt, vtBool, vtChar, vtFloat, vtString,
    vtInt64, vtRootRef
  TVarRec* = object
    case vType*: TVarType
    of vtInt : vInt* : int
    of vtBool : vBool* : bool
    of vtChar : vChar* : char
    of vtFloat : vFloat* : float
    of vtString : vString* : string
    of vtInt64 : vInt64* : int64
    of vtRootRef : vRootRef* : RootRef 
  TArrayOfConst = seq[TVarRec]
  AC* = object
    discard

const ac = AC()

template defineType(T: typedesc) =
  converter `from T`* (value: T.type): TVarRec =  
    result.vType = `vt T`
    result.`v T` = value
  
  proc toVrec * (value: T.type): TVarRec =  
    result.vType = `vt T`
    result.`v T` = value

defineType(int)    
defineType(bool)
defineType(char)
defineType(float)
defineType(string)
defineType(int64)
defineType(RootRef)

template `[]`* (ac: AC, data: varargs[TVarRec, toVrec]): TArrayOfConst =
  var result: TArrayOfConst
  newSeq(result, data.len)
  for i, c in data: result[i] = c
  result

when isMainModule:

  proc Format(f: string, data: varargs[TVarRec, toVrec]) =
    echo f
    for c in data:
      echo c.repr
  
  proc Format(f: string, data: TArrayOfConst) =
    echo f
    for c in data:
      echo c.repr

  Format("blah", 1, true, 'x', 3.14, "Hallo", 0x1234_1234_1234)      
  Format("blah", ac[1, true, 'x', 3.14, "Hallo", 0x1234_1234_1234])      
  Format("blah", @[1.toVrec, true, 'x', 3.14, "Hallo", 0x1234_1234_1234])      
