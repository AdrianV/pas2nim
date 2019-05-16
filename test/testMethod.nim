
type
  ## MyClass* = class;
  MyArray* = seq[string]
  TNotifyEvent* = proc (Sender: RootRef) {.closure.}
  MyRecord* {.final.} = object
    FData*: int

  MyObject* = object {.inheritable.}
    FData*: int

  MySeconObject* = object of MyObject
    fd2*: string

  MyClass* = ref object of RootRef
    Hallo*: int
    FData: string

  Second* = ref object of MyClass
    fd2: bool

  Third* = ref object of Second
  

proc init(self: var MyObject)
template data*(self: MyClass): string =
  self.FData

template `data =`(self: MyClass; v: string) =
  self.FData = v

template data2*(self: MyClass): string =
  self.FData

template `data2 =`(self: MyClass; v: string) =
  setData(self, v)

template myHello*(self: MyClass): int =
  self.Hello

template `myHello =`(self: MyClass; v: int) =
  when compiles(Hello(self, v)):
    Hello(self, v)
  else:
    self.Hello = v

type
  `MyClass*myArray` = distinct MyClass

template myArray*(self: MyClass): `MyClass*myArray` =
  self.`MyClass*myArray`

template `[]`*(self: `MyClass*myArray`; index: int): string =
  getMyArray(self.MyClass, index)

template `[]`*(self: MyClass; index: int): string =
  getMyArray(self.MyClass, index)

template `[]=`*(self: `MyClass*myArray`; index: int; v: string) =
  setMyArray(self.MyClass, index, v)

template `[]=`*(self: MyClass; index: int; v: string) =
  setMyArray(self.MyClass, index, v)

proc create(self: MyClass; v: int): MyClass {.discardable.}
template create(T: typedesc[MyClass]; v: int): untyped =
  cast[T.type](create(new(T), v))

method doIt(self: MyClass)
proc calc(self: MyClass; a, b: float64): float64
method doIt(self: Second)
method doIt(self: Third)
var aisPublic*: int

## # implementation

{.this: self.}
template inherited(self: MySeconObject): MyObject =
  MyObject(self)

proc doSomething(self: MyClass; Sender: RootRef)
proc setData(self: MyClass; value: string)
proc getMyArray(self: MyClass; index: int): string
proc setMyArray(self: MyClass; index: int; value: string)
template inherited(self: Second): MyClass =
  MyClass(self)

proc create(self: Second; a: bool; v: int): Second {.discardable.}
template create(T: typedesc[Second]; a: bool; v: int): untyped =
  cast[T.type](create(new(T), a, v))

template inherited(self: Third): Second =
  Second(self)

import
  strutils

var aisPrivate: int

proc write(x: string) =
  echo(x)

proc init(self: var MyObject) =
  FData = 23

proc doSomething(self: MyClass; Sender: RootRef) =
  var j: int
  write("blah")

proc create(self: MyClass; v: int): MyClass =
  result = self
  Hallo = v
  FData = "ich bin Text"

proc calc(self: MyClass; a, b: float64): float64 =
  result = a + b * float64(Hallo)

proc getMyArray(self: MyClass; index: int): string =
  result = intToStr(index)

proc setMyArray(self: MyClass; index: int; value: string) =
  write("set myArray[]=" & value)

proc setData(self: MyClass; value: string) =
  FData = value
  write(value)

method doIt(self: MyClass) =
  write("MyClass doIt")

proc create(self: Second; a: bool; v: int): Second =
  result = self
  fd2 = a
  create(inherited(self), v)

method doIt(self: Second) =
  proc innerDoIt(): string =
    result = "this my second doIt"

  write(innerDoIt())

when false:
  proc test(data: TArrayOfConst) =
    nil

method doIt(self: Third) =
  write("hello from Third")
  procCall inherited(self).doIt()

var
  sec: Second
  my: MyClass
  thd: Third

sec = Second.create(true, 145)
sec.data2 = "Hallo Nim"
write(sec.data)
sec.doIt
my = MyClass.create(321)
my.doIt
my = sec
my.doIt
thd = Third.create(false, 17)
thd.doIt
write(my.myArray[11])
my.myArray[11] = "Hallo Nim"
my[1] = "works as well"