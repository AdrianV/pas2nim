interface

type
  MyClass = class;
  MyArray = array of string;
  TNotifyEvent = procedure(Sender: TObject) of object;
  MyRecord = record
    FData: Integer;
  end;
  MyObject = object
    FData: Integer;
    procedure init;
  end;
  MySeconObject = object(MyObject)
    fd2: string;
  end;
  MyClass = class
  public
    Hallo: Integer;
    property data: string read FData write FData;
    property data2: string read FData write setData;
    property myHello: Integer read Hello write Hello;
    property myArray[index: Integer]: string read getMyArray write setMyArray; default;
    constructor create(v: Integer);
    procedure doIt; virtual;
  private
    FData: string;
    procedure doSomething(Sender: TObject);  
    procedure setData(value: string);
    function getMyArray(const index: Integer): String;
    procedure setMyArray(const index: Integer; value: String);
  published
    function calc(a,b: Double): Double;
  end;
  Second = class(MyClass)
  private
    fd2: Boolean;
    constructor create(const a: Boolean; v: Integer);
  public
    procedure doIt; virtual; override;
  end;
  Third = class(Second)
  public
    procedure doIt; virtual; override;
  end;
  

var 
  aisPublic: Integer;
implementation
uses strutils;

var 
  aisPrivate: Integer;

procedure write(x: string); 
begin
  echo(x);
end;

procedure MyObject.init;
begin
  FData:= 23;
end;

procedure MyClass.doSomething(Sender: TObject); 
var
    j: Integer;
begin
  write('blah');
end;

constructor MyClass.create(v: Integer);
begin
  Hallo:= v;
  FData:= 'ich bin Text';
end;

function MyClass.calc(a,b: Double): Double;
begin
  result:= a + b * Double(Hallo);
end;

function MyClass.getMyArray(const index: Integer): String;
begin
  Result:= intToStr(index);
end;

procedure MyClass.setMyArray(const index: Integer; value: String);
begin
  write('set myArray[]=' + value);
end;

procedure MyClass.setData(value: string);
begin
  FData:= value;
  write(value);
end;

procedure MyClass.doIt; 
begin
  write('MyClass doIt');
end;

constructor Second.create(const a: Boolean; v: Integer);
begin
  fd2:= a;
  inherited create(v);
end;

procedure Second.doIt; 
  function innerDoIt: String;
  begin
    Result:= 'this my second doIt'
  end;
begin
  write(innerDoIt());
end;

{$if false}

procedure test(const data: array of const);
begin
  
end;

{$endif}

procedure Third.doIt; 
begin
  write('hello from Third');
  inherited;
end;

var 
  sec: Second;
  my: MyClass;
  thd: Third;
begin
  sec:= Second.create(True, 145);
  sec.data2:= 'Hallo Nim';
  write(sec.data);
  sec.doIt;
  my:= MyClass.create(321);
  my.doIt;
  my:= sec;
  my.doIt;
  thd:= Third.create(False, 17);
  thd.doIt;
  write(my.myArray[11]);
  my.myArray[11]:= 'Hallo Nim';
  my[1]:= 'works as well';
end.