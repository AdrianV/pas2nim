interface

type
  MyArray = array of string;
  TNotifyEvent = procedure(Sender: TObject) of object;
  MyRecord = record
    FData: Integer;
  end;
  MyObject = object
    FData: Integer;
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
    constructor create(v: Integer);
    procedure doIt; virtual;
  private
    FData: string;
    procedure doSomething(Sender: TObject);  
    procedure setData(value: string);
  published
    function calc(a,b: Double): Double;
  end;
  Second = class(MyClass)
  private
    fd2: Boolean;
    constructor create(a: Boolean; v: Integer);
  public
    procedure doIt; virtual; override;
  end;

var 
  aisPublic: Integer;
implementation
var 
  aisPrivate: Integer;

procedure write(x: string); 
begin
  echo(x);
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

procedure MyClass.setData(value: string);
begin
  FData:= value;
  write(value);
end;

procedure MyClass.doIt; 
begin
  write('MyClass doIt');
end;

constructor Second.create(a: Boolean; v: Integer);
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

//procedure test(data: array of const);
//begin
//  
//end;

{$endif}

var 
  sec: Second;
  my: MyClass;
begin
  sec:= Second.create(True, 145);
  sec.data2:= 'Hallo Nim';
  write(sec.data);
  sec.doIt;
  my:= MyClass.create(321);
  my.doIt;
end.