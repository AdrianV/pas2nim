program IntfTest;
{ M4 interfaces: `IFoo = interface` lowers to an abstract ref class
  whose methods are nimony `method`s with discard (procedure) or
  `result = default(T)` (function) bodies; nimony's dynamic dispatch
  plays the vtable. A class implements one interface; TInterfacedObject
  counts as empty plumbing and dissolves into the interface's generated
  class, so `class(TInterfacedObject, IFoo)` inherits it and Delphi's
  implicitly-virtual interface implementations become overrides.
  v1 gaps: no refcounting (object lifetime is independent), no
  QueryInterface (is/as ride the object's class), properties rejected. }

type
  ICounter = interface
    function Next: Integer;
    procedure Reset;
  end;
  IDog = interface(ICounter)
    procedure Fetch;
  end;
  TCounter = class(TInterfacedObject, ICounter)
  private
    FVal: Integer;
  public
    function Next: Integer;
    procedure Reset;
  end;
  TLab = class(TInterfacedObject, IDog)
  private
    FVal: Integer;
  public
    function Next: Integer;
    procedure Reset;
    procedure Fetch;
  end;

function TCounter.Next: Integer;
begin
  Inc(FVal);
  Result := FVal;
end;

procedure TCounter.Reset;
begin
  FVal := 0;
end;

function TLab.Next: Integer;
begin
  FVal := FVal + 10;
  Result := FVal;
end;

procedure TLab.Reset;
begin
  FVal := 0;
end;

procedure TLab.Fetch;
begin
  writeln('fetching');
end;

var
  C: ICounter;
  D: IDog;
  N: Integer;
begin
  C := TCounter.Create;
  writeln('next=', C.Next);
  writeln('next=', C.Next);
  C.Reset;
  writeln('after reset next=', C.Next);
  if C is ICounter then writeln('is-ok');
  N := (C as ICounter).Next;
  writeln('as-next=', N);
  D := TLab.Create;
  D.Fetch;
  C := D;
  writeln('dog next=', C.Next);
  D := nil;
  if D = nil then writeln('cleared');
  writeln('done');
end.