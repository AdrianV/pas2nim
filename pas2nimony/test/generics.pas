program Generics;
{ M4-10: generics. Delphi style `TPair<K, V> = class` with inline
  instantiations at use sites (`TPair<Integer, String>` in type
  positions and `TPair<Integer, String>.Create(...)` constructor
  calls) and FPC objfpc style (`specialize TPair<Integer, String>`
  aliases). Generic members lower to generic nimony procs
  (`proc GetKey[K, V](self: TPair[K, V]): K`); call sites stay
  unchanged and nimony resolves the instantiation. A generic class's
  bodiless declarations must not become module-level forwards (the
  type params would be undeclared there). Constraints (`T: class`)
  are parsed and dropped in v1. }

type
  TPair<K, V> = class
  private
    FKey: K;
    FValue: V;
  public
    constructor Create(AKey: K; AValue: V);
    function GetKey: K;
    function GetValue: V;
  end;

  TIntPair = specialize TPair<Integer, String>;

  TWidget = class
  public
    Name: String;
  end;

  TBox<T: class> = class
  private
    FData: T;
  public
    constructor Create(AData: T);
    function GetData: T;
  end;

constructor TPair<K, V>.Create(AKey: K; AValue: V);
begin
  FKey := AKey;
  FValue := AValue;
end;

function TPair<K, V>.GetKey: K;
begin
  Result := FKey;
end;

function TPair<K, V>.GetValue: V;
begin
  Result := FValue;
end;

constructor TBox<T>.Create(AData: T);
begin
  FData := AData;
end;

function TBox<T>.GetData: T;
begin
  Result := FData;
end;

var
  P: TPair<Integer, String>;
  Q: TIntPair;
  W: TWidget;
  B: TBox<TWidget>;
begin
  P := TPair<Integer, String>.Create(42, 'hello');
  writeln('key=', P.GetKey, ' val=', P.GetValue);
  Q := TIntPair.Create(7, 'fpc');
  writeln('qkey=', Q.GetKey);
  W := TWidget.Create;
  W.Name := 'widget';
  B := TBox<TWidget>.Create(W);
  writeln('boxed=', B.GetData.Name);
end.