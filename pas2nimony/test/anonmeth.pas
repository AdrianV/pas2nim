program AnonTest;
{ M4-11: anonymous methods. `reference to procedure/function` type
  defs lower to plain nimony proc types (the closure state lives in
  nimony's anonymous-proc machinery, which captures outer variables
  by reference). Delphi anonymous method literals
  `procedure(X: Integer) begin ... end` lower to nameless nimony
  lambdas: block form when assigned, inline form in call arguments.
  v1 notes: a property name and a module-level var of the same name
  collide in nimony's namespace (avoid in samples). }

type
  TProc = reference to procedure(X: Integer);
  TAdder = reference to function(A, B: Integer): Integer;
  TCalc = class
  private
    FOnDone: TProc;
    FTotal: Integer;
  public
    procedure SetOnDone(F: TProc);
    procedure Run;
  end;

procedure TCalc.SetOnDone(F: TProc);
begin
  FOnDone := F;
end;

procedure TCalc.Run;
begin
  if FOnDone <> nil then FOnDone(7);
end;

var
  Sum: Integer;
  F: TProc;
  G: TAdder;
  C: TCalc;
begin
  F := procedure(X: Integer)
  begin
    Sum := Sum + X;
    writeln('anon x=', X);
  end;
  F(2);
  G := function(A, B: Integer): Integer
  begin
    Result := A * B;
  end;
  writeln('mul=', G(6, 7));
  C := TCalc.Create;
  C.SetOnDone(procedure(X: Integer)
  begin
    C.FTotal := C.FTotal + X;
    Sum := Sum + X;
  end);
  C.Run;
  writeln('sum=', Sum, ' calc=', C.FTotal);
end.