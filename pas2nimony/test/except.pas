program Except1;
{ exception instances (M4-2): `raise SomeE.Create(msg)` stashes the
  instance, `on E: SomeEx do` binds it (E.Message), bare `raise;`
  re-raises, and an uncaught exception is reported by the generated
  main-block handler }

type
  EMyError = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

constructor EMyError.Create(const Msg: String);
begin
  inherited Create(Msg);
end;

procedure Boom;
begin
  raise Exception.Create('boom');
end;

procedure BoomCustom;
begin
  raise EMyError.Create('custom boom');
end;

procedure ReRaiser;
begin
  try
    BoomCustom;
  except
    on E: EMyError do
    begin
      writeln('inner: ', E.Message);
      raise;
    end;
  end;
end;

var
  X: Integer;
begin
  X := 0;
  try
    X := 10;
    Boom;
    X := 99;              { never reached }
  except
    on E: Exception do
      writeln('caught: ', X, ' msg=', E.Message);
  end;
  try
    X := 5;
  finally
    writeln('finally ran, x=', X);
  end;
  try
    ReRaiser;
  except
    on E: Exception do
      writeln('outer after reraise: ', E.Message, ' x=', X);
  end;
  writeln('after');
  { uncaught: the main-block handler reports the instance message }
  BoomCustom;
end.