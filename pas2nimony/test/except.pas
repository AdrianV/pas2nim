program Except1;
{ try/finally + except mapping (ErrorCode, lossy) }

procedure Boom;
begin
  raise Exception.Create('boom');
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
      writeln('caught: ', X);
  end;
  try
    X := 5;
  finally
    writeln('finally ran, x=', X);
  end;
  writeln('after');
end.
