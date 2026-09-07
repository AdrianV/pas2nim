program GotoTest;
{ M4 goto/label: forward gotos lower to named-block breaks (including
  out of loops and out of try-finally - the finally runs, like
  Delphi), backward gotos to while/continue loops, numeric labels,
  and routine-local label scopes }

label
  Retry, Cleanup, 10;

var
  I, Tries: Integer;
  S: String;

procedure Spin;
label
  Again;
var
  N: Integer;
begin
  N := 0;
Again:
  Inc(N);
  if N < 2 then goto Again;
  writeln('spin=', N);
end;

begin
  Tries := 0;
Retry:
  Inc(Tries);
  if Tries < 3 then goto Retry;
  writeln('tries=', Tries);
  S := 'untouched';
  for I := 1 to 10 do
  begin
    if I = 2 then goto Cleanup;
    if I = 1 then S := 'skipped';
  end;
  S := 'no-cleanup';         { never reached }
Cleanup:
  writeln('cleanup s=', S);
  try
    goto 10;
  finally
    writeln('finally ran');
  end;
  writeln('not reached');    { never reached }
10:
  writeln('done');
  Spin;
end.