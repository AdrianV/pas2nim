(* included at UNIT level. It pulls in a second file by the quoted
  spelling, which the lexer has to capture rather than discard. *)
{$I 'inctestdeep.inc'}

const
  CFromInclude = NNested * 2;

procedure IncProc;
begin
  writeln('proc n=', NNested);
end;
