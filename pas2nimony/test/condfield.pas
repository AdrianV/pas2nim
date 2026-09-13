program CondField;
{ A conditional may select the NAME of a field rather than whether a
  declaration exists at all - the shape real socket headers use, where
  the field after a dot is chosen per target. The name is decided at
  parse time from the define set, which a frontend does know: it comes
  from the command line and from a DEFINE directive. }
{$define WIDE_FIELDS}

type
  TAddr = record
    {$IFDEF WIDE_FIELDS}alpha{$ELSE}beta{$ENDIF}: Integer;
  end;

var
  a: TAddr;

begin
  a.alpha := 41;
  writeln('alpha=', a.alpha);
end.
