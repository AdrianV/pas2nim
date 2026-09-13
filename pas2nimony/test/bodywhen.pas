program BodyWhen;
{ A conditional group that sits INSIDE a begin/end block. A front end
  cannot choose the branch, so it is forwarded as a Nim `when` in the
  block. The block must NOT end early at the directive. }
procedure p;
begin
  writeln('a');
  {$if 0 <> 0}
  writeln('never');
  {$ifend}
  writeln('b');
end;

begin
  p;
  {$if 1 = 1}
  writeln('taken');
  {$endif}
end.
