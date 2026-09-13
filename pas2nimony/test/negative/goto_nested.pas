program GotoNested;
// A `goto` whose label lives in a block NESTED DEEPER than the goto.
// Delphi allows the jump; Nim has no equivalent, and pas2nimony must
// report it instead of emitting broken code or spinning.
//
// This is the shape that gated a whole corpus closure: the first
// pass of a scan starts at the CURRENT item, so it skips the cursor
// advance - the original jumped past the `inc` from an enclosing
// block into the scan loop. The corpus resolves it as
// `if FFirst then FFirst := False else inc(FItemAdr.Item)`.
label
  lbl_Deeper;
var
  i, n: Integer;
  first: Boolean;
begin
  n := 3;
  i := 0;
  first := true;
  while i < n do begin
    if first then begin
      first := false;
      goto lbl_Deeper;
    end;
    while i < n do begin
      while true do begin
lbl_Deeper:
        i := i + 1;
        break;
      end;
    end;
  end;
  writeln('i=', i);
end.
