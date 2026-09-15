program intindex;

{ Locks the type-fidelity fixes for narrow integer expressions:
  - a bare literal adapting to a Byte array element (d[0] and $3F)
  - mixed-width and/or promotion (Byte or Integer)
  - a Byte index into a Byte array (nimony allows only a 32-bit index)
  - narrowing assignment (Word -> Byte) }

var
  d: array[0..3] of Byte;
  b: Byte;
  c: Byte;
  w: Word;
  i: Integer;

begin
  d[0] := 5;
  d[1] := 3;
  d[2] := 200;
  d[3] := 7;

  c := (d[0] and $3F) shl 2 + (d[1] and $30) shr 4;
  WriteLn(c);

  i := 16;
  c := d[1] or i;
  WriteLn(c);

  for b := 0 to 3 do
    WriteLn(d[b]);

  w := 300;
  c := w;
  WriteLn(c);
end.
