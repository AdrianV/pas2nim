program varray;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 4: the VarArray* API and indexing.

  Elements are always built from typed variables, so no element tag depends on
  the *literal* policy (that has its own samples). Real payloads are printed
  as scaled integers, like the other probes. }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

function Txt(const v: Variant): string;
begin
  Result := VarToStr(v);
end;

var
  a: Variant;
  i: Integer; d: Double;
  s: AnsiString; ws: WideString;
  b: Boolean; ch: Char; c: Currency;
begin
  DecimalSeparator := '.';
  i := 7; d := 2.5; s := 'two'; ws := 'wide';
  b := True; ch := 'Z'; c := 2.5;

  a := VarArrayCreate([0, 2], varInteger);
  WriteLn('create type=', Hex(VarType(a)), ' isarray=', VarIsArray(a),
          ' dims=', VarArrayDimCount(a),
          ' low=', VarArrayLowBound(a, 1), ' high=', VarArrayHighBound(a, 1));
  a[0] := 10; a[1] := 20; a[2] := 30;
  WriteLn('elems ', Txt(a[0]), ',', Txt(a[1]), ',', Txt(a[2]),
          ' type0=', Hex(VarType(a[0])));
  WriteLn('elem sum=', Txt(a[0] + a[1] + a[2]));

  a := VarArrayOf([i, s, d]);
  WriteLn('of type=', Hex(VarType(a)), ' n=', VarArrayHighBound(a, 1) + 1,
          ' e0=', Hex(VarType(a[0])), ' e1=', Hex(VarType(a[1])),
          ' e2=', Hex(VarType(a[2])));
  WriteLn('of e0=', Txt(a[0]), ' e1=', Txt(a[1]),
          ' e2x100=', Round(TVarData(a[2]).VDouble * 100));

  { how does the compiler convert each *element* of the open `array of
    Variant`? measured: an AnsiString element becomes varOleStr (0008), not
    the varString (0100) that a plain assignment yields }
  a := VarArrayOf([b, ch, c, ws]);
  WriteLn('of mix type=', Hex(VarType(a)),
          ' e0=', Hex(VarType(a[0])), ' e1=', Hex(VarType(a[1])),
          ' e2=', Hex(VarType(a[2])), ' e3=', Hex(VarType(a[3])));
  WriteLn('of mix x10000=', TVarData(a[2]).VInt64, ' e3=', Txt(a[3]));

  a := VarArrayOf([i, s]);
  VarArrayRedim(a, 3);
  WriteLn('redim type=', Hex(VarType(a)), ' dims=', VarArrayDimCount(a),
          ' low=', VarArrayLowBound(a, 1), ' high=', VarArrayHighBound(a, 1));
  a[3] := 99;
  WriteLn('redim e3=', Txt(a[3]), ' e0=', Txt(a[0]), ' e1=', Txt(a[1]));

  WriteLn('isarray int=', VarIsArray(i), ' isarray str=', VarIsArray(s));
  WriteLn('dimcount int=', VarArrayDimCount(i));
end.
