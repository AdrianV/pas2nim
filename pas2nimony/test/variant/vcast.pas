program vcast;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 3: the `Variant(x)` *typecast*.

  An assignment converts, but `Variant(x)` is a cast and need not agree with
  it: the probe measures both spellings side by side for every source type.
  Real text is printed through the typed payload or a scaled integer, because
  VarToStr on a real is locale formatted (see vcore). }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

procedure PI(const what: string; const x: Variant);
begin
  WriteLn(what, ' type=', Hex(VarType(x)), ' val=', VarToStr(x));
end;

var
  v: Variant;
  i: Integer; i64: Int64; d: Double; c: Currency;
  b: Boolean; ch: Char; s: AnsiString;
begin
  DecimalSeparator := '.';

  i := 70000; i64 := -1234567890123; d := 2.5; c := 2.5;
  b := True; ch := 'Z'; s := 'abc';

  PI('cast int  ', Variant(i));
  PI('asgn int  ', i);
  PI('cast i64  ', Variant(i64));
  PI('asgn i64  ', i64);

  v := Variant(d);
  WriteLn('cast dbl   type=', Hex(VarType(v)),
          ' x100=', Round(TVarData(v).VDouble * 100));
  v := d;
  WriteLn('asgn dbl   type=', Hex(VarType(v)),
          ' x100=', Round(TVarData(v).VDouble * 100));

  v := Variant(c);
  WriteLn('cast cur   type=', Hex(VarType(v)), ' x10000=', TVarData(v).VInt64);
  v := c;
  WriteLn('asgn cur   type=', Hex(VarType(v)), ' x10000=', TVarData(v).VInt64);

  PI('cast bool ', Variant(b));
  PI('cast char ', Variant(ch));
  PI('cast str  ', Variant(s));
  PI('cast null ', Variant(Null));
  PI('cast unasn', Variant(Unassigned));
end.
