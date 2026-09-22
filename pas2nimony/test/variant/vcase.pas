program vcase;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 5: the Variant as a *discriminated* union.

  The shim's Variant is a nimony `case object` whose discriminator is the
  measured tag: each tag owns its payload field and the branches alias, so
  Delphi's union views survive. This probe drives every branch through its
  own tag, then reads the payload back three ways the corpus uses:

    1. the tag dispatch `case TVarData(v).VType of` + the branch's own field
       (as the corpus does),
    2. a *cross-view* read of the aliased payload (VInteger of a varLongWord,
       VInt64 of a varCurrency),
    3. `String(TVarData(v).VString)` for the varString branch.

  It also checks that a string payload is the Variant's own copy and that
  constructing/destroying string payloads in a loop is safe - the reason the
  Variant is tag-discriminated at all: a tag-blind destroy would free a
  non-string payload. }
uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

procedure Show(const what: string; const v: Variant);
begin
  Write(what, ' type=', Hex(VarType(v)), ' ');
  case TVarData(v).VType of
    varEmpty:    WriteLn('empty');
    varNull:     WriteLn('null');
    varSmallint: WriteLn('i16=', TVarData(v).VSmallint);
    varInteger:  WriteLn('i32=', TVarData(v).VInteger);
    varShortInt: WriteLn('i8=', TVarData(v).VShortInt);
    varByte:     WriteLn('u8=', TVarData(v).VByte);
    varWord:     WriteLn('u16=', TVarData(v).VWord);
    varLongWord: WriteLn('u32=', TVarData(v).VLongWord);
    varInt64:    WriteLn('i64=', TVarData(v).VInt64);
    varSingle:   WriteLn('f32x100=', Round(TVarData(v).VSingle * 100));
    varDouble:   WriteLn('f64x100=', Round(TVarData(v).VDouble * 100));
    varCurrency: WriteLn('cur=', TVarData(v).VInt64);
    varDate:     WriteLn('datex100=', Round(TVarData(v).VDate * 100));
    varBoolean:  WriteLn('bool VInteger=', TVarData(v).VInteger);
    varString:   WriteLn('str=', String(TVarData(v).VString));
    varOleStr:   WriteLn('wide=', VarToStr(v));
    else         WriteLn('other');
  end;
end;

var
  i8: ShortInt; u8: Byte; i16: Smallint; u16: Word; i32: Integer;
  u32: LongWord; i64: Int64; f32: Single; f64: Double; c: Currency;
  dt: TDateTime; b: Boolean; ch: Char; s: AnsiString; ws: WideString;
  v: Variant; n, k: Integer;
begin
  DecimalSeparator := '.';
  i8 := -5; u8 := 200; i16 := -300; u16 := 60000; i32 := -70000;
  u32 := 4000000000; i64 := -1234567890123; f32 := 1.5; f64 := -2.25;
  c := 3.25; dt := 2.5; b := True; ch := 'Z'; s := 'ansi'; ws := 'wide';

  { every scalar tag through its own branch }
  Show('smallint', i16);
  Show('integer', i32);
  Show('shortint', i8);
  Show('byte', u8);
  Show('word', u16);
  Show('longword', u32);
  Show('int64', i64);
  Show('single', f32);
  Show('double', f64);
  Show('currency', c);
  Show('date', dt);
  Show('boolean', b);
  Show('string', s);
  Show('widestring', ws);
  Show('char', ch);

  { the special three }
  Show('null', Null);
  Show('unassigned', Unassigned);
  Show('emptyparam', EmptyParam);
  WriteLn('null isnull=', VarIsNull(Null), ' isempty=', VarIsEmpty(Null),
          ' unassigned isempty=', VarIsEmpty(Unassigned));

  { the aliased views: every branch shares one payload }
  v := u32;
  WriteLn('view longword VInteger=', TVarData(v).VInteger,
          ' VLongWord=', TVarData(v).VLongWord);
  v := i64;
  WriteLn('view int64 VInteger=', TVarData(v).VInteger);
  v := c;
  WriteLn('view currency VInt64=', TVarData(v).VInt64);
  v := b;
  WriteLn('view bool VInteger=', TVarData(v).VInteger);
  v := b;
  v := v + 1;
  WriteLn('bool+1=', VarToStr(v), ' type=', Hex(VarType(v)));

  { the varString payload is the Variant's own copy: the source may be
    rewritten afterwards (Delphi shares the buffer and detaches on write) }
  s := 'ansi';
  v := s;
  s[1] := 'A';
  WriteLn('payload copy src=', s, ' var=', VarToStr(v),
          ' len=', Length(String(TVarData(v).VString)));

  { string payloads built, copied and destroyed in a loop }
  v := Null;
  k := 0;
  for n := 1 to 50 do
  begin
    v := IntToStr(n);
    k := k + Length(VarToStr(v));
  end;
  WriteLn('loop last=', VarToStr(v), ' lensum=', k);

  { a Variant in an array element and back out }
  v := VarArrayOf([i32, s]);
  WriteLn('array e0=', VarToStr(v[0]), ' e1=', VarToStr(v[1]),
          ' type=', Hex(VarType(v)));
end.
