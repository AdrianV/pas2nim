program vcore;
{$APPTYPE CONSOLE}
{ Variant conformance probe, leg 1: type codes, storage types,
  Null/Unassigned/Clear, the core conversions and the operators.

  Must compile and run under BOTH Delphi 2007 (dcc32/Win32) and FPC
  (-Mdelphi), so: no mode directive, no locale-dependent formatting
  (DecimalSeparator is pinned), type codes printed as hex because the
  $01xx Delphi-specific ones are the interesting part. }
uses SysUtils, Variants;

var
  v, w: Variant;
  raised: Boolean;

procedure P(const s: string);
begin
  WriteLn(s);
end;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

{ the raise checks report only *that* something was raised: lowering `on E: T`
  to a `case` over the caught ErrorCode is a nimony toolchain defect today (the
  same one that blocks the corpus unit except.pas), so the probes stay with a
  plain except. The error *class* is measured separately - the oracles differ
  (dcc32 EVariantTypeCastError is a subclass of EVariantError, FPC's is not) -
  and our own hierarchy is pinned by test/variant/vshim.nim, which the same
  toolchain does compile. }
procedure Report(const what: string; wasRaised: Boolean);
begin
  if wasRaised then WriteLn(what);
  raised := False;
end;

procedure Store(const tag: string; const x: Variant);
begin
  v := x;
  WriteLn(tag, ' type=', Hex(VarType(v)));
end;

var
  i32: Integer; i16: SmallInt; i8: ShortInt; u8: Byte; u16: Word;
  u32: Cardinal; i64: Int64; q64: UInt64;
  sg: Single; db: Double; cu: Currency; dt: TDateTime;
  bo: Boolean; ch: Char; an: AnsiString; ws: WideString;
  { NOTE: a WideChar local is unmeasurable on our leg for now - nimony has
    no WideChar and the shim maps nothing onto it (measured divergence:
    dcc32 stores varWord, FPC varOleStr; the shim's uint16 overload answers
    varWord). It needs a shim/emitter mapping of its own, unrelated to the
    Variant work. }
  ss: ShortString; pc: PChar; pt: Pointer;
begin
  DecimalSeparator := '.';

  { layout (sizeof/offsets) lives in vlayout.pas - informational,
    because a boxed Nim Variant cannot match either oracle's record }

  { --- documented type codes --- }
  WriteLn('codes: empty=', Hex(varEmpty), ' null=', Hex(varNull),
          ' smallint=', Hex(varSmallint), ' integer=', Hex(varInteger),
          ' single=', Hex(varSingle), ' double=', Hex(varDouble),
          ' currency=', Hex(varCurrency), ' date=', Hex(varDate),
          ' olestr=', Hex(varOleStr), ' dispatch=', Hex(varDispatch),
          ' error=', Hex(varError), ' boolean=', Hex(varBoolean),
          ' variant=', Hex(varVariant), ' unknown=', Hex(varUnknown),
          ' shortint=', Hex(varShortInt), ' byte=', Hex(varByte),
          ' word=', Hex(varWord), ' longword=', Hex(varLongWord),
          ' int64=', Hex(varInt64), ' string=', Hex(varString));
{$IFDEF FPC}
  { D2007 rejected both of these with E2003 (measured) }
  WriteLn('fpc-only codes: ustring=', Hex(varUString), ' any=', Hex(varAny));
{$ENDIF}

  { --- what each assignment actually stores --- }
  i32 := 70000; i16 := -300; i8 := -5; u8 := 200; u16 := 60000;
  u32 := 4000000000; i64 := -1234567890123; q64 := 10000000000;
  sg := 1.5; db := 2.5; cu := 4.25; dt := 45000.5;
  bo := True; ch := 'Z'; an := 'ansi'; ws := 'wide';
  ss := 'short'; pc := 'pchar'; pt := Pointer(1234);
  Store('unassigned', Unassigned);
  Store('null      ', Null);
  Store('integer   ', i32);
  Store('smallint  ', i16);
  Store('shortint  ', i8);
  Store('byte      ', u8);
  Store('word      ', u16);
  Store('cardinal  ', u32);
  Store('int64     ', i64);
  Store('uint64    ', q64);
  Store('single    ', sg);
  Store('double    ', db);
  Store('currency  ', cu);
  Store('datetime  ', dt);
  Store('boolean   ', bo);
  Store('char      ', ch);
  Store('ansistring', an);
  Store('widestring', ws);
  Store('strliteral', 'literal');
  Store('nested    ', v);
  Store('emptyparam', EmptyParam);

  { --- payload view of a few --- }
  v := 70000;
  WriteLn('payload int: type=', Hex(TVarData(v).VType),
          ' VInteger=', TVarData(v).VInteger);
  v := 4000000000;
  WriteLn('payload card: type=', Hex(TVarData(v).VType),
          ' VLongWord=', TVarData(v).VLongWord,
          ' VInteger=', TVarData(v).VInteger);
  v := i64;
  WriteLn('payload i64: type=', Hex(TVarData(v).VType),
          ' VInt64=', TVarData(v).VInt64);
  v := db;
  WriteLn('payload dbl: type=', Hex(TVarData(v).VType),
          ' VDouble=', TVarData(v).VDouble:0:4);

  { --- Null / Unassigned / Clear --- }
  v := Null; w := Unassigned;
  WriteLn('null: isnull=', VarIsNull(v), ' isempty=', VarIsEmpty(v),
          ' isclear=', VarIsClear(v));
  WriteLn('unassigned: isnull=', VarIsNull(w), ' isempty=', VarIsEmpty(w),
          ' isclear=', VarIsClear(w));
  v := 1;
  WriteLn('int: isnull=', VarIsNull(v), ' isempty=', VarIsEmpty(v),
          ' isclear=', VarIsClear(v));

  { --- conversions --- }
  v := 42;      WriteLn('VarToStr(int)=', VarToStr(v));
  v := i64;     WriteLn('VarToStr(i64)=', VarToStr(v));
  db := 1.5; v := db;
  { VarToStr on a real is *locale* formatted by both oracles (OLE conversion
    semantics: the pinned DecimalSeparator does not reach it), so the payload
    is printed through the typed view - that path does honour the separator.
    The operand is a typed Double: a decimal *literal* would be varCurrency
    under dcc32 (see vlitfloat). }
  WriteLn('VarToStr(dbl) payload*100=', Round(TVarData(v).VDouble * 100));
  v := True;    WriteLn('VarToStr(bool)=', VarToStr(v));
  v := 'abc';   WriteLn('VarToStr(str)=', VarToStr(v));
  v := 4000000000; WriteLn('VarToStr(cardlit)=', VarToStr(v));
  v := u32;     WriteLn('VarToStr(longword)=', VarToStr(v));
  v := Null;    WriteLn('VarToStr(null)=[', VarToStr(v), ']');
  v := Unassigned; WriteLn('VarToStr(unass)=[', VarToStr(v), ']');
  try
    v := 1.9; v := VarAsType(v, varInteger);
    WriteLn('VarAsType(1.9,varInteger)=', VarToStr(v), ' type=', Hex(VarType(v)));
  except raised := True; end;
  Report('VarAsType(1.9) raised', raised);
  try
    v := 2.5; v := VarAsType(v, varInteger);
    WriteLn('VarAsType(2.5,varInteger)=', VarToStr(v));
  except raised := True; end;
  Report('VarAsType(2.5) raised', raised);
  try
    v := '42'; v := VarAsType(v, varInteger);
    WriteLn('VarAsType(''42'',varInteger)=', VarToStr(v));
  except raised := True; end;
  Report('VarAsType(''42'') raised', raised);
  try
    v := 'abc'; v := VarAsType(v, varInteger);
    WriteLn('VarAsType(''abc'',varInteger)=', VarToStr(v));
  except raised := True; end;
  Report('VarAsType(''abc'') raised', raised);

  { the operators are measured in vops2 (typed operands) and the literal
    typing policy in vlitint / vlitfloat - each needs its own sample,
    because a *literal* operand's tag depends on the literal policy alone }
end.
