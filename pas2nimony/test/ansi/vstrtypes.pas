program vstrtypes;
{$APPTYPE CONSOLE}
{ Which variant tag do the string types get on a direct assignment, and can
  the payload be read back through VarToStr? D2007 has no UnicodeString, so
  that arm is FPC-only (and FPC tags it varOleStr, not varUString - measured),
  hence the expect file follows delphi.

  The AnsiString(v) / string(v) read-back *cast* is deliberately NOT
  exercised here: our chain lowers it to a Nim `string(v)`, which nimony does
  not resolve through a converter. That lowering gap is tracked separately and
  is exactly the variant-boundary conversion point the mapping split must
  route through VarToStr / the AnsiString bridge. }

uses SysUtils, Variants;

function Hex(const t: TVarType): string;
begin
  Result := IntToHex(t, 4);
end;

var
  v: Variant;
  a: AnsiString;
  s: string;
  w: WideString;
{$IFDEF FPC}
  u: UnicodeString;
{$ENDIF}
begin
  a := 'A';
  s := 'S';
  w := 'W';
  v := a;  WriteLn('ansi    type=', Hex(VarType(v)), ' str=', VarToStr(v));
  v := s;  WriteLn('string  type=', Hex(VarType(v)), ' str=', VarToStr(v));
  v := w;  WriteLn('wide    type=', Hex(VarType(v)), ' str=', VarToStr(v));
  v := 'L'; WriteLn('literal type=', Hex(VarType(v)), ' str=', VarToStr(v));
{$IFDEF FPC}
  u := 'U';
  v := u;  WriteLn('unicode type=', Hex(VarType(v)), ' str=', VarToStr(v));
{$ENDIF}
end.
