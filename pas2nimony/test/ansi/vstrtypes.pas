program vstrtypes;
{$APPTYPE CONSOLE}
{ Which variant tag do the string types get on a direct assignment, and can
  the payload be read back through VarToStr and through the explicit
  AnsiString(v) / string(v) cast? D2007 has no UnicodeString, so that arm is
  FPC-only (and FPC tags it varOleStr, not varUString - measured), hence the
  expect file follows delphi.

  The AnsiString(v) / string(v) cast is the variant-boundary conversion the
  mapping split must route through the AnsiString bridge; the parser now
  lowers it to VarToStr on a Variant, so it is exercised here. }

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
  v := a;  WriteLn('ansi    type=', Hex(VarType(v)), ' str=', VarToStr(v), ' back=', AnsiString(v));
  v := s;  WriteLn('string  type=', Hex(VarType(v)), ' str=', VarToStr(v), ' back=', string(v));
  v := w;  WriteLn('wide    type=', Hex(VarType(v)), ' str=', VarToStr(v));
  v := 'L'; WriteLn('literal type=', Hex(VarType(v)), ' str=', VarToStr(v), ' back=', AnsiString(v));
{$IFDEF FPC}
  u := 'U';
  v := u;  WriteLn('unicode type=', Hex(VarType(v)), ' str=', VarToStr(v));
{$ENDIF}
end.
