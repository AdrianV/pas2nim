program vlayout;
{$APPTYPE CONSOLE}
{ Variant *layout* probe - INFORMATIONAL ONLY (variant/vlayout.expect = none).

  Neither oracle's numbers can be ours: our Variant is a boxed Nim object
  (a tag plus synchronised union views), not a 16-byte (dcc32/Win32) or
  24-byte (FPC x86-64) TVarData record. The measurements below are still
  worth pinning - they document what the value semantics must reproduce,
  and they show VType at offset 0 and the payload at 8 in BOTH oracles,
  which is why the shim keeps `VType` as the first field. }
uses SysUtils, Variants;

type
  TOfs = {$IFDEF FPC} PtrUInt {$ELSE} Cardinal {$ENDIF};

var
  v: Variant;
begin
  WriteLn('sizeof(Variant)=', SizeOf(Variant));
  WriteLn('sizeof(TVarData)=', SizeOf(TVarData));
  WriteLn('sizeof(TVarType)=', SizeOf(TVarType));
  v := 1;
  WriteLn('off(VType)=', TOfs(@TVarData(v).VType) - TOfs(@TVarData(v)));
  WriteLn('off(VInteger)=', TOfs(@TVarData(v).VInteger) - TOfs(@TVarData(v)));
  WriteLn('off(VInt64)=', TOfs(@TVarData(v).VInt64) - TOfs(@TVarData(v)));
  WriteLn('off(VDouble)=', TOfs(@TVarData(v).VDouble) - TOfs(@TVarData(v)));
  WriteLn('off(VOleStr)=', TOfs(@TVarData(v).VOleStr) - TOfs(@TVarData(v)));
end.
