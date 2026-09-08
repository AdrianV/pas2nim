program SysUtilsTest;
{ M3-2: SysUtils core - conversions, string helpers, path functions,
  Format with array-of-const (the parser lowers [a, b, c] to toVrec
  calls; the shim takes openArray[TVarRec]). Format supports the
  Delphi spec: index (0-based), '-' left-align, width, precision,
  d/u/s/x/X/e/f/g/G/n/m, %% literal. Float rendering follows C
  printf via nimony's formatBiggestFloat. }
uses SysUtils;
var
  x: Double;
  s: string;
begin
  writeln('hex=', IntToHex(255, 4), ' ', IntToHex(-1, 2));
  writeln('float=', StrToFloatDef('3.25e1', 0.0), ' def=', StrToFloatDef('x', 9.5));
  writeln('trim=[', TrimLeft('  x  '), '][', TrimRight('  x  '), ']');
  writeln('cmpstr=', CompareStr('abc', 'abd'), ' ansiup=', AnsiUpperCase('mix'));
  s := 'C:/home/adrian/report.final.pas';
  writeln('path=', ExtractFilePath(s), '| file=', ExtractFileName(s));
  writeln('dir=', ExtractFileDir(s), '| ext=', ExtractFileExt(s));
  writeln('chgext=', ChangeFileExt('report.pas', '.txt'));
  writeln('incl=', IncludeTrailingPathDelimiter('C:/home'),
    ' excl=', ExcludeTrailingPathDelimiter('C:/home/'));
  writeln('exists=', FileExists('systempas.nim'), ' ', FileExists('nope.nim'));
  x := 2.675;
  writeln('fmt1=', Format('x=%d y=%s', [42, 'hello']));
  writeln('fmt2=', Format('%05d|%-6s|%x|%.2f', [42, 'ab', 255, x]));
  writeln('fmt3=', Format('%1:s and %0:s', ['second', 'first']));
  writeln('fmt4=', Format('100%% %e %g', [1234.5, 0.00025]));
  writeln('fmt5=', Format('chars: %s', [Format('%s', ['nested'])]));
  writeln('fmt6=', Format('empty []', []));
end.
