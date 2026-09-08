program MissingTest;
{ M3-3: DateUtils naming layer, TStringList, SysUtils file ops,
  Format %n, Math.SameValue. }
uses SysUtils, Classes, DateUtils, Math;
var
  L: TStringList;
  d, d2: TDateTime;
begin
  { TStringList }
  L := TStringList.Create;
  L.Add('beta');
  L.Add('alpha');
  L.Add('alpha');
  L.Add('gamma');
  writeln('count=', L.Count, ' first=', L[0]);
  L[0] := 'BETA';
  writeln('set0=', L[0], ' idx-of=', L.IndexOf('gamma'), ' idx-ci=', L.IndexOf('ALPHA'));
  L.Sort;
  writeln('sorted0=', L[0], ' sorted3=', L[3], ' textlen=', Length(L.Text));
  L.Insert(1, 'inserted');
  writeln('after-ins=', L[1], ' ', L[2]);
  L.Delete(1);
  writeln('after-del=', L[1]);
  L.Clear;
  writeln('cleared=', L.Count);
  L.Add('name=Adrian');
  L.Add('city=Berlin');
  L['city'] := 'Munich';
  writeln('value=', L.ValueOf('CITY'), ' name0=', L.NameOfIndex(1));
  L.SaveToFile('shimlist.txt');
  L.Clear;
  L.LoadFromFile('shimlist.txt');
  writeln('reloaded=', L.Count, ' v1=', L.ValueOf('name'));

  { DateUtils }
  d := EncodeDateTime(2024, 5, 17, 14, 30, 45);
  d2 := IncDay(d, 10);
  writeln('inc10=', FormatDateTime('yyyy-mm-dd', d2));
  writeln('incmonth=', FormatDateTime('yyyy-mm-dd', IncMonth(d, 7)),
    ' clamped=', FormatDateTime('yyyy-mm-dd', IncMonth(EncodeDate(2024, 1, 31), 1)));
  writeln('incyear=', YearOf(IncYear(d, 2)));
  writeln('between=', DaysBetween(d2, d), ' hours=', HoursBetween(d2, d));
  writeln('doy=', DayOfTheYear(d), ' week=', WeekOfTheYear(d));
  writeln('startmonth=', FormatDateTime('yyyy-mm-dd', StartOfTheMonth(d)),
    ' endmonth=', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndOfTheMonth(d)));
  writeln('recode=', FormatDateTime('yyyy-mm-dd', RecodeDate(d, 2000, 12, 25)));
  writeln('valid=', IsValidDate(2024, 2, 29), ' ', IsValidDate(1900, 2, 29));

  { SysUtils file ops + Format %n + Math.SameValue }
  writeln('exists=', DirectoryExists('.'), ' created=', CreateDir('shimtmp'),
    ' deleted=', DeleteFile('no-such-file.txt'));
  writeln('pct-n=', Format('%n', [1234567.891]), ' same=', SameValue(0.1 + 0.2, 0.3));
  writeln('ansipos=', AnsiPos('iss', 'Mississippi'));
end.
