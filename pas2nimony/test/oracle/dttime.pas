program DateTimeOracle;
{ M5 oracle: TDateTime core + DateUtils, deterministic (no Now) }
uses SysUtils, DateUtils;
var
  d, d2: TDateTime;
  y, m, dd: Word;
begin
  d := EncodeDate(2024, 5, 17);
  DecodeDate(d, y, m, dd);
  writeln('round=', y, '-', m, '-', dd, ' dow=', DayOfTheWeek(d));
  writeln('fmt=', FormatDateTime('yyyy-mm-dd', d));
  writeln('dow2=', DayOfTheWeek(EncodeDate(1900, 1, 1)), ' ',
    DayOfTheWeek(EncodeDate(2000, 1, 1)));
  writeln('dim=', DaysInAMonth(2024, 2), ' ', DaysInAMonth(1900, 2), ' ',
    DaysInAMonth(1901, 2), ' leap=', IsLeapYear(2024), ' ', IsLeapYear(1900),
    ' dt=', DaysInMonth(EncodeDate(2024, 2, 5)));
  d2 := IncDay(d, 10);
  writeln('inc10=', FormatDateTime('yyyy-mm-dd', d2));
  writeln('incm=', FormatDateTime('yyyy-mm-dd', IncMonth(d, 7)),
    ' clamp=', FormatDateTime('yyyy-mm-dd', IncMonth(EncodeDate(2024, 1, 31), 1)));
  writeln('incy=', YearOf(IncYear(d, 2)));
  writeln('between=', DaysBetween(d2, d), ' ', HoursBetween(d2, d));
  writeln('doy=', DayOfTheYear(d), ' week=', WeekOfTheYear(d));
  writeln('som=', FormatDateTime('yyyy-mm-dd', StartOfTheMonth(d)),
    ' eom=', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndOfTheMonth(d)));
  writeln('rec=', FormatDateTime('yyyy-mm-dd', RecodeDate(d, 2000, 12, 25)));
  writeln('valid=', IsValidDate(2024, 2, 29), ' ', IsValidDate(1900, 2, 29));
  writeln('ft=', FloatToStr(EncodeTime(14, 30, 45, 0)));
end.
