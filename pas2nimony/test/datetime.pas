program DateTimeTest;
{ M3-1: TDateTime - pure nimony core ported from p4n's DateTime.hx
  (encode/decode cascade, rounding-guarded time decomposition,
  unix timestamps). TDateTime is a float64 alias, so Delphi arithmetic
  works: EncodeDate(...) + 1 is the next day. FormatDateTime covers
  the common token subset. Now/Date are UTC (nimony's clock). }
var
  d: TDateTime;
  y, m, dd, h, n, s, ms: Word;
begin
  d := EncodeDate(2024, 5, 17);
  DecodeDate(d, y, m, dd);
  writeln('date=', y, '-', m, '-', dd, ' dow=', DayOfTheWeek(d));
  d := EncodeDateTime(2024, 5, 17, 14, 30, 45);
  DecodeTime(d, h, n, s, ms);
  writeln('time=', h, ':', n, ':', s, ' ms=', ms);
  writeln('fmt=', FormatDateTime('yyyy-mm-dd hh:nn:ss', d));
  writeln('fmt2=', FormatDateTime('ddd, d mmm yyyy (am/pm hh:nn)', d));
  writeln('year=', YearOf(d), ' month=', MonthOf(d), ' day=', DayOf(d));
  d := FromUnixTimestamp(1715961045.0);
  writeln('back=', FormatDateTime('yyyy-mm-dd hh:nn:ss', d));
  writeln('leap=', IsLeapYear(2024), ' ', IsLeapYear(1900));
  writeln('days=', DaysInAMonth(2024, 2), ' ', DaysInAMonth(1900, 2), ' ', DaysInAMonth(1901, 2), ' dt=', DaysInMonth(EncodeDate(2024, 2, 5)));
  writeln('last=', LastDayOfMonth(EncodeDate(2008, 2, 3)));
  writeln('arith=', FormatDateTime('yyyy-mm-dd', EncodeDate(2024, 12, 31) + 1));
  writeln('week=', ISOWeekNumber(EncodeDate(2024, 1, 1)));
end.
