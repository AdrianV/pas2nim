{.feature: "lenientnils".}
#
# pasdateutils - Delphi DateUtils naming layer over the TDateTime
# core (pasdatetime, re-exported by systempas). `uses DateUtils`
# resolves here. Inc* use plain float arithmetic except IncMonth
# (calendar-aware, the day clamped to the target month's length).
# The Between* family truncates whole units; MonthsBetween and
# YearsBetween are approximations (30.4375-day months, 365.25-day
# years - Delphi weighs days of month differently).

import pasdatetime

proc DateOf*(dt: TDateTime): TDateTime = float64(int(dt))
proc TimeOf*(dt: TDateTime): TDateTime = TimeValue(dt)

proc Today*(): TDateTime = Date()
proc Yesterday*(): TDateTime = Date() - 1.0
proc Tomorrow*(): TDateTime = Date() + 1.0

proc IncDay*(dt: TDateTime; days: int32 = 1): TDateTime =
  dt + float64(int(days))

proc IncWeek*(dt: TDateTime; weeks: int32 = 1): TDateTime =
  dt + float64(int(weeks)) * 7.0

proc IncHour*(dt: TDateTime; hours: int32 = 1): TDateTime =
  dt + float64(int(hours)) / 24.0

proc IncMinute*(dt: TDateTime; minutes: int32 = 1): TDateTime =
  dt + float64(int(minutes)) / 1440.0

proc IncSecond*(dt: TDateTime; seconds: int32 = 1): TDateTime =
  dt + float64(int(seconds)) / 86400.0

proc IncMilliSecond*(dt: TDateTime; msec: int32 = 1): TDateTime =
  dt + float64(int(msec)) / 86400000.0

proc IncMonth*(dt: TDateTime; months: int32 = 1): TDateTime =
  var y: int32 = 0
  var m: int32 = 0
  var d: int32 = 0
  DecodeDateCore(dt, y, m, d)
  var mo = int(m) + int(months)
  var yr = int(y)
  while mo > 12:
    mo = mo - 12
    inc yr
  while mo < 1:
    mo = mo + 12
    dec yr
  if yr < 1 or yr > 9999:
    return 0.0
  var dd = d
  let dim = DaysInAMonth(int32(yr), int32(mo))
  if dd > dim: dd = dim
  EncodeDate(int32(yr), int32(mo), dd) + TimeValue(dt)

proc IncYear*(dt: TDateTime; years: int32 = 1): TDateTime =
  IncMonth(dt, int32(int(years) * 12))

proc absDT(dt: TDateTime): TDateTime =
  if dt < 0.0: -dt else: dt

proc DaysBetween*(a, b: TDateTime): int32 =
  int32(int(absDT(a - b)))

proc WeeksBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) / 7.0)))

proc HoursBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) * 24.0)))

proc MinutesBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) * 1440.0)))

proc SecondsBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) * 86400.0)))

proc MilliSecondsBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) * 86400000.0)))

proc MonthsBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) / 30.4375)))

proc YearsBetween*(a, b: TDateTime): int32 =
  int32(int(absDT((a - b) / 365.25)))

proc DayOfTheMonth*(dt: TDateTime): int32 = DayOf(dt)
proc MonthOfTheYear*(dt: TDateTime): int32 = MonthOf(dt)

proc DayOfTheYear*(dt: TDateTime): int32 =
  int32(int(dt - EncodeDate(YearOf(dt), 1, 1)) + 1)

proc WeekOfTheYear*(dt: TDateTime): int32 = ISOWeekNumber(dt)

proc EndOfDayFrac: TDateTime = EncodeTime(23, 59, 59, 999)

proc StartOfTheDay*(dt: TDateTime): TDateTime = float64(int(dt))
proc EndOfTheDay*(dt: TDateTime): TDateTime =
  float64(int(dt)) + EndOfDayFrac()

proc StartOfTheMonth*(dt: TDateTime): TDateTime =
  EncodeDate(YearOf(dt), MonthOf(dt), 1)

proc EndOfTheMonth*(dt: TDateTime): TDateTime =
  EncodeDate(YearOf(dt), MonthOf(dt), DaysInAMonth(YearOf(dt), MonthOf(dt))) +
      EndOfDayFrac()

proc StartOfTheYear*(dt: TDateTime): TDateTime =
  EncodeDate(YearOf(dt), 1, 1)

proc EndOfTheYear*(dt: TDateTime): TDateTime =
  EncodeDate(YearOf(dt), 12, 31) + EndOfDayFrac()

proc StartOfAMonth*(year, month: int32): TDateTime =
  EncodeDate(year, month, 1)

proc EndOfAMonth*(year, month: int32): TDateTime =
  EncodeDate(year, month, DaysInAMonth(year, month)) + EndOfDayFrac()

proc StartOfAYear*(year: int32): TDateTime = EncodeDate(year, 1, 1)
proc EndOfAYear*(year: int32): TDateTime =
  EncodeDate(year, 12, 31) + EndOfDayFrac()

proc IsSameDay*(a, b: TDateTime): bool = int(a) == int(b)

proc RecodeDate*(dt: TDateTime; year, month, day: int32): TDateTime =
  EncodeDate(year, month, day) + TimeValue(dt)

proc RecodeTime*(dt: TDateTime; hour, minute, second, msec: int32): TDateTime =
  float64(int(dt)) + EncodeTime(hour, minute, second, msec)

proc IsValidDate*(year, month, day: int32): bool =
  (year >= 1) and (year <= 9999) and (month >= 1) and (month <= 12) and
      (day >= 1) and (day <= DaysInAMonth(year, month))

proc IsValidTime*(hour, minute, second, msec: int32): bool =
  (hour >= 0) and (hour <= 23) and (minute >= 0) and (minute <= 59) and
      (second >= 0) and (second <= 59) and (msec >= 0) and (msec <= 999)

proc IsValidDateTime*(year, month, day, hour, minute, second, msec: int32): bool =
  IsValidDate(year, month, day) and IsValidTime(hour, minute, second, msec)