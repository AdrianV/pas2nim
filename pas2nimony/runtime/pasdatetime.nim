{.feature: "lenientnils".}
#
# pasdatetime - pure nimony TDateTime core
#
# Ported from p4n's DateTime.hx (Adrian Veith, MPL 1.1). The date is
# encoded in the integer part of a float counted from 12/30/1899, the
# time in the fractional part - Delphi/FPC TDateTime semantics.
# TDateTime is a plain float64 alias, so Pascal arithmetic
# (D + 1 = next day) works unchanged.
#
# Nimony notes baked in (same as systempas): `proc` (not `func`),
# no {.raises.}, `result = ""` before incremental string building.
#
# UTC note: Now/Date read nimony's std/times clock, which is UTC; a
# local-timezone offset is future work.

import std/[times, math, strutils]

type
  TDateTime* = float64

const
  DateDelta* = 693594          # days from 12/30/1899 back to year 1
  UnixStart* = 25569.0         # TDateTime of the unix epoch 1970-01-01

proc IsLeapYear*(year: int32): bool =
  (year mod 4 == 0) and ((year mod 100 != 0) or (year mod 400 == 0))

proc DaysInMonthCore(year, month: int32): int32 =
  case month
  of 1, 3, 5, 7, 8, 10, 12: result = 31
  of 4, 6, 9, 11: result = 30
  of 2:
    if IsLeapYear(year): result = 29 else: result = 28
  else: result = 0

proc DaysInAMonth*(year, month: int32): int32 =
  ## Delphi/FPC two-argument form
  DaysInMonthCore(year, month)

proc DaysInMonth*(dt: TDateTime): int32 =
  ## Delphi/FPC: DaysInMonth takes a TDateTime
  DaysInMonthCore(YearOf(dt), MonthOf(dt))

proc EncodeDate*(year, month, day: int32): TDateTime =
  ## Delphi EncodeDate; 0.0 for out-of-range input
  if (year < 1) or (year > 9999) or (month < 1) or (month > 12) or
      (day < 1) or (day > DaysInMonthCore(year, month)):
    return 0.0
  var doy = int(day)
  var m = 1
  while m < int(month):
    doy = doy + int(DaysInMonthCore(year, int32(m)))
    inc m
  let i = int(year) - 1
  result = float64(i * 365 + i div 4 - i div 100 + i div 400 + doy -
      DateDelta)

proc EncodeTime*(hour, minute, second, msec: int32): TDateTime =
  result = float64(int(hour)) / 24.0 + float64(int(minute)) / 1440.0 +
      float64(int(second)) / 86400.0 + float64(int(msec)) / 86400000.0

proc EncodeDateTime*(year, month, day, hour, minute, second: int32): TDateTime =
  EncodeDate(year, month, day) + EncodeTime(hour, minute, second, 0)

proc DecodeDateCore*(dt: TDateTime; year, month, day: var int32) =
  ## the year/month/day cascade from DateTime.hx decode()
  var t = int(dt) + DateDelta
  if t <= 0:
    year = 0
    month = 0
    day = 0
    return
  dec t
  var y = 1
  while t >= 146097:           # D400
    t = t - 146097
    y = y + 400
  var i = t div 36524          # D100
  var d = t mod 36524
  if i == 4:
    i = 3
    d = d + 36524
  y = y + i * 100
  i = d div 1461               # D4
  d = d mod 1461
  y = y + i * 4
  i = d div 365                # D1
  d = d mod 365
  if i == 4:
    i = 3
    d = d + 365
  y = y + i
  # d is the 0-based day of year; walk the months
  var m = 1
  while m <= 12 and d >= int(DaysInMonthCore(int32(y), int32(m))):
    d = d - int(DaysInMonthCore(int32(y), int32(m)))
    inc m
  year = int32(y)
  month = int32(m)
  day = int32(d + 1)

proc DecodeDate*(dt: TDateTime; year, month, day: var uint16) =
  ## Delphi/FPC signature: Word var params
  var y: int32 = 0
  var m: int32 = 0
  var d: int32 = 0
  DecodeDateCore(dt, y, m, d)
  year = uint16(y)
  month = uint16(m)
  day = uint16(d)

proc YearOf*(dt: TDateTime): int32 =
  var y: int32 = 0
  var m: int32 = 0
  var d: int32 = 0
  DecodeDateCore(dt, y, m, d)
  result = y

proc MonthOf*(dt: TDateTime): int32 =
  var y: int32 = 0
  var m: int32 = 0
  var d: int32 = 0
  DecodeDateCore(dt, y, m, d)
  result = m

proc DayOf*(dt: TDateTime): int32 =
  var y: int32 = 0
  var m: int32 = 0
  var d: int32 = 0
  DecodeDateCore(dt, y, m, d)
  result = d

proc TimeValue*(dt: TDateTime): TDateTime =
  ## the time fraction of the value
  result = dt - float64(int(dt))

proc DecodeTimeCore*(dt: TDateTime; hour, minute, second, msec: var int32) =
  ## the rounding guard from DateTime.hx decodeTime: nudge by half a
  ## step away from the 1.0 boundary before decomposing
  var t = min(1.0 - 0.00005 / 86400.0, TimeValue(dt) + 0.00005 / 86400.0) * 24.0
  var h = int(t)
  t = (t - float64(h)) * 60.0
  var m = int(t)
  t = (t - float64(m)) * 60.0
  var secFrac = round(t * 1000.0)
  var s = int64(secFrac / 1000.0)
  var ms = int64(secFrac) mod 1000
  hour = int32(h)
  minute = int32(m)
  second = int32(s)
  msec = int32(ms)

proc DecodeTime*(dt: TDateTime; hour, minute, second, msec: var uint16) =
  ## Delphi/FPC signature: Word var params
  var h: int32 = 0
  var n: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeTimeCore(dt, h, n, s, ms)
  hour = uint16(h)
  minute = uint16(n)
  second = uint16(s)
  msec = uint16(ms)

proc HourOf*(dt: TDateTime): int32 =
  var h: int32 = 0
  var n: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeTimeCore(dt, h, n, s, ms)
  result = h

proc MinuteOf*(dt: TDateTime): int32 =
  var h: int32 = 0
  var n: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeTimeCore(dt, h, n, s, ms)
  result = n

proc SecondOf*(dt: TDateTime): int32 =
  var h: int32 = 0
  var n: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeTimeCore(dt, h, n, s, ms)
  result = s

proc MilliSecondOf*(dt: TDateTime): int32 =
  var h: int32 = 0
  var n: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeTimeCore(dt, h, n, s, ms)
  result = ms

proc DayOfTheWeek*(dt: TDateTime): int32 =
  ## Delphi convention: Monday = 1 .. Sunday = 7
  result = int32((int(dt) + 5) mod 7 + 1)

proc LastDayOfMonth*(dt: TDateTime): int32 =
  DaysInMonthCore(YearOf(dt), MonthOf(dt))

proc MonthDelta*(a, b: TDateTime): int32 =
  ## whole months between a and b
  result = int32((int(MonthOf(a)) - int(MonthOf(b))) +
      12 * (int(YearOf(a)) - int(YearOf(b))))

proc ISOWeekNumber*(dt: TDateTime): int32 =
  ## ISO week number (first day Monday, week 1 contains Jan 4)
  let dow = int(DayOfTheWeek(dt))
  let day4 = dt - float64(dow) + 8.0 - 4.0
  let y4 = YearOf(day4)
  let jan1 = EncodeDate(y4, 1, 1)
  result = int32(int((day4 - jan1) / 7.0) + 1)

proc EasterSunday*(year: int32): TDateTime =
  ## Easter Sunday for a year (the anonymous Gregorian algorithm)
  let a = int(year) mod 19
  var b = (204 - 11 * a) mod 30
  if b == 28 or b == 29:
    dec b
  let c = (int(year) + int(year) div 4 + b - 13) mod 7
  var day = 28 + b - c - 2
  var month = 3
  if day > 31:
    day = day - 31
    month = 4
  result = EncodeDate(year, int32(month), int32(day))

proc FromUnixTimestamp*(sec: float64): TDateTime =
  UnixStart + sec / 86400.0

proc ToUnixTimestamp*(dt: TDateTime): float64 =
  (dt - UnixStart) * 86400.0

proc Now*(): TDateTime =
  ## the current UTC date and time (nimony's clock is UTC; a local
  ## timezone offset is future work)
  let n = now()
  let ms = float64(int(n.nanosecond)) / 1000000.0
  EncodeDateTime(int32(n.year), int32(int(n.month)), n.monthday, n.hour,
      n.minute, n.second) + ms / 86400000.0

proc Date*(): TDateTime =
  ## today without the time fraction
  result = float64(int(Now()))

# ---------------------------------------------------------------------------
# FormatDateTime (Delphi token syntax, common subset)

proc pad2(v: int): string =
  result = ""
  if v < 10: result.add('0')
  result.add($v)

proc pad3(v: int): string =
  result = ""
  if v < 100: result.add('0')
  if v < 10: result.add('0')
  result.add($v)

const
  MonthNames* = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                 "Sep", "Oct", "Nov", "Dec"]
  DayNames* = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

proc FormatDateTime*(fmt: string; dt: TDateTime): string =
  ## Delphi FormatDateTime common subset: yyyy yy mmmm mmm mm m
  ## dddd ddd dd d hh h nn n ss s zzz am/pm; other characters are
  ## literal. Month/day names are English. h/hh switch to 12-hour
  ## when the format contains am/pm.
  var y: int32 = 0
  var mo: int32 = 0
  var d: int32 = 0
  var h: int32 = 0
  var mi: int32 = 0
  var s: int32 = 0
  var ms: int32 = 0
  DecodeDateCore(dt, y, mo, d)
  DecodeTimeCore(dt, h, mi, s, ms)
  let hasAmPm = find(fmt, "am/pm", 0) >= 0
  result = ""
  var p = 0
  while p < fmt.len:
    var matched = false
    const tokens = ["yyyy", "yy", "mmmm", "mmm", "mm", "m",
                    "dddd", "ddd", "dd", "d", "zzz", "hh", "h",
                    "nn", "n", "ss", "s", "am/pm"]
    for tok in tokens:
      if p + tok.len <= fmt.len and substr(fmt, p, p + tok.len - 1) == tok:
        matched = true
        p = p + tok.len
        case tok
        of "yyyy": result.add($int(y))
        of "yy": result.add(pad2(int(y) mod 100))
        of "mmmm": result.add(MonthNames[int(mo) - 1])  # full names: v1 = abbrev
        of "mmm": result.add(MonthNames[int(mo) - 1])
        of "mm": result.add(pad2(int(mo)))
        of "m": result.add($int(mo))
        of "dddd": result.add(DayNames[(int(dt) + 5) mod 7])
        of "ddd": result.add(DayNames[(int(dt) + 5) mod 7])
        of "dd": result.add(pad2(int(d)))
        of "d": result.add($int(d))
        of "zzz": result.add(pad3(int(ms)))
        of "hh", "h":
          var hh = int(h)
          if hasAmPm and hh > 12: hh = hh - 12
          if tok == "hh": result.add(pad2(hh)) else: result.add($hh)
        of "nn": result.add(pad2(int(mi)))
        of "n": result.add($int(mi))
        of "ss": result.add(pad2(int(s)))
        of "s": result.add($int(s))
        of "am/pm":
          if h < 12: result.add("AM") else: result.add("PM")
        else: discard
        break
    if not matched:
      result.add(fmt[p])
      inc p

proc DateTimeToStr*(dt: TDateTime): string =
  FormatDateTime("yyyy-mm-dd hh:nn:ss", dt)