/**************************************************************************************************
{                                                                                                  
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); 
{ you may not use this file except in compliance with the License. You may obtain a copy of the    
{ License at http://www.mozilla.org/MPL/                                                           
{                                                                                                  
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   
{ ANY KIND, either express or implied. See the License for the specific language governing rights  
{ and limitations under the License.                                                               
{                                                                                                  
{ The Original Code is TDateTime.hx.                                                                   
{                                                                                                  
{ The Initial Developer of the Original Code is Adrian Veith <adrian@veith-system.de>.             
{ Portions created by Adrian Veith Copyright (C) Adrian Veith. All Rights Reserved.                
{                                                                                                  
{ Contributors:                                                                                    
{   Adrian Veith                                                                                   
{                                                                                                  
{**************************************************************************************************/

package p4n;

typedef DateRec = {
	var day: Int;
	var month: Int;
	var year: Int;
}

typedef TimeRec = {
	var hour: Int;
	var minute: Int;
	var sec: Float;	
}

typedef DateTimeRec = { > DateRec,
	> TimeRec,
}

typedef WeekNumber = {
	week: Int,
	year: Int,
	weekDay: WeekDays,
}

@:forward
enum abstract WeekDays(Int) from Int to Int {
	var Mon = 0;
	var Tue = 1;
	var Wed = 2;
	var Thu = 3;
	var Fri = 4;
	var Sat = 5;
	var Sun = 6;
	inline public function isWeekend(): Bool return this == Sat || this == Sun;
	@:op(A - B) static function sub1(lhs:Int, rhs:WeekDays):Int;
	@:op(A + B) static function add1(lhs:Int, rhs:WeekDays):Int;
	@:op(A < B) static function less(lhs:WeekDays, rhs:WeekDays):Bool;
	@:op(A <= B) static function leq(lhs:WeekDays, rhs:WeekDays):Bool;
	@:op(A == B) static function eq(lhs:WeekDays, rhs:WeekDays):Bool;
	@:op(A => B) static function qeq(lhs:WeekDays, rhs:WeekDays):Bool;
	@:op(A > B) static function greater(lhs:WeekDays, rhs:WeekDays):Bool;
	@:op(A != B) static function neq(lhs:WeekDays, rhs:WeekDays):Bool;
	public static var days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
	public function toString() return days[this];		

}


/**
	DateTime encodes the Date and Time in a Float, compatible to the Delphi and Freepascal TDateTime type. The Date value is 
	encoded in the Int part of the Float counted from 12/30/1899. The Time value is encoded in the fractional part of the Float.
**/
#if (js && !nodejs) @:expose("p4n.DateTime") #end
@:nullSafety
//@:notNull
abstract DateTime(Float) from Float to Float
{
	static inline var DATE_DELTA: Int = 693594;
	
	static var MD0(default, never) = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; 
	static var MD1(default, never) = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; 
	static inline var D1: Int = 365;
	static inline var D4: Int = D1 * 4 + 1;
	static inline var D100: Int = D4 * 25 - 1;
	static inline var D400: Int = D100 * 4 + 1;
	static public inline var HOURS_PER_DAY = 24;
	static public inline var MINUTES_PER_DAY = (24 * 60);
	static public inline var SECONDS_PER_DAY = (24 * 60 * 60);
	static public inline var HOURS: Float = 1 / HOURS_PER_DAY;
	static public inline var MINUTES: Float = 1 / MINUTES_PER_DAY;
	static public inline var SECONDS: Float = 1 / SECONDS_PER_DAY;
	static public inline var UNIX_START = 25569;

  static final MD0S = [0,31,59,90,120,151,181,212,243,273,304,334,365];

	static final MD1S = [0,31,60,91,121,152,182,213,244,274,305,335,366];

	/**
	 * the default first day in the week is Monday. Adjust to your needs
	 */
	static public var ISOFirstWeekDay: WeekDays = Mon; 
	/**
	 * by default the first week in the year, is the week which contains the 4.th January
	 */
	static public var ISOFirstWeekMinDays: Int = 4; 

	@:op(A + B) static function add(lhs:DateTime, rhs:DateTime):DateTime;
	@:commutative @:op(A + B) static function add1(lhs:DateTime, rhs:Float):DateTime;
	@:commutative @:op(A + B) static function add2(lhs:DateTime, rhs:Int):DateTime;
	@:commutative @:op(A * B) static function mul(lhs:DateTime, rhs:Float):DateTime;
	@:commutative @:op(A * B) static function mul1(lhs:DateTime, rhs:Int):DateTime;
	@:op(A - B) static function sub1(lhs:DateTime, rhs:Float):DateTime;
	@:op(A - B) static function sub2(lhs:DateTime, rhs:DateTime):DateTime;
	@:op(A - B) static function sub3(lhs:Float, rhs:DateTime):DateTime;
	@:op(A / B) static function div1(lhs:DateTime, rhs:Float):Float;
	@:op(A < B) static function lt(lhs:DateTime, rhs:DateTime):Bool;
	@:op(A <= B) static function lte(lhs:DateTime, rhs:DateTime):Bool;
	@:op(A == B) static function eq(lhs:DateTime, rhs:DateTime):Bool;
	@:op(A != B) static function neq(lhs:DateTime, rhs:DateTime):Bool;
	@:op(A >= B) static function gte(lhs:DateTime, rhs:DateTime):Bool;
	@:op(A > B) static function gt(lhs:DateTime, rhs:DateTime):Bool;

	@:op(A < B) static function lt1(lhs:DateTime, rhs:Float):Bool;
	@:op(A <= B) static function lte1(lhs:DateTime, rhs:Float):Bool;
	@:op(A == B) static function eq1(lhs:DateTime, rhs:Float):Bool;
	@:op(A != B) static function neq1(lhs:DateTime, rhs:Float):Bool;
	@:op(A >= B) static function gte1(lhs:DateTime, rhs:Float):Bool;
	@:op(A > B) static function gt1(lhs:DateTime, rhs:Float):Bool;
	
	public inline function new(v: Float) this = v;
	
	//@:to inline function toFloat(): Float return this;
	//@:from static inline function fromFloat(v: Float): DateTime return new DateTime(v);
	
	public inline function isInitialized(): Bool {
		#if (neko || js)
			return this != null && this != 0.0;
		#else
			return this != 0.0;
		#end
	}
	
	public static inline function isLeapYear(year: Int): Bool {
		return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
	}
	
	public static function encode(year: Int, month: Int, day: Int): DateTime {
		final dayTable = if (isLeapYear(year)) MD1 else MD0;
		if ((year >= 1) && (year <= 9999) && (month >= 1) && (month <= 12) 
			&& (day >= 1) && (day <= dayTable[month]))
		{
			if (month > 1) {
				day += if (isLeapYear(year)) MD1S[month-1] else MD0S[month-1];
			}
			var I = year - 1;
			return (I * 365 + Math.floor(I / 4) - Math.floor(I / 100) + Math.floor(I / 400) + day - DATE_DELTA);
		} else
			return 0.0;
	}
	
	public static inline function encodeTime(hour: Int, minute: Int, sec: Float): DateTime {
		return ((hour * HOURS) + minute * MINUTES + sec * SECONDS);
	}
	public static function encodeDateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, sec: Float): DateTime {
		return (encode(year, month, day) + (hour * HOURS) + minute * MINUTES + sec * SECONDS);
	}
	
	public inline function abs(): DateTime return Math.abs(this);
	
	public function decode(): DateRec {
		#if (neko || js)
		if (this == null) return { day:0, month: 0, year: 0 };
		#end
		if (Math.isNaN(this)) return { day:0, month: 0, year: 0 };
		var t: Int = toInt() + DATE_DELTA;
		//if (Math.isNaN(T)) return { day:0, month: 0, year: 0 };
		if (t <= 0) {
			return { day: 0, year: 0, month: 0 };
		} else {
			t--;
			var y: Int = 1;
			while (t >= D400) {
				t -= D400;
				y += 400;
			}
			var i = Math.floor(t / D100);
			var d: Int = t % D100;
			if (i == 4) {
				i--;
				d += D100;
			}
			y += i * 100;
			i = Math.floor(d / D4);
			d = d % D4;
			y += i * 4;
			i = Math.floor(d / D1);
			d = d % D1;
			if (i == 4) {
				i--;
				d += D1;
			}
			y += i;
			final dayTable = if (isLeapYear(y)) MD1S else MD0S;
      var m = Std.int(d / 29) + 1;
      var dmax = dayTable[m-1];
      if (d < dmax) {
        m--;
        dmax = dayTable[m-1];
      }
      return { day: d - dmax + 1, month: m, year: y };
		}
	}
		
	/**
	 * returns the year value of a DateTime
	 * e.g. 11/28/2015 -> 2015
	 **/
	public inline function year(): Int {
		return decode().year;
	}
	
	/**
	 * returns the month value of a DateTime
	 * e.g. 11/28/2015 -> 11
	 **/
	public inline function month(): Int {
		return decode().month;
	}
	
	/**
	 * returns the day value of a DateTime
	 * e.g. 11/28/2015 -> 28
	 **/
	public inline function day(): Int {
		return decode().day;
	}
	
	/**
	 * calculates how many month are between to DateTimes
	 */
	public function monthDelta(d2: DateTime): Int {
		var dd1 = decode();
		var dd2 = d2.decode();
		return (dd1.month - dd2.month) + 12 * (dd1.year - dd2.year);
	}
	
	/**
	 * calculates the last day of the DateTime
	 * DateTime.lastDayOf(2, 2008) -> 29
	 * @param	month
	 * @param	year
	 * @return  the last day value of that month
	 */
	public static function lastDayOf(month: Int, year: Int): Int {
		return if (isLeapYear(year)) MD1[month] else MD0[month];
	}
	
	/**
	 * calculates the last day of the DateTime
	 * `DateTime.encode(2008,2,3).lastDayOfMonth()` -> `29`
	 * @return last day of the DateTime
	 */
	public function lastDayOfMonth(): Int {
		var dt: DateRec = decode();
		return if (isLeapYear(dt.year)) MD1[dt.month] else MD0[dt.month];
	}
	
	public function dayOfWeek(): WeekDays {
		// Mo = 0; Sun= 6
		return (Math.floor(this) +5) % 7;
	}
	
	public function fixDay(day: Int): DateTime {
		var dt: DateRec = decode();
		return encode(dt.year, dt.month, day);
	}
	
	public function ISOWeekNumber(?first: WeekDays, ? minDays: Int): WeekNumber {
		if (first == null) first = ISOFirstWeekDay;
		if (minDays == null) minDays = ISOFirstWeekMinDays;
		//var YearOfWeekNumber, WeekDay: Integer): Integer;
		var weekDay : WeekDays = ((dayOfWeek() - first + 7) % 7) + 1;
		var day4: DateTime = this - weekDay + 8 - minDays;
		var dt: DateRec = day4.decode();
		return { week: Math.floor((day4 - encode(dt.year, 1, 1)) / 7.0) +1, year: dt.year, weekDay: weekDay };
	}
	
	public inline function weekNumber(): Int {
		return ISOWeekNumber().week;
	}

	public function decodeDateTime(): DateTimeRec {
		var dt: DateRec = decode();
		//trace(dt);
		var t = Math.min(1 - 0.00005 * SECONDS, timeValue() + 0.00005 * SECONDS) * 24;
		var h: Int = Math.floor(t);
		t = (t - h) * 60;
		var m = Math.floor(t);
		t = (t - m) * 60;
		return {year: dt.year, month: dt.month, day: dt.day, hour: h, minute: m, sec: MathX.round(t, 3) };
	}
	
	public function decodeTime(): TimeRec {
		var t = Math.min(1 - 0.00005 * SECONDS, timeValue() + 0.00005 * SECONDS) * 24;
		var h: Int = Math.floor(t);
		t = (t - h) * 60;
		var m = Math.floor(t);
		t = (t - m) * 60;
		return { hour: h, minute: m, sec: MathX.round(t, 3) };		
	}
	
	//public static function DecodeDateTime(dt: Float): DateTimeRec { return if (dt != null) TDateTime.fromFloat(dt).decodeDateTime() else null; }
	
	public inline function format(f: String) return if (isInitialized()) DateTimeFormatter.format(this, f) else "";
	
	@:to public inline function toInt(): Int { return Math.floor(this); }
	
	/**
	  returns the time fraction of the DateTime value 
	**/
	public function timeValue(): DateTime { return this - Math.floor(this); }
	
	/**
	  returns the DateTime of the day without the time fraction
	**/
	public function dayValue(): DateTime return Math.floor(this);
	
	@:to public function toDate(): Date { 
		//trace(me);
		var dt: DateTimeRec = decodeDateTime();
		return new Date(dt.year, dt.month -1, dt.day, dt.hour, dt.minute, Math.floor(dt.sec));
	}
	
	@:to public function toString(): String return Std.string(this);
	
	public static function EasterSunday(year: Int): DateTime {
		var a :Int = year % 19;
		var b : Int = (204-11*a) % 30;
		if (b == 28 || b == 29) 
			b--;
		var c: Int = (year + Math.floor(year / 4) + b - 13) % 7;
		var day : Int = 28 + b - c - 2;
		var month : Int = 3;
		if (day > 31) {
			day -= 31;
			month = 4;
		}
		return encode(year, month, day);
	}
	
	@:from public static inline function fromInt(v: Int): DateTime { return new DateTime(v); }
	
	@:from public static function fromDate(d: Date): DateTime {
		//var res: TDateTime = TDateTime.EncodeDateTime(1970, 1, 1, 1, 0, 0);
		//res.toFloat += d.getTime() / (1000 * 60 * 60 * 24); 
		return d != null ? DateTime.encodeDateTime(d.getFullYear(), d.getMonth() +1, d.getDate(), 
      d.getHours(), d.getMinutes(), d.getSeconds()): 0.0;
	}
	
	//@:from public static function fromString(v: String): DateTime {	
	//}
	// todo
	
	#if (false)
	// fromDynamic has bad side effects - better not to use it
	@:from 
	#end
	
	public static function fromDynamic(d: Dynamic): DateTime {
		switch Type.typeof(d) {
			case TNull : return 0;
			case TInt : return fromInt(d);
			case TFloat : return new DateTime(d);
			case TObject : if ( Std.isOfType(d, Date) ) return fromDate(d);
			default:
		}
		return 0;
	}
	
	public static function fromTime(d: Date): DateTime {
		return d != null ? DateTime.encodeTime(d.getHours(), d.getMinutes(), d.getSeconds()) : 0.0;
	}

	public static function fromDay(d: Date): DateTime {
		return d != null ? DateTime.encode(d.getFullYear(), d.getMonth() + 1, d.getDate()) : 0.0;
	}
	
	/**
		Returns the actual day (without time).
	**/
	public static function date(): DateTime {
		return fromDate(Date.now()).toInt();
	}

	/**
		Returns the actual day and time.
	**/
	public static function now(): DateTime {
		return fromDate(Date.now());
	}
	
	/**
	 * creates DateTime from Unix timestamp (seconds since 1970-01-01)
	 * @param	tsec timestamp (seconds since 1970-01-01)
	 * @return DateTime
	 */
	public static inline function fromUnixTimestamp(tsec: Float): DateTime {
		return new DateTime(UNIX_START + tsec * SECONDS);
	}
	
	/**
	 * converts DateTime to Unix timestamp
	 * @return seconds since 1970-01-01
	 */
 
	public function toUnixTimestamp(): Float {
		return (this - UNIX_START) * SECONDS_PER_DAY;
	}
	
	/*
	 * returns the current delta between utc and local time
	 */
	static public function getTimeOffset(): Float {
		var n = Date.now();
		return n.getTimezoneOffset() * MINUTES;
		//return fromUnixTimestamp(n.getTime() / 1000) - fromDate(n);
	}
	
	/*
	 * return utc time when this time is local, otherwhise junk !
	 */
	public inline function utc(): DateTime return this + getTimeOffset();
	
	/*
	 * return local time when this time is utc, otherwhise junk !
	 */
	public inline function localTime(): DateTime return this - getTimeOffset();
	
}

private enum DTFPart {
	Lit(s: String);
	DatePart(call: (d: DateRec) -> String);
	TimePart(call: (t: TimeRec) -> String);
	DateTimePart(call: (d: DateTime) -> String);
	DateFormat;
	Error(s: String);
}

private class DateTimeFormatter {

	static inline function pad(s: String) {
		final full = "00" + s;
		return full.substring(full.length - 2);
	}

	static function _a(d: DateTime) return inline d.dayOfWeek().toString();
	static function _d(d: DateRec) return pad(Std.string(d.day));
	static function _m(d: DateRec) return pad(Std.string(d.month));
	static function _y(d: DateRec) return pad(Std.string(d.year % 100));
	static function _Y(d: DateRec) return Std.string(d.year);
	static function _dmin(d: DateRec) return Std.string(d.day);
	static function _mmin(d: DateRec) return Std.string(d.month);
	static function _ymin(d: DateRec) return Std.string(d.year % 100);

	static function _H(t: TimeRec) return pad(Std.string(t.hour));
	static function _I(t: TimeRec) {final h = Math.floor(t.hour % 12); return pad(Std.string(h > 0 ? h : 12));};
	static function _M(t: TimeRec) return pad(Std.string(t.minute));
	static function _S(t: TimeRec) return pad(Std.string(Math.floor(t.sec)));
	static function _Hmin(t: TimeRec) return Std.string(t.hour);
	static function _Imin(t: TimeRec) return Std.string(t.hour % 12 + 1);
	static function _Mmin(t: TimeRec) return Std.string(t.minute);
	static function _Smin(t: TimeRec) return Std.string(Math.floor(t.sec));
	static function _p(t: TimeRec) return t.hour < 12 ? "AM" : "PM";
	//static function _f(t: TimeRec) return MathX.frac(t.sec)  < 12 ? "AM" : "PM";
	static function _u(d: DateTime) return Std.string(d.dayOfWeek() + 1);
	static function _w(d: DateTime) return Std.string((d.dayOfWeek() + 7 - Sun) % 7);
	static function _U(d: DateTime) return pad(Std.string(d.ISOWeekNumber(Sun, 4).week));
	static function _W(d: DateTime) return pad(Std.string(d.ISOWeekNumber(Mon, 4).week));

	static function analyze(f: String, result: Array<DTFPart>)
	{
		inline function error(s: String) {
			result.resize(0);
			result.push(Error(s));
		}

		var p = 0;	
		while (p < f.length) {
			var n = f.indexOf('%', p);
			if (n >= 0) {
				if (n > p) result.push(Lit(f.substring(p, n)));
				n++;
				if (n < f.length) {
					switch StringTools.fastCodeAt(f, n) {
						case 'a'.code : result.push(DateTimePart(_a));
						case 'd'.code : result.push(DatePart(_d));
						case 'm'.code : result.push(DatePart(_m));
						case 'y'.code : result.push(DatePart(_y));
						case 'H'.code : result.push(TimePart(_H));
						case 'I'.code : result.push(TimePart(_I));
						case 'M'.code : result.push(TimePart(_M));
						case 'S'.code : result.push(TimePart(_S));
						case 'Y'.code : result.push(DatePart(_Y));
						case 'p'.code : result.push(TimePart(_p));
						case '%'.code : result.push(Lit('%'));
						case 't'.code : result.push(Lit('\t'));
						case 'n'.code : result.push(Lit('\n'));
						case 'w'.code : result.push(DateTimePart(_w));
						case 'W'.code : result.push(DateTimePart(_W));

						case '-'.code : {
							n++;
							if (n < f.length) {
								switch StringTools.fastCodeAt(f, n) {
									case 'd'.code : result.push(DatePart(_dmin));
									case 'm'.code : result.push(DatePart(_mmin));
									case 'y'.code : result.push(DatePart(_ymin));
									case 'H'.code : result.push(TimePart(_Hmin));
									case 'I'.code : result.push(TimePart(_Imin));
									case 'M'.code : result.push(TimePart(_Mmin));
									case 'S'.code : result.push(TimePart(_Smin));
									case var e: 
										return error('unsupported code %$e in pattern $f');
								}
							} else {
								return error('unexpected end of string at code %- in pattern $f');
							}
							p = n + 1;
							continue;
						}
						case _ :
							result.resize(0);
							result.push(DateFormat);
							return;
					}
				}
				p = n + 1;
			} else {
				result.push(Lit(f.substring(p)));
				break;
			}
		}
	}

	static var _cache: Map<String, DateTimeFormatter> = [];

	public static function format(d: DateTime, f: String) {
		var formatter = _cache.get(f);
		if (formatter == null) {
			formatter = new DateTimeFormatter(f);
			_cache.set(f, formatter);
		}
		return formatter.doFormat(d);
	}

	var f: String;
	final action: Array<DTFPart> = [];
	function new(f: String) {
		this.f = f;
		analyze(f, action);
	}

	function doFormat(d: DateTime): String {
		var result = "";
		var dr : DateRec = null;
		var tr : TimeRec = null;
		for (a in action) {
			switch a {
				case Lit(s): result += s;
				case DatePart(call):
					if (dr == null) dr = d.decode(); 
					result += call(dr);
				case TimePart(call):
					if (tr == null) tr = d.decodeTime();
					result += call(tr);
				case DateTimePart(call): result += call(d);
				case DateFormat: result += DateTools.format(d, f);
				case Error(s): throw s;
			}
		}
		return result;
	}
}

