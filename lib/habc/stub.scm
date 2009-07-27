(define +)
(define +.)
(define -)
(define -.)
(define *)
(define *.)
(define /)

(define <)
(define <=)
(define >)
(define >=)
(define =)

(define trace)
(define Object)

(class Date (Object) (date dateUTC day dayUTC fulYear fullYearUTC
			   ours hoursUTC milliseconds millisecondsUTC minutes minutesUTC month
			   mounthUTC secnods secondsUTC time timezoneOffset)
       (method getDate ())
       (method getDay ())
       (method getHours ())
       (method getMilliseconds ())
       (method getMinutes ())
       (method getMonth ())
       (method getSecnods ())
       (method getTime ())
       (method getTimezoneOffset ())
       (method getUTCDate ())
       (method getUTCDay ())
       (method getUTCFulYear ())
       (method getUTCHours ())
       (method getUTCMilliseconds ())
       (method getUTCMinutes ())
       (method getUTCMonth ())
       (method getUTCSeconds ())
       (static parse (date))
       (method setDate (day))
       (method setFullYear (year month day))
       (method setHours (hour minute second millisecond))
       (method setMilliseconds (millisecond))
       (method setMinutes (minute second millisecond))
       (method setMonth (month day))
       (method setSeconds (second millisecond))
       (method setTime (millisecond))
       (method setUTCDate (day))
       (method setUTCFullYear(year day))
       (method setUTCHours(hour minute second millisecond))
       (method setUTCMilliseconds(millisecond))
       (method setUTCMinutes(minute second millisecond))
       (method setUTCMonth(month day))
       (method setUTCSeconds(second millisecond))
       (method toDateString())
       (method toLocaleDateString())
       (method toLocaleString())
       (method toLocaleTimeString())
       (method toString())
       (method toTimeString())
       (method toUTCString())
       (method UTC())
       (method valueOf()))

(class Math (Object) ()
       (static abs ())
       (static acos ())
       (static asin ())
       (static atan ())
       (static atan2 ())
       (static ceil ())
       (static cos ())
       (static exp ())
       (static floor ())
       (static log ())
       (static max ())
       (static min ())
       (static pow ())
       (static random ())
       (static round ())
       (static sin ())
       (static tan ()))
