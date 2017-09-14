#lang agile

(require racket/format
         "abbrev-string.rkt"
         (submod "abbrev-string.rkt" example))

;; ------------------------------------------------------------------------

(provide time-period time-period?
         time-period-start
         time-period-duration
         time-period-end
         time-period/start+end
         time-of-week time-of-week?
         time-of-week-day
         time-of-week-hour
         time-of-week-minute
         day-of-week=?
         SUNDAY MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY
         time-of-week+
         earliest-of-day latest-of-day
         time-of-day-hour-fraction
         duration duration?
         duration-days duration-hours duration-minutes
         day-of-week->abbrev-string
         time-period->string/hm
         time-of-day->string)

;; A TimePeriod is a (time-period TimeofWeek Duration)
;; A TimeofWeek is a (time-of-week DayofWeek HourofDay Minute)
;; A Duration is a (duration Nat Nat Minute)
;; A DayofWeek is an Int[0,7)
;; A HourofDay is an Int[0,24)
;; A Minute is an Int[0,60)
(struct time-period [start duration] #:transparent)
(struct time-of-week [day hour minute] #:transparent)
(struct duration [days hours minutes] #:transparent)
(define SUNDAY 0)
(define MONDAY 1)
(define TUESDAY 2)
(define WEDNESDAY 3)
(define THURSDAY 4)
(define FRIDAY 5)
(define SATURDAY 6)

;; time-period-end : TimePeriod -> TimeofWeek
(define (time-period-end tp)
  (time-of-week+ (time-period-start tp) (time-period-duration tp)))

;; time-period/start+end : TimeofWeek TimeofWeek -> TimePeriod
(define (time-period/start+end start end)
  (time-period start (time-of-week∆ start end)))

;; earliest-of-day : [Listof TimeofWeek] -> TimeofWeek
(define (earliest-of-day tows)
  (argmin time-of-day-hour-fraction tows))

;; latest-of-day : [Listof TimeofWeek] -> TimeofWeek
(define (latest-of-day tows)
  (argmax time-of-day-hour-fraction tows))

;; time-of-day-hour-fraction : TimeofWeek -> Rational
(define (time-of-day-hour-fraction tow)
  (match tow
    [(time-of-week d h m)
     (+ h (/ m 60))]))

;; time-of-week+ : TimeofWeek Duration -> TimeofWeek
(define (time-of-week+ tow dur)
  (match* [tow dur]
    [[(time-of-week d h m) (duration ∆d ∆h ∆m)]
     (define-values [eh m*] (quo/mod (+ m ∆m) 60))
     (define-values [ed h*] (quo/mod (+ h ∆h eh) 24))
     (define-values [_w d*] (quo/mod (+ d ∆d ed) 7))
     (time-of-week d* h* m*)]))

(define (time-of-week∆ a b)
  (match* [a b]
    [[(time-of-week ad ah am) (time-of-week bd bh bm)]
     (define-values [eh ∆m] (quo/mod (- bm am) 60))
     (define-values [ed ∆h] (quo/mod (+ (- bh ah) eh) 24))
     (define-values [_w ∆d] (quo/mod (+ (- bd ad) ed) 7))
     (duration ∆d ∆h ∆m)]))

;; day-of-week=? : DayofWeek DayofWeek -> Bool
(define (day-of-week=? a b)
  (= a b))

;; day-of-week->abbrev-string : DayofWeek -> AbbrevString
(define (day-of-week->abbrev-string dow)
  (list-ref DAYS-OF-WEEK-ABBREVS dow))

;; time-period->string/hm : TimePeriod -> String
(define (time-period->string/hm tp)
  (format "~a \u2013 ~a" ; \u2013 is an en dash
          (time-of-day->string (time-period-start tp))
          (time-of-day->string (time-period-end tp))))

;; time-of-day->string : TimeofWeek -> String
(define (time-of-day->string t)
  (match t
    [(time-of-week _ h m)
     (format "~a:~a"
             (add1 (modulo (sub1 h) 12))
             (~r m #:min-width 2 #:pad-string "0"))]))

;; ------------------------------------------------------------------------

(define (quo/mod a b)
  (define mod (modulo a b))
  (define quo (/ (- a mod) b))
  (values quo mod))

