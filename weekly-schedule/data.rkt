#lang agile

(require "data/time.rkt"
         "data/abbrev-string.rkt")

(module+ example
  (provide (all-defined-out)))

;; ------------------------------------------------------------------------

(provide event event?
         event-name
         event-place
         event-time-period)

;; A WeeklySchedule is a [Listof Event]

;; A Event is a (event Name Place TimePeriod)
(struct event [name place time-period] #:transparent)

;; A Name is an AbbrevString
;; A Place is an AbbrevString

(define (events/times name place time-periods)
  (for/list ([tp (in-list time-periods)])
    (event name place tp)))

(define (time-periods/starts starts dur)
  (for/list ([s (in-list starts)])
    (time-period s dur)))

(define (times-of-week/days days hour minute)
  (for/list ([d (in-list days)])
    (time-of-week d hour minute)))

(module+ example
  (define WEEKDAYS (list MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY))

  (define CHAMBER-REHEARSAL
    (events/times
     (abvstr "Chamber Orchestra Rehearsal" "Chamber")
     (abvstr "Cur 013")
     (time-periods/starts (times-of-week/days WEEKDAYS 7 30)
                          (duration 0 0 35))))

  (define SYMPHONY-REHEARSAL
    (list
     (event
      (abvstr "Symphony Orchestra Rehearsal" "Sym Orch")
      (abvstr "Fen 101")
      (time-period (time-of-week TUESDAY (+ 12 6) 0)
                   (duration 0 2 30)))))

  (define CONCERT-ORCH-REHEARSAL
    (events/times
     (abvstr "Concert Orchestra Rehearsal" "Con Orch")
     (abvstr "Aud 101")
     (time-periods/starts (times-of-week/days WEEKDAYS (+ 12 1) 40)
                          (duration 0 0 52))))

  (define QUARTET-STUFF
    (list
     (event
      (abvstr "Quartet Coaching Session" "Quartet")
      (abvstr "Ry 345")
      (time-period (time-of-week THURSDAY (+ 12 6) 0)
                   (duration 0 1 20)))
     (event
      (abvstr "Quartet Rehearsal" "Quartet")
      (abvstr "WC 056")
      (time-period (time-of-week FRIDAY (+ 12 5) 0)
                   (duration 0 1 45)))))

  (define CHOIR-STUFF
    (list
     (event
      (abvstr "Choir Rehearsal" "Choir")
      (abvstr "Fen 101")
      (time-period (time-of-week WEDNESDAY (+ 12 6) 0)
                   (duration 0 2 30)))
     (event
      (abvstr "Chamber Choir Rehearsal" "Choir")
      (abvstr "Fen 101")
      (time-period (time-of-week WEDNESDAY (+ 12 8) 30)
                   (duration 0 1 0)))))

  (define CHURCH-STUFF
    (list
     (event
      (abvstr "Church")
      (abvstr "Church")
      (time-period (time-of-week SUNDAY 10 30)
                   (duration 0 1 30)))
     (event
      (abvstr "Church Choir")
      (abvstr "Church")
      (time-period (time-of-week THURSDAY (+ 12 7) 30)
                   (duration 0 1 30)))))

  (define WEEKLY-SCHEDULE-EXAMPLE-1
    (append CHAMBER-REHEARSAL
            SYMPHONY-REHEARSAL
            CONCERT-ORCH-REHEARSAL
            QUARTET-STUFF
            CHOIR-STUFF
            CHURCH-STUFF)))

