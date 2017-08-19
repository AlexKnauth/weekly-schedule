#lang agile

(module+ example
  (provide (all-defined-out)))

;; ------------------------------------------------------------------------

(provide abvstr)

;; An AbbrevString is a [Listof String]

(define abvstr list)

(module+ example
  (define SUNDAY-ABBREVS (abvstr "Sunday" "Sun" "S"))
  (define MONDAY-ABBREVS (abvstr "Monday" "Mon" "M"))
  (define TUESDAY-ABBREVS (abvstr "Tuesday" "Tue" "T"))
  (define WEDNESDAY-ABBREVS (abvstr "Wednesday" "Wed" "W"))
  (define THURSDAY-ABBREVS (abvstr "Thursday" "Thu" "R"))
  (define FRIDAY-ABBREVS (abvstr "Friday" "Fri" "F"))
  (define SATURDAY-ABBREVS (abvstr "Saturday" "Sat" "S"))

  (define DAYS-OF-WEEK-ABBREVS
    (list SUNDAY-ABBREVS
          MONDAY-ABBREVS
          TUESDAY-ABBREVS
          WEDNESDAY-ABBREVS
          THURSDAY-ABBREVS
          FRIDAY-ABBREVS
          SATURDAY-ABBREVS)))

;; ------------------------------------------------------------------------

