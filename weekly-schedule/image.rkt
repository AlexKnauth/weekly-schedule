#lang agile

(provide weekly-schedule->image)

(require 2htdp/image
         lang/posn
         "data.rkt"
         "data/time.rkt")

(define (posn+ . ps)
  (make-posn (apply + (map posn-x ps))
             (apply + (map posn-y ps))))

;; ------------------------------------------------------------------------

;; A WeeklySchedule is a [Listof Event]
;; as defined in "data.rkt"

;; A Event is a (event Name Place TimePeriod)
;; as defined in "data.rkt"

;; ------------------------------------------------------------------------

(define BACKGROUND-COLOR "white")

(define 12HOUR-GRIDLINE-PEN
  (pen "dimgray" 3 "solid" "butt" "round"))
(define 3HOUR-GRIDLINE-PEN
  (pen "dimgray" 1 "solid" "butt" "round"))
(define HOUR-GRIDLINE-PEN
  (pen "gray" 1 "solid" "butt" "round"))
(define QUARTER-HOUR-GRIDLINE-PEN
  (pen "whitesmoke" 1 "solid" "butt" "round"))

(define EVENT-TEXT-COLOR "black")
(define EVENT-OUTLINE-COLOR "black")
(define EVENT-BACKGROUND-COLOR "floralwhite")

(define DAY-LABEL-TEXT-COLOR "black") 
(define HOUR-LABEL-TEXT-COLOR "black") 

;; ------------------------------------------------------------------------

(define WIDTH 800)
(define HEIGHT 1050)

(define LABEL-HEIGHT 30)
(define LABEL-WIDTH 30)

(define GRID-POSN (make-posn LABEL-WIDTH LABEL-HEIGHT))
(define GRID-WIDTH (- WIDTH LABEL-WIDTH))
(define GRID-HEIGHT (- HEIGHT LABEL-HEIGHT 8))

(define GRID-Xi (posn-x GRID-POSN))
(define GRID-Xf (+ GRID-Xi GRID-WIDTH))

(define DAY-LABEL-OFFSET
  (make-posn (* GRID-WIDTH 1/7 1/2) (- (* LABEL-HEIGHT 1/2))))
(define HOUR-LABEL-OFFSET
  (make-posn (- (* LABEL-WIDTH 1/2)) 0))

(define DAY-LABEL-TEXT-SIZE 20)
(define HOUR-LABEL-TEXT-SIZE 12)

(define EVENT-NAME-MAX-TEXT-SIZE 14)
(define EVENT-NAME-MIN-TEXT-SIZE 4)
(define EVENT-PLACE-MAX-TEXT-SIZE 12)
(define EVENT-PLACE-MIN-TEXT-SIZE 3)
(define EVENT-TIME-MAX-TEXT-SIZE 12)
(define EVENT-TIME-MIN-TEXT-SIZE 3)

(define BACKGROUND (rectangle WIDTH HEIGHT "solid" BACKGROUND-COLOR))

(define (time->posn t early late)
  (define ∆h (- late early))
  (posn+
   GRID-POSN
   (make-posn
    (* GRID-WIDTH 1/7 (time-of-week-day t))
    (* GRID-HEIGHT (/ 1 ∆h) (- (time-of-day-hour-fraction t) early)))))

(define (time->posn-y t early late)
  (posn-y (time->posn t early late)))

;; ------------------------------------------------------------------------

;; weekly-schedule->image : WeeklySchedule -> Image
(define (weekly-schedule->image s)
  (define early
    (earliest-of-day (map time-period-start (map event-time-period s))))
  (define early-hour (sub1 (time-of-week-hour early)))
  (define late-hour 24)
  (place-images/align
   (for/list ([e (in-list s)])
     (event->image e early-hour late-hour))
   (for/list ([e (in-list s)])
     (time->posn (time-period-start (event-time-period e))
                 early-hour late-hour))
   "left" "top"
   (grid-image early-hour late-hour)))

;; event->image : Event Hour Hour -> Image
(define (event->image e early late)
  (define height
    (- (time->posn-y (time-period-end (event-time-period e)) early late)
       (time->posn-y (time-period-start (event-time-period e)) early late)))
  (unless (positive? height)
    (error 'event->image "bad event: ~v" e))
  (define width (* 1/7 GRID-WIDTH))
  (define spc
    (rectangle 0 1 "solid" "transparent"))
  (define name
    (text/fit (last (event-name e))
              width
              (* 1/2 (- height (image-height spc)))
              EVENT-NAME-MAX-TEXT-SIZE
              EVENT-NAME-MIN-TEXT-SIZE
              EVENT-TEXT-COLOR))
  (define place
    (text/fit (last (event-place e))
              width
              (* 1/2 (- height (image-height spc) (image-height name)))
              EVENT-PLACE-MAX-TEXT-SIZE
              EVENT-PLACE-MIN-TEXT-SIZE
              EVENT-TEXT-COLOR))
  (define time
    (text/fit (time-period->string/hm (event-time-period e))
              width
              (- height (image-height spc) (image-height name)
                 (image-height place))
              EVENT-TIME-MAX-TEXT-SIZE
              EVENT-TIME-MIN-TEXT-SIZE
              EVENT-TEXT-COLOR))
  (overlay/align
   "center" "top"
   (above spc
          name
          place
          time)
   (rectangle width height "outline" EVENT-OUTLINE-COLOR)
   (rectangle width height "solid" EVENT-BACKGROUND-COLOR)))

;; ------------------------------------------------------------------------

;; grid-image : Hour Hour -> Image
(define (grid-image early late)
  (define hour-span (- late early))
  (define hour-gridbox
    (rectangle (* GRID-WIDTH 1/7)
               (* GRID-HEIGHT (/ 1 hour-span))
               "outline"
               HOUR-GRIDLINE-PEN))
  (define quarter-hour-gridbox
    (rectangle (* GRID-WIDTH 1/7)
               (* GRID-HEIGHT (/ 1 hour-span) 1/4)
               "outline"
               QUARTER-HOUR-GRIDLINE-PEN))
  (place-images/align
   (for/list ([d (in-range 0 7)])
     (text (second (day-of-week->abbrev-string d))
           DAY-LABEL-TEXT-SIZE
           DAY-LABEL-TEXT-COLOR))
   (for/list ([d (in-range 0 7)])
     (posn+ DAY-LABEL-OFFSET
            (time->posn (time-of-week d early 0) early late)))
   "center" "center"
   (place-images/align
    (for/list ([h (in-range early (add1 late))])
      (text (number->string (add1 (modulo (sub1 h) 12)))
            HOUR-LABEL-TEXT-SIZE
            HOUR-LABEL-TEXT-COLOR))
    (for/list ([h (in-range early (add1 late))])
      (posn+ HOUR-LABEL-OFFSET
             (time->posn (time-of-week 0 h 0) early late)))
    "center" "center"
    (place-lines
     (for/list ([h (in-range early (add1 late))]
                #:when (zero? (modulo h 3)))
       (cond
         [(zero? (modulo h 12))
          (line-info
           (make-posn GRID-Xi (time->posn-y (time-of-week 0 h 0) early late))
           (make-posn GRID-Xf (time->posn-y (time-of-week 0 h 0) early late))
           12HOUR-GRIDLINE-PEN)]
         [else
          (line-info
           (make-posn GRID-Xi (time->posn-y (time-of-week 0 h 0) early late))
           (make-posn GRID-Xf (time->posn-y (time-of-week 0 h 0) early late))
           3HOUR-GRIDLINE-PEN)]))
     (place-images/align
      (for*/list ([d (in-range 0 7)] [h (in-range early late)])
        hour-gridbox)
      (for*/list ([d (in-range 0 7)] [h (in-range early late)])
        (time->posn (time-of-week d h 0) early late))
      "left" "top"
      (place-images/align
       (for*/list ([d (in-range 0 7)]
                   [h (in-range early late)]
                   [m (in-range 0 60 15)])
         quarter-hour-gridbox)
       (for*/list ([d (in-range 0 7)]
                   [h (in-range early late)]
                   [m (in-range 0 60 15)])
         (time->posn (time-of-week d h m) early late))
       "left" "top"
       BACKGROUND))))))

;; ------------------------------------------------------------------------

;; A LineInfo is a (line-info Posn Posn (U Pen ImageColor))
(struct line-info [start end pen-or-color] #:transparent)

;; place-line : LineInfo Image -> Image
(define (place-line l scene)
  (match l
    [(line-info p1 p2 pen-or-color)
     (scene+line scene
                 (posn-x p1) (posn-y p1)
                 (posn-x p2) (posn-y p2)
                 pen-or-color)]))

;; place-lines : [Listof LineInfo] Image -> Image
(define (place-lines ls scene)
  (foldl place-line scene ls))

;; ------------------------------------------------------------------------

(define (text/fit str w h max-size min-size color)
  (or
   (for/or ([size (in-range max-size min-size -1)])
     (define img (text str size color))
     (and
      (< (image-height img) h)
      (< (image-width img) w)
      img))
   (text str min-size color)))

;; ------------------------------------------------------------------------

