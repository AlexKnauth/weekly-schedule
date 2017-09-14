#lang agile

(provide template name place times
         #%datum
         (rename-out [-module-begin #%module-begin]))

(require racket/stxparam
         weekly-schedule/image
         (for-syntax megaparsack
                     megaparsack/text
                     racket/list
                     (only-in srfi/1 append-reverse)
                     racket/match
                     racket/string
                     syntax/id-table
                     syntax/stx
                     syntax/parse/class/local-value
                     (only-in data/applicative pure)
                     (only-in data/monad do <-)))

(begin-for-syntax
  (define (stx-strings->tokens strs acc)
    (cond [(stx-null? strs) (reverse acc)]
          [else
           (stx-strings->tokens
            (stx-cdr strs)
            (append-reverse
             (stx-string->tokens (stx-car strs))
             acc))]))
  (define (stx-string->tokens str)
    (parse-result!
     (parse-syntax-string (many/p (syntax-box/p (satisfy/p char?))) str)))
  (define (parse-syntax-strings parser strs)
    (parse parser (stx-strings->tokens strs '())))
  (define (list-append*/p ps)
    (do
      [lol <- (apply list/p ps)]
      (pure (append* lol))))
  (define (char-not-newline? c)
    (not (char=? c #\newline)))
  (define char-not-newline/p
    (satisfy/p char-not-newline?))
  (define string-no-newlines/p
    (do
      [cs <- (many/p char-not-newline/p)]
      (pure (list->string cs))))
  (define (quote-expr/p parser)
    (do
      [expr <- parser]
      (pure #`'#,expr)))
  (define string-no-newlines-qstx/p
    (quote-expr/p (syntax/p string-no-newlines/p)))
  (define day-of-week-letter/p
    (or/p
     (do
       [c <- (one-of/p (string->list "MTWRF"))]
       (pure (match c
               [#\M 1]
               [#\T 2]
               [#\W 3]
               [#\R 4]
               [#\F 5])))
     (do (try/p (string/p "Sun")) (pure 0))
     (do (try/p (string/p "Sat")) (pure 6))))
  (define day-of-week-letters/p
    (many/p day-of-week-letter/p))
  (define time-of-day/p
    (do
      [h <- integer/p]
      (string/p ":")
      [m <- integer/p]
      [am/pm <- (or/p (do (string/p "am") (pure 0))
                      (do (string/p "pm") (pure 12)))]
      (pure #`(list #,(+ (modulo h 12) am/pm) #,m))))
  (define-syntax-class ws-str
    [pattern s:str
      #:when (for/and ([c (in-string (syntax-e #'s))])
               (char-whitespace? c))]))

;; ------------------------------------------------------------------------

(begin-for-syntax
  (define (out-of-context-error stx)
    (raise-syntax-error #f "used out of context" stx))
  (struct template-placeholder-info [find-parser data-parser]
    #:transparent
    #:property prop:procedure
    (λ (this stx) (out-of-context-error stx)))
  (define-syntax-class template-placeholder-id
    [pattern id
      #:declare id (local-value template-placeholder-info?)
      #:attr info (attribute id.local-value)
      #:attr find-parser
      (template-placeholder-info-find-parser (attribute info))
      #:attr data-parser
      (template-placeholder-info-data-parser (attribute info))]))

(define-syntax name
  (template-placeholder-info
   string-no-newlines/p
   string-no-newlines-qstx/p))

(define-syntax place
  (template-placeholder-info
   string-no-newlines/p
   string-no-newlines-qstx/p))

(define-syntax times
  (template-placeholder-info
   string-no-newlines/p
   (do
     [days <- day-of-week-letters/p]
     (string/p " ")
     [start <- time-of-day/p]
     (string/p " - ")
     [end <- time-of-day/p]
     (pure #`(list
              #,@(for/list ([day (in-list days)])
                   #`(time-period/day+start+end #,day #,start #,end)))))))

(define-simple-macro (template stuff ...)
  (void))

;; ------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class schedule-template
    #:attributes [parser]
    #:literals [template]
    [pattern (template elem:template-elem ...)
      #:attr parser (list-append*/p (attribute elem.parser))])
  (define-syntax-class template-elem
    #:attributes [parser]
    [pattern s:str
      #:attr parser (do (string/p (syntax-e #'s)) (pure '()))]
    [pattern x:template-placeholder-id
      #:attr parser
      (do
        [str <- (syntax/p (attribute x.find-parser))]
        (pure (list (list #'x.id str))))])
  (define-syntax-class weekly-schedule-data
    #:attributes [template data]
    [pattern (:ws-str ... template:schedule-template rst:str ...)
      #:do [(define parser
              (do
                (many/p space/p)
                [result <- (many/p
                            (do
                              [evt <- (attribute template.parser)]
                              (many/p space/p)
                              (pure evt)))]
                eof/p
                (pure result)))
            (define result
              (parse-result! (parse-syntax-strings parser #'[rst ...])))]
      #:with data #`(weekly-schedule #,@result)]))

(define-simple-macro (-module-begin . stuff:weekly-schedule-data)
  (#%module-begin (weekly-schedule->image stuff.data)))

;; ------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class event
    #:attributes [data]
    [pattern ([x:template-placeholder-id s:str] ...)
      #:fail-when (check-duplicate-identifier (stx->list #'(x ...)))
      "duplicate identifier"
      #:do [(define hsh
              (for/fold ([table (make-immutable-free-id-table)])
                        ([key (in-list (stx->list #'[x.id ...]))]
                         [parser (in-list (attribute x.data-parser))]
                         [s (in-list (stx->list #'[s ...]))])
                (free-id-table-set
                 table
                 key
                 (parse-result! (parse-syntax-string parser s)))))]
      #:with name (free-id-table-ref hsh #'name)
      #:with place (free-id-table-ref hsh #'place)
      #:with times (free-id-table-ref hsh #'times)
      #:with data #'(events/name+place+times name place times)]))

(define-simple-macro (weekly-schedule e:event ...)
  (append e.data ...))

;; ------------------------------------------------------------------------

(require weekly-schedule/data
         weekly-schedule/data/time)

;; events/name+place+times :
;; String String [Listof TimePeriod] -> [Listof Event]
(define (events/name+place+times name place times)
  (for/list ([time (in-list times)])
    (event (list name) (list place) time)))

;; time-period/day+start+end :
;; DayofWeek [List Hour Minute] [List Hour Minute] -> TimePeriod
(define (time-period/day+start+end day start end)
  (match-define (list sh sm) start)
  (match-define (list eh em) end)
  (time-period/start+end (time-of-week day sh sm)
                         (time-of-week day eh em)))

;; ------------------------------------------------------------------------

