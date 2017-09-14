#lang agile

(provide (rename-out
          [-read read]
          [-read-syntax read-syntax]))

(require syntax/strip-context
         (only-in scribble/reader
           [read-syntax-inside at-read-syntax-inside]))

(define (-read in)
  (syntax->datum (-read-syntax #f in)))

(define (-read-syntax src in)
  (strip-context
   #`(module anonymous-module weekly-schedule/image/lang
       #,@(at-read-syntax-inside src in #:command-char #\◊))))

