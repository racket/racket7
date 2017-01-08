#lang racket/base
(require "error.rkt"
         "wrap.rkt")

(provide read-quote)

(define (read-quote read-one sym desc c in config)
  (define wrapped-sym (wrap sym in config c))
  (define e (read-one in config))
  (when (eof-object? e)
    (reader-error in config #:eof? #t
                  "expected an element for ~a (found end-of-file)"))
  (wrap (list wrapped-sym e) in config #f))
