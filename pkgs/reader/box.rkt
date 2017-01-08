#lang racket/base
(require "error.rkt"
         "wrap.rkt"
         "parameter.rkt")

(provide read-box)

(define (read-box read-one dispatch-c in config)
  (unless (check-parameter read-accept-box config)
    (reader-error in config
                  "`~a&` forms not enabled"
                  dispatch-c))
  (define e (read-one in config))
  (when (eof-object? e)
    (reader-error in config #:eof? #t
                  "expected an element for `~a&` box, found end-of-file"
                  dispatch-c))
  (wrap (box e) in config #f))
