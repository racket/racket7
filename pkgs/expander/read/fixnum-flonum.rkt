#lang racket/base
(require "config.rkt"
         "error.rkt"
         "whitespace.rkt"
         "special.rkt"
         "symbol-or-number.rkt")

(provide read-fixnum
         read-flonum)

(define (read-fixnum read-one init-c in config)
  (unless init-c
    (skip-whitespace-and-comments! read-one in config))
  (define-values (line col pos) (port-next-location in))
  (define v (read-number-literal init-c in config "#e"))
  (cond
   [(fixnum? v) v]
   [(eof-object? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a fixnum, found ~a"
                  v)]))

(define (read-flonum read-one init-c in config)
  (unless init-c
    (skip-whitespace-and-comments! read-one in config))
  (define-values (line col pos) (port-next-location in))
  (define v (read-number-literal init-c in config "#i"))
  (cond
   [(flonum? v) v]
   [(eof-object? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a flonum, found ~a"
                  v)]))

;; ----------------------------------------

(define (read-number-literal init-c in config mode)
  (define c (or init-c
                (read-char/special in config)))
  (cond
   [(not (char? c)) c]
   [else
    (read-symbol-or-number c in config #:mode mode)]))
