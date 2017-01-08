#lang racket/base
(require "config.rkt"
         "error.rkt"
         "whitespace.rkt"
         "symbol.rkt")

(provide read-fixnum
         read-flonum)

(define (read-fixnum in config)
  (skip-whitespace-and-comments! in config)
  (define-values (line col pos) (port-next-location in))
  (define v (read-number-literal in config "#e"))
  (cond
   [(fixnum? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a fixnum, found ~a"
                  v)]))

(define (read-flonum in config)
  (skip-whitespace-and-comments! in config)
  (define-values (line col pos) (port-next-location in))
  (define v (read-number-literal in config "#i"))
  (cond
   [(flonum? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a flonum, found ~a"
                  v)]))

;; ----------------------------------------

(define (read-number-literal in config mode)
  (read-number-or-symbol #f in config #:mode mode))
