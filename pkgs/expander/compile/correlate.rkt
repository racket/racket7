#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/datum-map.rkt"
         "../host/correlate.rkt"
         (only-in "../host/syntax-to-reader-syntax.rkt" srcloc->vector))

;; The `correlate*` function takes the source location of an expander
;; syntax object and applies it to a host-system syntax object (i.e.,
;; a "correlated")

(provide correlate*
         ->correlated)

(define (correlate* stx s-exp)
  (if (syntax-srcloc stx)
      (datum->correlated s-exp (srcloc->vector (syntax-srcloc stx)))
      s-exp))

(define (->correlated s)
  (datum->correlated s #f))
