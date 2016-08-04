#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/datum-map.rkt"
         "../host/correlate.rkt"
         (only-in "../host/syntax-to-reader-syntax.rkt" srcloc->vector))

;; The `correlate*` function takes the source location and relevant
;; properties of an expander syntax object and applies it to a
;; host-system syntax object (i.e., a "correlated")

(provide correlate*)

(define (correlate* stx s-exp)
  (define e
    (cond
     [(and (datum-has-elements? s-exp)
           (syntax-srcloc stx))
      ;; Avoid pushing source locations to nested objects
      (datum->correlated (correlated-e (datum->correlated s-exp))
                         (srcloc->vector (syntax-srcloc stx)))]
     [else
      (datum->correlated s-exp (srcloc->vector (syntax-srcloc stx)))]))
  (define maybe-n (syntax-property stx 'inferred-name))
  (if maybe-n
      (correlated-property e 'inferred-name (if (syntax? maybe-n) 
                                                (syntax->datum maybe-n)
                                                maybe-n))
      e))
