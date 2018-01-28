#lang racket/base
(require "state.rkt")

(provide propagate?
         get-propagate)

(define (propagate? v) (symbol? v))

(define (get-propagate id rhs env state)
  (cond
    [(and (symbol? rhs)
          (not (mutated? (hash-ref state rhs #f)))
          (not (mutated? (hash-ref state id #f))))
     (define r (hash-ref env rhs #f))
     (if (propagate? r)
         r
         rhs)]
    [else #t]))
