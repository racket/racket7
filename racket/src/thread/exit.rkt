#lang racket/base
(require (only-in racket/base
                  [exit host:exit])
         "../common/check.rkt")

(provide exit
         exit-handler)

(define/who exit-handler
  (make-parameter (lambda (v)
                    (cond
                     [(byte? v)
                      (host:exit v)]
                     [else
                      (host:exit 0)]))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 1) p)
                    p)))

(define (exit [v #t])
  ((exit-handler) v))
