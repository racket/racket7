#lang racket/base
(require (only-in racket/base
                  [exit host:exit])
         "../common/check.rkt"
         "plumber.rkt")

(provide exit
         exit-handler)

(define/who exit-handler
  (make-parameter (let ([root-plumber (current-plumber)])
                    (lambda (v)
                      (plumber-flush-all root-plumber)
                      (cond
                        [(byte? v)
                         (host:exit v)]
                        [else
                         (host:exit 0)])))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 1) p)
                    p)))

(define (exit [v #t])
  ((exit-handler) v))
