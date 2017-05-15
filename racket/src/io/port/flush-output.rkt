#lang racket/base
(require "../common/check.rkt"
         "output-port.rkt")

(provide flush-output)

(define (flush-output p)
  (check 'flush-output output-port? p)
  (let ([p (->core-output-port p)])
    (let loop ()
      (define r ((core-output-port-write-out p) #"" 0 0 #f #f))
      (cond
        [(eq? r 0) (void)]
        [(not r) (loop)]
        [else (error 'flush-output "weird result")]))))
