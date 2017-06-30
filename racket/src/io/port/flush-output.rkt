#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "output-port.rkt")

(provide flush-output)

(define/who (flush-output p)
  (check who output-port? p)
  (let ([p (->core-output-port p)])
    (let loop ()
      (define r (atomically
                 ((core-output-port-write-out p) #"" 0 0 #f #f #f)))
      (cond
        [(eq? r 0) (void)]
        [(not r) (loop)]
        [else (error 'flush-output "weird result")]))))
