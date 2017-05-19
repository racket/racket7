#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide port-closed?
         close-input-port
         close-output-port)

(define (port-closed? p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (core-input-port-closed? p))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (core-output-port-closed? p))]
    [else
     (raise-argument-error 'close-input-port "port?" p)]))

(define (close-input-port p)
  (check 'close-input-port input-port? p)
  (let ([p (->core-input-port p)])
    (unless (core-input-port-closed? p)
      (set-core-input-port-closed?! p #t)
      ((core-input-port-close p)))))

(define (close-output-port p)
  (check 'close-output-port output-port? p)
  (let ([p (->core-output-port p)])
    (unless (core-output-port-closed? p)
      (set-core-output-port-closed?! p #t)
      ((core-output-port-close p)))))
