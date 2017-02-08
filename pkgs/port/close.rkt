#lang racket/base
(require "input-port.rkt"
         "output-port.rkt"
         "check.rkt")

(provide close-input-port
         close-output-port)

(define (close-input-port p)
  (check 'close-input-port input-port? p)
  (unless (input-port-closed? p)
    (set-input-port-closed?! p #t)
    ((input-port-close p))))

(define (close-output-port p)
  (check 'close-output-port output-port? p)
  (unless (output-port-closed? p)
    (set-output-port-closed?! p #t)
    ((output-port-close p))))
