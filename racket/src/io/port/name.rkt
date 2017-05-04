#lang racket/base
(require "input-port.rkt"
         "output-port.rkt")

(provide port-name)

(define (port-name p)
  (cond
   [(input-port? p) (input-port-name p)]
   [(output-port? p) (output-port-name p)]
   [else (raise-argument-error 'port-name "port?" p)]))
