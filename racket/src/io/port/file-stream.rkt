#lang racket/base
(require "input-port.rkt"
         "output-port.rkt")

(provide prop:file-stream
         file-stream-port?)

(define-values (prop:file-stream file-stream? file-stream-ref)
  (make-struct-type-property 'file-stream))

(define (file-stream-port? p)
  (cond
    [(input-port? p)
     (file-stream? (input-port-data p))]
    [(output-port? p)
     (file-stream? (output-port-data p))]
    [else
     (raise-argument-error 'file-stream-port?
                           "port?"
                           p)]))

    
