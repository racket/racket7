#lang racket/base
(require compiler/cm
         compiler/zo-marshal)

(provide current-excluded-modules
         garbage-collect-toplevels-enabled
         demodularize)

(define current-excluded-modules (make-parameter null))
(define garbage-collect-toplevels-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize file-to-batch [output-file #f])
  (error "not yet reimplemented"))
