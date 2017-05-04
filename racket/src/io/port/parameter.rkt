#lang racket/base
(require (only-in racket/base
                  [current-input-port host:current-input-port]
                  [current-output-port host:current-output-port]
                  [current-error-port host:current-error-port])
         "output-port.rkt"
         "input-port.rkt"
         "host-port.rkt")

(provide current-input-port
         current-output-port
         current-error-port)

(define current-input-port
  (make-parameter (open-input-host (host:current-input-port) 'stdin)
                  (lambda (v)
                    (unless (input-port? v)
                      (raise-argument-error 'current-input-port
                                            "input-port?"
                                            v))
                    v)))
                  
(define current-output-port
  (make-parameter (open-output-host (host:current-output-port) 'stdout)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-output-port
                                            "output-port?"
                                            v))
                    v)))
                  
(define current-error-port
  (make-parameter (open-output-host (host:current-error-port) 'stderr)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-error-port
                                            "output-port?"
                                            v))
                    v)))
