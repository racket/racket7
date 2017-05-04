#lang racket/base
(provide make-output-port)

(define (make-output-port name
                          evt
                          write-out
                          close
                          [write-out-special #f]
                          [get-write-evt #f]
                          [get-write-special-evt #f]
                          [get-location #f]
                          [count-lines! void]
                          [init-position 1]
                          [buffer-mode #f])
  (error 'make-output-port "not yet implemented"))
