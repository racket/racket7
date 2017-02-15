#lang racket/base
(provide make-input-port)

(define (make-input-port name
                         read-in
                         peek
                         close
                         [get-progress-evt #f]
                         [commit #f]
                         [get-location #f]
                         [count-lines! void]
                         [init-position 1]
                         [buffer-mode #f])
  (error 'make-input-port "not yet implemented"))
