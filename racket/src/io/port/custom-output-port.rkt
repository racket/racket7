#lang racket/base
(require "../common/check.rkt"
         "../host/evt.rkt"
         "output-port.rkt")

(provide make-output-port)

(define/who (make-output-port name
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
  (check who evt? evt)

  (define (do-write-out bstr start end must-write? enable-break? copy?)
    (if copy?
        (write-out (subbytes bstr start end) 0 (- end start) must-write? enable-break?)
        (write-out bstr start end must-write? enable-break?)))

  (make-core-output-port
   #:name name
   #:evt evt
   #:write-out do-write-out
   #:close (lambda ()
             (end-atomic)
             (close)
             (start-atomic))
   #:write-out-special write-out-special
   #:get-write-evt get-write-evt
   #:get-write-special-evt get-write-special-evt
   #:get-location get-location
   #:count-lines! count-lines!))
