#lang racket/base
(require "../error/abort.rkt"
         "output-port.rkt")

(provide write-some-bytes)

(define (write-some-bytes who out bstr start end
                          #:buffer-ok? [buffer-ok? #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f])
  (cond
   [(output-port-closed? out)
    (raise-arguments-error who
                           "output port is closed"
                           "output port" out)]
   [(= start end)
    ;; Avoid an accidental flush request
    0]
   [else
    (define write-out (output-port-write-out out))
    (let try-again ()
      (define v (write-out bstr start end (not buffer-ok?) enable-break?))
      (cond
       [(or (not v)
            (eq? v 0)) ; zero is not supposed to happen, but treat it like #f
        (if zero-ok?
            0
            (try-again))]
       [(exact-integer? v) v]
       [else (abort "write-some-bytes: weird result")]))]))
