#lang racket/base
(require "../error/abort.rkt"
         "../host/evt.rkt"
         "port.rkt"
         "output-port.rkt")

(provide write-some-bytes)

(define (write-some-bytes who out bstr start end
                          #:copy-bstr? [copy-bstr? #t]
                          #:buffer-ok? [buffer-ok? #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f])
  (cond
    [(core-port-closed? out)
     (raise-arguments-error who
                            "output port is closed"
                            "output port" out)]
    [(= start end) 0]
    [else
     (define write-out (core-output-port-write-out out))
     (let try-again ()
       (define v (write-out bstr start end (not buffer-ok?) enable-break? copy-bstr?))
       (cond
         [(not v)
          (if zero-ok?
              0
              (try-again))]
         [(evt? v)
          (if zero-ok?
              0
              (begin
                (sync v)
                (try-again)))]
         [(and (exact-integer? v)
               (v . > . 0))
          v]
         [else (abort (format "write-some-bytes: weird result ~s" v))]))]))
