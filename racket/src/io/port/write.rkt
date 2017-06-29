#lang racket/base
(require "../common/internal-error.rkt"
         "../host/evt.rkt"
         "port.rkt"
         "output-port.rkt"
         "count.rkt")

(provide write-some-bytes)

(define (write-some-bytes who out bstr start end
                          #:copy-bstr? [copy-bstr? #t]
                          #:buffer-ok? [buffer-ok? #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f])
  (define write-out (core-output-port-write-out out))
  (let try-again ()
    (start-atomic)
    (cond
      [(core-port-closed? out)
       (end-atomic)
       (raise-arguments-error who
                              "output port is closed"
                              "output port" out)]
      [(= start end)
       (end-atomic)
       0]
      [else
       (define v (write-out bstr start end (not buffer-ok?) enable-break? copy-bstr?))
       (let result-loop ([v v])
         (cond
           [(not v)
            (end-atomic)
            (if zero-ok?
                0
                (try-again))]
           [(evt? v)
            (end-atomic)
            (cond
              [zero-ok? 0]
              [else
               (define new-v (if enable-break?
                                 (sync/enable-break v)
                                 (sync v)))
               (start-atomic)
               (result-loop new-v)])]
           [(exact-positive-integer? v)
            (port-count! out v bstr start)
            (end-atomic)
            v]
           [else
            (end-atomic)
            (internal-error (format "write-some-bytes: weird result ~s for ~s ~s ~s" v bstr start end))]))])))
