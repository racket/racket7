#lang racket/base
(require "input-port.rkt"
         "internal-pipe.rkt"
         "count.rkt")

(provide read-some-bytes!
         peek-some-bytes!)

;; Read up to `(- end start)` bytes, producing at least a
;; single by unless `zero-ok?` is true. The result is
;; EOF or the number of bytes read.
(define (read-some-bytes! who orig-in bstr start end
                          ;; Zero is ok for `read-bytes!*`:
                          #:zero-ok? [zero-ok? #f]
                          ;; When calling an externally implemented
                          ;; port, we normally make a fresh byte
                          ;; string, because we don't trust the
                          ;; reading proc to not retain the byte
                          ;; string and change it later. We can skip
                          ;; the copy if the bstr is the right length
                          ;; and won't be exposed, though.
                          #:copy-bstr [copy-bstr? #t]
                          ;; If `keep-eof?`, don't consume an EOF
                          #:keep-eof? [keep-eof? #f])
  (let loop ([in orig-in])
    (cond
     [(input-port-closed? in)
      (raise-arguments-error who
                             "input port is closed"
                             "input port" orig-in)]
     ;; previously detected EOF?
     [(input-port-pending-eof? in) ;; FIXME: sync
      (unless keep-eof?
        (set-input-port-pending-eof?! in #f))
      eof]
     [else
      ;; normal mode...
      (define read-in (input-port-read-in in))
      (cond
       [(procedure? read-in)
        (define v (read-in bstr start end copy-bstr?))
        (cond
         [(exact-nonnegative-integer? v)
          (cond
           [(zero? v)
            (if zero-ok?
                0
                (loop in))]
           [(v . <= . (- end start))
            (port-count! orig-in v bstr start)
            v]
           [else
            (raise-arguments-error who
                                   "result integer is larger than the supplied byte string"
                                   "result" v
                                   "byte-string length" (- end start))])]
         [(eof-object? v) eof]
         [else
          (raise-result-error who
                              "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f procedure?)"
                              v)])]
       [else (loop read-in)])])))

;; Like `read-some-bytes!`, but merely peeks
(define (peek-some-bytes! who orig-in bstr start end skip
                          #:zero-ok? [zero-ok? #f]
                          #:copy-bstr? [copy-bstr? #t])
  (let loop ([in orig-in])
    (cond
     [(input-port-closed? in)
      (raise-arguments-error who
                             "input port is closed"
                             "input port" orig-in)]
     ;; previously detected EOF? (never skip past it)
     [(input-port-pending-eof? in)
      eof]
     [else
      (define peek-in (input-port-peek-in in))
      (cond
       [(procedure? peek-in)
        (define v (peek-in bstr start end skip copy-bstr?))
        (cond
         [(exact-nonnegative-integer? v)
          (cond
           [(zero? v)
            (if zero-ok?
                0
                (loop in))]
           [(v . <= . (- end start)) v]
           [else
            (raise-arguments-error who
                                   "result integer is larger than the supplied byte string"
                                   "result" v
                                   "byte-string length" (- end start))])]
         [(eof-object? v) eof]
         [else
          (raise-result-error who
                              "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f procedure?)"
                              v)])]
       [else (loop peek-in)])])))
