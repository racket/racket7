#lang racket/base
(require "../common/atomic.rkt"
         "../host/evt.rkt"
         "port.rkt"
         "input-port.rkt"
         "count.rkt")

(provide read-some-bytes!
         peek-some-bytes!)

;; Read up to `(- end start)` bytes, producing at least a
;; single by unless `zero-ok?` is true. The result is
;; EOF or the number of bytes read.
(define (read-some-bytes! who orig-in bstr start end
                          ;; Zero is ok for `read-bytes!*`:
                          #:zero-ok? [zero-ok? #f]
                          ;; Enable breaks while blocking?
                          #:enable-break? [enable-break? #f]
                          ;; When calling an externally implemented
                          ;; port, we normally make a fresh byte
                          ;; string, because we don't trust the
                          ;; reading proc to not retain the byte
                          ;; string and change it later. We can skip
                          ;; the copy if the bstr is the right length
                          ;; and won't be exposed, though.
                          #:copy-bstr? [copy-bstr? #t]
                          ;; If `keep-eof?`, don't consume an EOF
                          #:keep-eof? [keep-eof? #f])
  (let loop ([in orig-in])
    (cond
     [(= start end) 0]
     [(core-port-closed? in)
      (raise-arguments-error who
                             "input port is closed"
                             "input port" orig-in)]
     ;; previously detected EOF?
     [(core-input-port-pending-eof? in) ;; FIXME: sync
      (unless keep-eof?
        (set-core-input-port-pending-eof?! in #f))
      eof]
     [else
      ;; normal mode...
      (define read-in (core-input-port-read-in in))
      (cond
        [(procedure? read-in)
         (start-atomic)
         (define v (read-in bstr start end copy-bstr?))
         (when (and (integer? v) (not (eq? v 0)))
           (port-count! orig-in v bstr start))
         (end-atomic)
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
           [(evt? v)
            (cond
              [zero-ok? 0]
              [else
               (sync v)
               (loop in)])]
           [else
            (raise-result-error who
                                "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f procedure?)"
                                v)])]
        [else (loop read-in)])])))

;; Like `read-some-bytes!`, but merely peeks
(define (peek-some-bytes! who orig-in bstr start end skip
                          #:progress-evt [progress-evt #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f]
                          #:copy-bstr? [copy-bstr? #t])
  (let loop ([in orig-in])
    (cond
     [(= start end) 0]
     [(core-port-closed? in)
      (raise-arguments-error who
                             "input port is closed"
                             "input port" orig-in)]
     ;; previously detected EOF? (never skip past it)
     [(core-input-port-pending-eof? in)
      eof]
     [(zero? (bytes-length bstr)) 0]
     [else
      (define peek-in (core-input-port-peek-in in))
      (cond
       [(procedure? peek-in)
        (define v (atomically (peek-in bstr start end skip progress-evt copy-bstr?)))
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
         [(evt? v)
          (cond
            [zero-ok? 0]
            [else
             (sync v)
             (loop in)])]
         [else
          (raise-result-error who
                              "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f procedure?)"
                              v)])]
       [else (loop peek-in)])])))
