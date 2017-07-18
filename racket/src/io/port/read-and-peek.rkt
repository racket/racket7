#lang racket/base
(require "../common/internal-error.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "count.rkt")

(provide read-some-bytes!
         peek-some-bytes!

         do-read-byte
         read-byte-via-bytes
         do-peek-byte
         peek-byte-via-bytes)

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
                          #:keep-eof? [keep-eof? #f]
                          ;; If not `special-ok?` and a special value is
                          ;; received, raise an exception
                          #:special-ok? [special-ok? #t])
  (let loop ([in orig-in])
    (start-atomic)
    (cond
      [(= start end)
       (end-atomic)
       0]
      [(closed-state-closed? (core-port-closed in))
       (end-atomic)
       (raise-arguments-error who
                              "input port is closed"
                              "input port" orig-in)]
      ;; previously detected EOF?
      [(core-input-port-pending-eof? in)
       (unless keep-eof?
         (set-core-input-port-pending-eof?! in #f))
       (end-atomic)
       eof]
      [else
       ;; normal mode...
       (define read-in (core-input-port-read-in in))
       (cond
         [(procedure? read-in)
          (define v (read-in bstr start end copy-bstr?))
          (let result-loop ([v v])
            (cond
              [(and (integer? v) (not (eq? v 0)))
               (port-count! orig-in v bstr start)]
              [(procedure? v)
               (port-count-byte! in #f)])
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
                  (define next-v (sync v))
                  (start-atomic)
                  (result-loop next-v)])]
              [(procedure? v)
               (if special-ok?
                   v
                   (raise-arguments-error who
                                          "non-character in an unsupported context"
                                          "port" orig-in))]
              [(procedure? v)
               (if special-ok?
                   v
                   (raise-arguments-error who
                                          "non-character in an unsupported context"
                                          "port" orig-in))]
              [else
               (internal-error (format "weird read-bytes result ~s" v))]))]
         [else
          (end-atomic)
          (loop (->core-input-port read-in))])])))

;; Like `read-some-bytes!`, but merely peeks
(define (peek-some-bytes! who orig-in bstr start end skip
                          #:progress-evt [progress-evt #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f]
                          #:copy-bstr? [copy-bstr? #t]
                          #:special-ok? [special-ok? #t])
  (let loop ([in orig-in])
    (start-atomic)
    (cond
      [(= start end)
       (end-atomic)
       0]
      [(closed-state-closed? (core-port-closed in))
       (end-atomic)
       (raise-arguments-error who
                              "input port is closed"
                              "input port" orig-in)]
      ;; previously detected EOF? (never skip past it)
      [(core-input-port-pending-eof? in)
       (end-atomic)
       eof]
      [else
       (define peek-in (core-input-port-peek-in in))
       (cond
         [(procedure? peek-in)
          (define v (peek-in bstr start end skip progress-evt copy-bstr?))
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
            [(procedure? v)
             (if special-ok?
                 v
                 (raise-arguments-error who
                                        "non-character in an unsupported context"
                                        "port" orig-in))]
            [else
             (internal-error (format "weird peek-bytes result ~s" v))])]
         [else
          (end-atomic)
          (loop peek-in)])])))


;; Use a `read-byte` shortcut
(define (do-read-byte read-byte in)
  (let loop ()
    (start-atomic)
    (define b (read-byte))
    (cond
      [(eof-object? b)
       (end-atomic)
       b]
      [(evt? b)
       (end-atomic)
       (sync b)
       (loop)]
      [else
       (port-count-byte! in b)
       (end-atomic)
       b])))

;; Use the general path; may return a procedure for a special
(define (read-byte-via-bytes in #:special-ok? [special-ok? #t])
  (define bstr (make-bytes 1))
  (define v (read-some-bytes! 'read-byte in bstr 0 1 #:copy-bstr? #f #:special-ok? special-ok?))
  (if (eq? v 1)
      (bytes-ref bstr 0)
      v))

;; Use a `peek-byte` shortcut
(define (do-peek-byte peek-byte in)
  (let loop ()
    (define b (atomically (peek-byte)))
    (cond
      [(evt? b)
       (sync b)
       (loop)]
      [else b])))

;; Use the general path; may return a procedure for a special
(define (peek-byte-via-bytes in skip-k
                             #:special-ok? [special-ok? #t]
                             #:progress-evt [progress-evt #f])
  (define bstr (make-bytes 1))
  (define v (peek-some-bytes! 'peek-byte in bstr 0 1 skip-k
                              #:copy-bstr? #f
                              #:special-ok? special-ok?
                              #:progress-evt progress-evt))
  (if (eq? v 1)
      (bytes-ref bstr 0)
      v))
