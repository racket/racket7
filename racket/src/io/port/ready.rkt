#lang racket/base
(require "../common/check.rkt"
         "../string/utf-8-decode.rkt"
         "input-port.rkt"
         "bytes-input.rkt")

(provide byte-ready?
         char-ready?)

(define/who (byte-ready? in)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (not (eqv? 0 (peek-bytes-avail!* (make-bytes 1) 0 #f in)))))

(define/who (char-ready? in)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (define peek-byte (core-input-port-peek-byte in))
    (define b (and peek-byte (peek-byte)))
    (cond
      [(and b
            (or (eof-object? b)
                (b . < . 128)))
       ;; Shortcut worked
       #t]
      [else
       (define bstr (make-bytes 1))
       (let loop ([offset 0] [state #f])
         (when (eq? 1 (peek-bytes-avail!* (make-bytes 1) offset #f in))
           (define-values (used-bytes got-chars state)
             (utf-8-decode! bstr 0 1
                            #f 0 #f
                            #:error-char #\?
                            #:abort-mode 'state
                            #:state state))
           (cond
             [(utf-8-state? state)
              (loop (add1 offset) state)]
             [else #t])))])))
