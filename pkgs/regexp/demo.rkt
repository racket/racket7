#lang racket/base
(require (prefix-in rx: "main.rkt"))


(define (check rx in N [M (max 1 (quotient N 10))])
  (define orig-v (regexp-match rx in))
  (define new-v (rx:regexp-match rx in))
  (unless (equal? orig-v new-v)
    (error 'check "failed ~s ~s ~s ~s" rx in orig-v new-v))
  
  (define c-start (current-inexact-milliseconds))
  (define orig-rx
    (if (bytes? rx)
        (for/fold ([r #f]) ([i (in-range M)])
          (byte-regexp rx))
        (for/fold ([r #f]) ([i (in-range M)])
          (regexp rx))))
  (define c-after-orig (current-inexact-milliseconds))
  (define new-rx
    (if (bytes? rx)
        (for/fold ([r #f]) ([i (in-range M)])
          (rx:byte-regexp rx))
        (for/fold ([r #f]) ([i (in-range M)])
          (rx:regexp rx))))
  (define c-after-new (current-inexact-milliseconds))

  (define start (current-inexact-milliseconds))
  (for/fold ([r #f]) ([i (in-range N)])
    (regexp-match orig-rx in))
  (define after-orig (current-inexact-milliseconds))
  (for/fold ([r #f]) ([i (in-range N)])
    (rx:regexp-match new-rx in))
  (define after-new (current-inexact-milliseconds))
  
  (define orig-c-msec (- c-after-orig c-start))
  (define new-c-msec (- c-after-new c-after-orig))
  (define orig-msec (- after-orig start))
  (define new-msec (- after-new after-orig))
  
  (printf "~.s:\n  ~a  (~a vs. ~a)\n"
          rx
          (/ new-c-msec orig-c-msec)
          orig-c-msec
          new-c-msec)
  (printf "~.s:\n  ~a  (~a vs. ~a)\n"
          in
          (/ new-msec orig-msec)
          orig-msec
          new-msec))

;; ----------------------------------------

(check #"ab(?:a*c)*d"
       #"abaacacaaacacaaacd"
       100000)

(define ipv6-hex "[0-9a-fA-F:]*:[0-9a-fA-F:]*")

(define url-s
  (string-append
   "^"
   "(?:"              ; / scheme-colon-opt
   "([^:/?#]*)"       ; | #1 = scheme-opt
   ":)?"              ; \
   "(?://"            ; / slash-slash-authority-opt
   "(?:"              ; | / user-at-opt
   "([^/?#@]*)"       ; | | #2 = user-opt
   "@)?"              ; | \
   "(?:"              ;
   "(?:\\["           ; | / #3 = ipv6-host-opt
   "(" ipv6-hex ")"   ; | | hex-addresses
   "\\])|"            ; | \
   "([^/?#:]*)"       ; | #4 = host-opt
   ")?"               ;
   "(?::"             ; | / colon-port-opt
   "([0-9]*)"         ; | | #5 = port-opt
   ")?"               ; | \
   ")?"               ; \
   "([^?#]*)"         ; #6 = path
   "(?:\\?"           ; / question-query-opt
   "([^#]*)"          ; | #7 = query-opt
   ")?"               ; \
   "(?:#"             ; / hash-fragment-opt
   "(.*)"             ; | #8 = fragment-opt
   ")?"               ; \
   "$"))

(define rlo "https://racket-lang.org:80x/people.html?check=ok#end")

(check (string->bytes/utf-8 url-s)
       (string->bytes/utf-8 rlo)
       100000)

(check url-s
       rlo
       10000)

(check #"a*b"
       (make-bytes 1024 (char->integer #\a))
       100000)
