#lang racket/base
(require "delimiter.rkt"
         "accum-string.rkt"
         "error.rkt"
         "consume.rkt"
         "wrap.rkt")

(provide read-delimited-constant)

(define (read-delimited-constant init-c chars val in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str init-c)
  (let loop ([chars chars])
    (define c (peek-char-or-special in))
    (cond
     [(char-delimiter? c config)
      (unless (null? chars)
        (reader-error in config
                      "bad syntax `#~a`" (accum-string-get! accum-str config)
                      #:eof? (eof-object? c)))]
     [(null? chars)
      (accum-string-add! accum-str c)
      (reader-error in config
                    "bad syntax `#~a`" (accum-string-get! accum-str config))]
     [(char-ci=? c (car chars))
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop (cdr chars))]
     [else
      (consume-char in c)
      (accum-string-add! accum-str c)
      (reader-error "bad syntax `#~a`" (accum-string-get! accum-str config))]))
  (wrap val in config (accum-string-get! accum-str config)))
