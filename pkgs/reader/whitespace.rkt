#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "consume.rkt")

(provide skip-whitespace-and-comments!)

;; Returns the next character as peeked
(define (skip-whitespace-and-comments! in config)
  (define c (peek-char in))
  (define ec (effective-char c config))
  (cond
   [(eof-object? ec) c]
   [(char-whitespace? ec)
    (consume-char in c)
    (skip-whitespace-and-comments! in config)]
   [(char=? #\; ec)
    (let loop ()
      (define c (read-char in))
      (unless (char=? #\newline (effective-char c config))
        (loop)))
    (skip-whitespace-and-comments! in config)]
   [else c]))
