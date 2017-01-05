#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "consume.rkt"
         "error.rkt")

(provide skip-whitespace-and-comments!)

;; Returns the next character as peeked
(define (skip-whitespace-and-comments! in config)
  (define c (peek-char-or-special in))
  (define ec (effective-char c config))
  (cond
   [(eof-object? ec) c]
   [(not (char? ec)) c]
   [(char-whitespace? ec)
    (consume-char in c)
    (skip-whitespace-and-comments! in config)]
   [(char=? #\; ec)
    (let loop ()
      (define c (read-char-or-special in))
      (unless (or (eof-object? c)
                  (char=? #\newline (effective-char c config)))
        (loop)))
    (skip-whitespace-and-comments! in config)]
   [(and (char=? #\# ec)
         (eqv? #\| (peek-char-or-special in 1)))
    (skip-pipe-comment! c in config)
    (skip-whitespace-and-comments! in config)]
   [else c]))

;; Skips balanced pipe comments
(define (skip-pipe-comment! init-c in config)
  (define-values (line col pos) (port-next-location in))
  (consume-char in init-c)
  (consume-char in #\|)
  (let loop ([prev-c #f] [depth 0])
    (define c (read-char-or-special in))
    (cond
     [(eof-object? c)
      (reader-error in (reading-at config line col pos) #:eof? #t
                    "end of file in `#|` comment")]
     [(not (char? c))
      (loop #f depth)]
     [(and (char=? #\| c) (eqv? prev-c #\#))
      (loop #f (add1 depth))]
     [(and (char=? #\# c) (eqv? prev-c #\|))
      (when (positive? depth)
        (loop #f (sub1 depth)))]
     [else (loop c depth)])))
