#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "readtable.rkt"
         "consume.rkt"
         "error.rkt")

(provide skip-whitespace-and-comments!)

;; Returns the next character as peeked
(define (skip-whitespace-and-comments! read-one in config)
  (define c (peek-char-or-special in))
  (define ec (effective-char c config))
  (cond
   [(eof-object? ec) c]
   [(not (char? ec)) c]
   [(char-whitespace? ec)
    (consume-char in c)
    (skip-whitespace-and-comments! read-one in config)]
   [(char=? #\; ec)
    (let loop ()
      (define c (read-char-or-special in))
      (unless (or (eof-object? c)
                  (char=? #\newline (effective-char c config)))
        (loop)))
    (skip-whitespace-and-comments! read-one in config)]
   [(and (char=? #\# ec)
         (eqv? #\| (peek-char-or-special in 1)))
    (skip-pipe-comment! c in config)
    (skip-whitespace-and-comments! read-one in config)]
   [(and (char=? #\# ec)
         (eqv? #\! (peek-char-or-special in 1))
         (let ([c3 (peek-char-or-special in 2)])
           (or (eqv? #\space c3)
               (eqv? #\/ c3))))
    (skip-unix-line-comment! in config)
    (skip-whitespace-and-comments! read-one in config)]
   [(and (char=? #\# ec)
         (eqv? #\; (peek-char-or-special in 1)))
    (consume-char in c)
    (consume-char in #\;)
    (define v
      (read-one in (struct*-copy read-config config
                                 [wrap #f])))
    (when (eof-object? v)
      (reader-error in config
                    #:eof? #t
                    "expected a commented-out element for `~a;', but found end-of-file"
                    ec))
    (skip-whitespace-and-comments! read-one in config)]
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

;; Skips a comment that starts #! and runs to the end of the line, but
;; can be continued with `\` at the end of the line
(define (skip-unix-line-comment! in config)
  (let loop ([backslash? #f])
    (define c (read-char-or-special in))
    (cond
     [(eof-object? c) (void)]
     [(not (char? c)) (loop #f)]
     [(char=? c #\newline)
      (when backslash?
        (loop #f))]
     [(char=? c #\\)
      (loop #t)]
     [else (loop #f)])))
