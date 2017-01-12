#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "consume.rkt"
         "error.rkt"
         "special-comment.rkt")

(provide skip-whitespace-and-comments!)

;; Skip whitespace, including non-character values that are
;; `special-comment?`s
(define (skip-whitespace-and-comments! read-one in config
                                       #:keep-special-comment? [keep-special-comment? #f])
  (define rt (read-config-readtable config))
  (define source (read-config-source config))
  (let skip-loop ()
    (define c (peek-char/special in config 0 source))
    (define ec (readtable-effective-char rt c))
    (cond
     [(eof-object? ec) c]
     [(not (char? ec))
      (define v (special-value c))
      (cond
       [(and (special-comment? v)
             (not keep-special-comment?))
        (consume-char in c)
        (skip-loop)]
       [else v])]
     [(char-whitespace? ec)
      (consume-char in c)
      (skip-loop)]
     [(char=? #\; ec)
      (let loop ()
        (define c (read-char/special in config source))
        (unless (or (eof-object? c)
                    (char=? #\newline (effective-char c config)))
          (loop)))
      (skip-loop)]
     [(and (char=? #\# ec)
           (eqv? #\| (peek-char/special in config 1 source)))
      (skip-pipe-comment! c in config)
      (skip-loop)]
     [(and (char=? #\# ec)
           (eqv? #\! (peek-char/special in config 1 source))
           (let ([c3 (peek-char/special in config 2 source)])
             (or (eqv? #\space c3)
                 (eqv? #\/ c3))))
      (skip-unix-line-comment! in config)
      (skip-loop)]
     [(and (char=? #\# ec)
           (eqv? #\; (peek-char/special in config 1 source)))
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
      (skip-loop)]
     [else c])))

;; Skips balanced pipe comments
(define (skip-pipe-comment! init-c in config)
  (define source (read-config-source config))
  (define-values (line col pos) (port-next-location in))
  (consume-char in init-c)
  (consume-char in #\|)
  (let loop ([prev-c #f] [depth 0])
    (define c (read-char/special in config source))
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
    (define c (read-char/special in config))
    (cond
     [(eof-object? c) (void)]
     [(not (char? c)) (loop #f)]
     [(char=? c #\newline)
      (when backslash?
        (loop #f))]
     [(char=? c #\\)
      (loop #t)]
     [else (loop #f)])))
