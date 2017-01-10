#lang racket/base
(require "config.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "parameter.rkt")

(provide read-number-or-symbol)

(define (read-number-or-symbol c in config
                               ;; `mode` can be 'number-or-symbol,
                               ;; 'symbol, 'keyword, or a number
                               ;; prefix string like "#e"
                               #:mode [mode 'number-or-symbol]
                               #:initial-pipe-quote? [initial-pipe-quote? #f]
                               #:extra-prefix [extra-prefix #f])
  (define accum-str (accum-string-init! config))
  (define quoted-ever? initial-pipe-quote?)
  (define case-sens? (check-parameter read-case-sensitive config))
  (when extra-prefix
    (accum-string-add! accum-str extra-prefix))
  (unless (or (not c) initial-pipe-quote?)
    (accum-string-add! accum-str c))
  
  ;; If we encounter an EOF or special in the wrong place:
  (define (unexpected-quoted c after-c)
    (reader-error in config #:eof? (eof-object? c)
                  "~a following `~a` in ~a"
                  (if (eof-object? c) "end-of-file" "non-character")
                  after-c (cond
                           [(eq? mode 'keyword) "keyword"]
                           [(string? mode) "number"]
                           [else "symbol"])))
  
  (let loop ([pipe-quote-c (and initial-pipe-quote? c)] ; currently quoting?
             [foldcase-from 0]) ; keep track of range to foldcase for case-insens
    (define c (peek-char-or-special in))
    (define ec (effective-char c config))
    (cond
     [(and pipe-quote-c
           (not (char? ec)))
      ;; Interrupted while in quoting mode
      (consume-char in c)
      (unexpected-quoted c pipe-quote-c)]
     [(and (not pipe-quote-c)
           (char-delimiter? c config))
      ;; EOF or other delimiter - done!
      (unless case-sens?
        (accum-string-convert! accum-str string-foldcase foldcase-from))]
     [(and pipe-quote-c (char=? c pipe-quote-c)) ; note: `pipe-quote-c` determines close, not readtable
      ;; End quoting mode
      (consume-char in c)
      (loop #f (accum-string-count accum-str))]
     [(char=? ec #\|)
      ;; Start quoting mode
      (consume-char in c)
      (set! quoted-ever? #t)
      (unless case-sens?
        (accum-string-convert! accum-str string-foldcase foldcase-from))
      (loop c (accum-string-count accum-str))]
     [(and (char=? ec #\\)
           (not pipe-quote-c))
      ;; Single-character quoting 
      (consume-char in c)
      (define next-c (read-char-or-special in))
      (unless (char? next-c)
        (unexpected-quoted next-c c))
      (unless (or pipe-quote-c case-sens?)
        (accum-string-convert! accum-str string-foldcase foldcase-from))
      (accum-string-add! accum-str next-c)
      (set! quoted-ever? #t)
      (loop #f (accum-string-count accum-str))]
     [else
      ;; Everything else
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop pipe-quote-c foldcase-from)]))
  
  (define str (accum-string-get! accum-str config))
  
  ;; Disallow "." as a symbol
  (when (and (= 1 (string-length str))
             (check-parameter read-accept-dot config)
             (not quoted-ever?)
             (char=? #\. (effective-char (string-ref str 0) config)))
    (reader-error in config "illegal use of `.`"))
  
  (define num
    (and (or (eq? mode 'number-or-symbol)
             (string? mode))
         (not quoted-ever?)
         (string->number (if (string? mode)
                             (string-append mode str)
                             str))))

  (when (and (not num)
             (string? mode))
    (reader-error in config
                  "bad number: `~a`"
                  (string-append mode str)))
  
  (wrap (or num
            (and (eq? mode 'keyword)
                 (string->keyword str))
            (string->symbol str))
        in
        config
        str))
