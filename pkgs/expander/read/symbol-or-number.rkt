#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "parameter.rkt")

(provide read-number-or-symbol)

(define (read-number-or-symbol init-c in config
                               ;; `mode` can be 'number-or-symbol,
                               ;; 'symbol, 'keyword, or a number
                               ;; prefix string like "#e"
                               #:mode [mode 'number-or-symbol]
                               #:extra-prefix [extra-prefix #f])
  (define accum-str (accum-string-init! config))
  (define quoted-ever? #f)
  (define case-sens? (check-parameter read-case-sensitive config))
  (when extra-prefix
    (accum-string-add! accum-str extra-prefix))
  (define rt (read-config-readtable config))
  (define source (read-config-source config))
  
  ;; If we encounter an EOF or special in the wrong place:
  (define (unexpected-quoted c after-c)
    (reader-error in config #:eof? (eof-object? c)
                  "~a following `~a` in ~a"
                  (if (eof-object? c) "end-of-file" "non-character")
                  after-c (cond
                           [(eq? mode 'keyword) "keyword"]
                           [(string? mode) "number"]
                           [else "symbol"])))
  
  (let loop ([init-c init-c]
             [pipe-quote-c #f] ; currently quoting?
             [foldcase-from 0]) ; keep track of range to foldcase for case-insens
    (define c (or init-c (peek-char/special in config 0 source)))
    (define ec (readtable-effective-char rt c))
    (cond
     [(and pipe-quote-c
           (not (char? ec)))
      ;; Interrupted while in quoting mode
      (unless init-c (consume-char in c))
      (unexpected-quoted c pipe-quote-c)]
     [(and (not pipe-quote-c)
           (readtable-char-delimiter? rt c config))
      ;; EOF or other delimiter - done!
      (unless case-sens?
        (accum-string-convert! accum-str string-foldcase foldcase-from))]
     [(and pipe-quote-c
           (char=? c pipe-quote-c)) ; note: `pipe-quote-c` determines close, not readtable
      ;; End quoting mode
      (unless init-c (consume-char in c))
      (loop #f #f (accum-string-count accum-str))]
     [(char=? ec #\|)
      ;; Start quoting mode
      (unless init-c (consume-char in c))
      (set! quoted-ever? #t)
      (unless case-sens?
        (accum-string-convert! accum-str string-foldcase foldcase-from))
      (loop #f c (accum-string-count accum-str))]
     [(and (char=? ec #\\)
           (not pipe-quote-c))
      ;; Single-character quoting 
      (unless init-c (consume-char in c))
      (define next-c (read-char/special in config source))
      (unless (char? next-c)
        (unexpected-quoted next-c c))
      (unless (or pipe-quote-c case-sens?)
        (accum-string-convert! accum-str string-foldcase foldcase-from))
      (accum-string-add! accum-str next-c)
      (set! quoted-ever? #t)
      (loop #f #f (accum-string-count accum-str))]
     [else
      ;; Everything else
      (unless init-c (consume-char in c))
      (accum-string-add! accum-str c)
      (loop #f pipe-quote-c foldcase-from)]))
  
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
                             str)
                         10
                         'read
                         (if (check-parameter read-decimal-as-inexact config)
                             'decimal-as-inexact
                             'decimal-as-exact))))
  (when (string? num)
    (reader-error in config "~a" num))

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
