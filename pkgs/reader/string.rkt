#lang racket/base
(require "config.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt")

(provide read-string)

(define (read-string in config)
  (define accum-str (accum-string-init! config))
  (define (bad-end c)
    (cond
     [(eof-object? c)
      (reader-error in config #:eof? #t "expected a closing `\"`")]
     [else
      (reader-error in config "found non-character while reading a string")]))
  (let loop ()
    (define c (read-char-or-special in))
    (define ec (effective-char c config))
    (cond
     [(not (char? ec))
      (bad-end ec)]
     [(char=? #\\ ec)
      (define escaping-c c)
      (define escaped-c (read-char-or-special in))
      (when (not (char? escaped-c))
        (bad-end escaped-c))
      (case escaped-c
        [(#\\ #\" #\')
         (accum-string-add! accum-str escaped-c)]
        [(#\a) (accum-string-add! accum-str #\u7)]
        [(#\b) (accum-string-add! accum-str #\backspace)]
        [(#\t) (accum-string-add! accum-str #\tab)]
        [(#\n) (accum-string-add! accum-str #\newline)]
        [(#\v) (accum-string-add! accum-str #\vtab)]
        [(#\f) (accum-string-add! accum-str #\page)]
        [(#\r) (accum-string-add! accum-str #\return)]
        [(#\e) (accum-string-add! accum-str #\u1B)]
        [(#\newline) (void)]
        [(#\return)
         (define maybe-newline-c (peek-char-or-special in))
         (when (eqv? maybe-newline-c #\newline)
           (consume-char in maybe-newline-c))
         (void)]
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         ;; Octal (valid if <= 255)
         (define pos (accum-string-count accum-str))
         (accum-string-add! accum-str escaped-c)
         (define init-v (digit->number escaped-c))
         (define v (read-digits in config accum-str #:base 8 #:max-count 2
                                #:init init-v
                                #:zero-digits-result init-v))
         (unless (v . <= . 255)
           (reader-error in config
                         "escape sequence `~a~a` is out of range in string"
                         escaping-c (accum-string-get! accum-str #:start-pos pos)))
         (set-accum-string-count! accum-str pos)
         (accum-string-add! accum-str (integer->char v))]
        [(#\x)
         ;; Hex, two characters (always valid)
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 2))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (set-accum-string-count! accum-str pos)
         (accum-string-add! accum-str (integer->char v))]
        [(#\u)
         ;; Hex, four characters (valid if not surrogate or if surrogate pair)
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 4))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (cond
          [(or (v . < . #xD800) (v . > . #xDFFF))
           ;; Normal \u escape
           (set-accum-string-count! accum-str pos)
           (accum-string-add! accum-str (integer->char v))]
          [else
           ;; Maybe a surrogate-pair encoding
           (define (next!)
             (define next-c (read-char-or-special in))
             (when (char? next-c)
               (accum-string-add! accum-str next-c))
             next-c)
           (define v2
             (let ([next-c (next!)])
               (cond
                [(char=? next-c #\\)
                 (define next-c (next!))
                 (cond
                  [(char=? next-c #\u)
                   (define v2 (read-digits in config accum-str #:base 16 #:max-count 4))
                   (cond
                    [(integer? v2)
                     (and (v2 . >= . #xDC00)
                          (v2 . <= . #xDFFF)
                          v2)]
                    [else v2])]    ; maybe EOF
                  [else next-c])]  ; maybe EOF
                [else next-c]))) ; maybe EOF
           (cond
            [(integer? v2)
             (define combined-v (+ (arithmetic-shift (- v #xD800) 10)
                                   (- v2 #xDC00)
                                   #x10000))
             (cond
              [(combined-v . > . #x10FFFF)
               (reader-error in config
                             "escape sequence `~au~a` is out of range in string"
                             escaping-c (accum-string-get! accum-str config #:start-pos pos))]
              [else
               (set-accum-string-count! accum-str pos)
               (accum-string-add! accum-str (integer->char combined-v))])]
            [else
             (reader-error in config
                           #:eof? (eof-object? v2)
                           "bad or incomplete surrogate-style encoding at `~au~a`"
                           escaping-c (accum-string-get! accum-str config #:start-pos pos))])])]
        [(#\U)
         (define pos (accum-string-count accum-str))
         (define v (read-digits in config accum-str #:base 16 #:max-count 8))
         (unless (integer? v) (no-hex-digits in config v escaping-c escaped-c))
         (cond
          [(and (or (v . < . #xD800) (v . > . #xDFFF))
                (v . <= . #x10FFFF))
           (set-accum-string-count! accum-str pos)
           (accum-string-add! accum-str (integer->char v))]
          [else
           (reader-error in config
                         "escape sequence `~aU~a` is out of range in string"
                         escaping-c (accum-string-get! accum-str config #:start-pos pos))])]
        [else
         (reader-error in config
                       "unknown escape sequence `~a~a` in string"
                       escaping-c escaped-c)])
      (loop)]
     [(char=? #\" ec)
      null]
     [else
      (accum-string-add! accum-str c)
      (loop)]))
  (define str (accum-string-get! accum-str config))
  (wrap str
        in
        config
        str))

;; ----------------------------------------

(define (read-digits in config accum-str
                     #:base base #:max-count max-count
                     #:init [init-v 0]
                     #:zero-digits-result [zero-digits-result #f])
  (define c (peek-char-or-special in))
  (cond
   [(digit? c base)
    (consume-char in c)
    (accum-string-add! accum-str c)
    (let loop ([v (+ (digit->number c) (* init-v base))]
               [max-count (sub1 max-count)])
      (cond
       [(zero? max-count) v]
       [else
        (define c (peek-char-or-special in))
        (cond
         [(digit? c base)
          (consume-char in c)
          (accum-string-add! accum-str c)
          (loop (+ (digit->number c) (* v base)) (sub1 max-count))]
         [else v])]))]
   [zero-digits-result zero-digits-result]
   [else c]))

(define (digit? c base)
  (cond
   [(not (char? c)) #f]
   [(= base 8) (and (char>=? c #\0) (char<=? c #\7))]
   [(= base 16) (or (and (char>=? c #\0) (char<=? c #\9))
                    (and (char>=? c #\A) (char<=? c #\F))
                    (and (char>=? c #\a) (char<=? c #\f)))]
   [else (and (char>=? c #\0) (char<=? c #\9))]))

(define (digit->number c)
  (cond
   [(and (char>=? c #\0) (char<=? c #\9))
    (- (char->integer c) (char->integer #\0))]
   [(and (char>=? c #\A) (char<=? c #\F))
    (- (char->integer c) (- (char->integer #\A) 10))]
   [else
    (- (char->integer c) (- (char->integer #\a) 10))]))

;; ----------------------------------------

(define (no-hex-digits in config c escaping-c escaped-c)
  (reader-error in config
                #:eof? (eof-object? c)
                "no hex digit following `~a~a`"
                escaping-c escaped-c))
