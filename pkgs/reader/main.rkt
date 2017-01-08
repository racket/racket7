#lang racket/base
(require "config.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "closer.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "indentation.rkt"
         "parameter.rkt"
         "digit.rkt"
         "sequence.rkt"
         "vector.rkt"
         "hash.rkt"
         "symbol.rkt"
         "string.rkt"
         "char.rkt")

(provide read-one
         make-read-config)

(define (read-one in config)
  (skip-whitespace-and-comments! in config)
  (define-values (line col pos) (port-next-location in))
  (define c (read-char-or-special in))
  (cond
   [(eof-object? c) eof]
   [(not (char? c)) c]
   [else
    ;; Map character via readtable:
    (define ec (effective-char c config))

    ;; Track indentation, unless it's a spurious closer:
    (when (not (char-closer? ec config))
      (track-indentation! config line col))
    (define r-config (reading-at config line col pos))
    
    (define-syntax-rule (guard-legal e body ...)
      (cond
       [e body ...]
       [else (reader-error in r-config "illegal use of `~a`" c)]))
    
    ;; Dispatch on character:
    (case ec
      [(#\#)
       (read-dispatch c in r-config)]
      [(#\')
       (read-quote 'quote "quoting '" c in r-config)]
      [(#\`)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (read-quote 'quasiquote "quasiquoting `" c in r-config))]
      [(#\,)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (define c2 (peek-char-or-special in))
        (if (eqv? c2 #\@)
            (begin
              (consume-char in c2)
              (read-quote 'unquote-splicing "unquoting ,@" c in r-config))
            (read-quote 'unquote "unquoting ," c in r-config)))]
      [(#\()
       (wrap (read-unwrapped-sequence read-one #\( #\) in r-config #:shape-tag? #t) in r-config ec)]
      [(#\))
       (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config))]
      [(#\[)
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (wrap (read-unwrapped-sequence read-one #\[ #\] in r-config #:shape-tag? #t) in r-config ec))]
      [(#\])
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config)))]
      [(#\{)
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (wrap (read-unwrapped-sequence read-one #\{ #\} in r-config #:shape-tag? #t) in r-config ec))]
      [(#\})
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config)))]
      [(#\")
       (read-string in r-config)]
      [(#\|)
       (read-number-or-symbol c in r-config #:initial-pipe-quote? #t #:mode 'symbol)]
      [else
       (read-number-or-symbol c in r-config)])]))

;; Dispatch on `#` character
(define (read-dispatch dispatch-c in config)
  (define c (read-char-or-special in))
  (cond
   [(eof-object? c)
    (reader-error in config #:eof? #t "bad syntax `~a`" dispatch-c)]
   [(not (char? c))
    (reader-error in config "bad syntax `~a`" dispatch-c)]
   [else
    (define-syntax-rule (guard-legal e c body ...)
      (cond
       [e body ...]
       [else (reader-error in config "bad syntax `~a~a`" dispatch-c)]))
    (case c
      [(#\()
       (read-vector read-one #\( #\) in config)]
      [(#\[)
       (guard-legal
        (check-parameter read-square-bracket-as-paren config)
        c
        (read-vector read-one #\[ #\] in config))]
      [(#\{)
       (guard-legal
        (check-parameter read-curly-brace-as-paren config)
        c
        (read-vector read-one #\{ #\} in config))]
      [(#\')
       (read-quote 'syntax "quoting #'" c in config)]
      [(#\`)
       (read-quote 'quasisyntax "quasiquoting #`" c in config)]
      [(#\,)
       (define c2 (peek-char-or-special in))
       (if (eqv? c2 #\@)
           (begin
             (consume-char in c2)
             (read-quote 'unsyntax-splicing "unquoting #,@" c in config))
           (read-quote 'unsyntax "unquoting #," c in config))]
      [(#\\)
       (read-character in config)]
      [(#\")
       (read-string in config #:mode '|byte string|)]
      [(#\<)
       (cond
        [(eqv? #\< (peek-char-or-special in))
         (consume-char in #\<)
         (read-here-string in config)]
        [else
         (reader-error in config "bad syntax `~a<`" dispatch-c)])]
      [(#\%)
       (read-number-or-symbol c in config #:extra-prefix dispatch-c #:mode 'symbol)]
      [(#\:)
       (read-number-or-symbol #f in config #:mode 'keyword)]
      [(#\t #\T)
       (define c2 (peek-char-or-special in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [else (read-delimited-constant c '(#\r #\u #\e) #t in config)])]
      [(#\f #\F)
       (define c2 (peek-char-or-special in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [(or (char=? c2 #\x) (char=? c2 #\l))
         (define vector-mode (if (char=? c2 #\x) 'fixnum 'flonum))
         (consume-char in c2)
         (when (read-config-for-syntax? config)
           (reader-error in config "literal f~avectors not allowed" c2))
         (define c3 (read-char-or-special in))
         (define-values (vector-len len-str c4)
           (cond
            [(decimal-digit? c3) (read-simple-number in config c3)]
            [else (values #f "" c3)]))
         (case c4
           [(#\()
            (read-vector read-one #\{ #\} in config #:mode vector-mode #:length vector-len)]
           [(#\[)
            (guard-legal
             (check-parameter read-square-bracket-as-paren config)
             (format "~a~a" c c2)
             (read-vector read-one #\[ #\] in config #:mode vector-mode #:length vector-len))]
           [(#\{)
            (guard-legal
             (check-parameter read-curly-brace-as-paren config)
             (format "~a~a" c c2)
             (read-vector read-one #\{ #\} in config #:mode vector-mode #:length vector-len))]
           [else
            (reader-error in config #:eof (eof-object? c4)
                          "expected `(`, `[`, or `{` after `#~a~a~a`"
                          c c2 len-str)])]
        [else (read-delimited-constant c '(#\a #\l #\s #\e) #f in config)])]
      [(#\e) (read-number-or-symbol #f in config #:mode "#e")]
      [(#\E) (read-number-or-symbol #f in config #:mode "#E")]
      [(#\i) (read-number-or-symbol #f in config #:mode "#i")]
      [(#\I) (read-number-or-symbol #f in config #:mode "#I")]
      [(#\d) (read-number-or-symbol #f in config #:mode "#d")]
      [(#\B) (read-number-or-symbol #f in config #:mode "#B")]
      [(#\o) (read-number-or-symbol #f in config #:mode "#o")]
      [(#\O) (read-number-or-symbol #f in config #:mode "#O")]
      [(#\D) (read-number-or-symbol #f in config #:mode "#D")]
      [(#\b) (read-number-or-symbol #f in config #:mode "#b")]
      [(#\x) (read-number-or-symbol #f in config #:mode "#x")]
      [(#\X) (read-number-or-symbol #f in config #:mode "#X")]
      [(#\h #\H) (read-hash read-one dispatch-c c in config)]
      [else
       (reader-error in config "bad syntax `~a~a`" dispatch-c c)])]))

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

(define (read-quote sym desc c in config)
  (define wrapped-sym (wrap sym in config c))
  (define e (read-one in config))
  (when (eof-object? e)
    (reader-error in config #:eof? #t
                  "expected an element for ~a (found end-of-file)"))
  (wrap (list wrapped-sym e) in config #f))

(define (read-simple-number in config init-c)
  (define accum-str (accum-string-init! config))
  (define v (read-digits in config accum-str
                         #:base 10 #:max-count +inf.0))
  (values v
          (accum-string-get! accum-str config)
          ;; We could avoid some peeks vising init-c
          ;; and having `read-digit` return its peek
          ;; result, but we don't for now
          (peek-char-or-special in)))
