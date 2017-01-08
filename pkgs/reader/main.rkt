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
         "sequence.rkt"
         "vector.rkt"
         "struct.rkt"
         "graph.rkt"
         "hash.rkt"
         "symbol-or-number.rkt"
         "string.rkt"
         "char.rkt"
         "quote.rkt"
         "constant.rkt"
         "box.rkt")

(provide read)

(define (read in #:source [source #f])
  (define config (make-read-config #:source source))
  (define v (read-one in config))
  (cond
   [(read-config-state-graph (read-config-st config))
    (make-reader-graph v)]
   [else v]))

(define (read-one in config)
  (cond
   [(not (check-parameter read-cdot config))
    ;; No parsing of `.` as `#%dot`
    (read-undotted in config)]
   [(check-parameter read-cdot config)
    ;; Look for `<something> . <something>`
    (define-values (line col pos) (port-next-location in))
    (define v (read-undotted in config))
    (let loop ([v v])
      (define c (peek-char-or-special in))
      (define ec (effective-char c config))
      (cond
       [(not (char? ec))
        v]
       [(char-whitespace? ec)
        (consume-char in c)
        (loop v)]
       [(char=? ec #\.)
        (define-values (dot-line dot-col dot-pos) (port-next-location in))
        (consume-char in c)
        (define cdot (wrap '#%dot in (reading-at config dot-line dot-col dot-pos) #\.))
        (define post-v (read-undotted in config))
        (loop (wrap (list '#%dot v post-v) in (reading-at config line col pos) #\.))]
       [else v]))]))

(define (read-undotted in config)
  (skip-whitespace-and-comments! read-one in config)
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
       (read-quote read-one 'quote "quoting '" c in r-config)]
      [(#\`)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (read-quote read-one 'quasiquote "quasiquoting `" c in r-config))]
      [(#\,)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (define c2 (peek-char-or-special in))
        (if (eqv? c2 #\@)
            (begin
              (consume-char in c2)
              (read-quote read-one 'unquote-splicing "unquoting ,@" c in r-config))
            (read-quote read-one 'unquote "unquoting ," c in r-config)))]
      [(#\()
       (wrap (read-unwrapped-sequence read-one ec #\( #\) in r-config #:shape-tag? #t) in r-config ec)]
      [(#\))
       (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config))]
      [(#\[)
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (wrap (read-unwrapped-sequence read-one ec #\[ #\] in r-config #:shape-tag? #t) in r-config ec))]
      [(#\])
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config)))]
      [(#\{)
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (wrap (read-unwrapped-sequence read-one ec #\{ #\} in r-config #:shape-tag? #t) in r-config ec))]
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
       [else (bad-syntax-error in config (format "~a~a" dispatch-c c))]))
    (case c
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       ;; Vector, graph definition, or graph reference
       (read-vector-or-graph read-one dispatch-c c in config)]
      [(#\()
       (read-vector read-one #\( #\( #\) in config)]
      [(#\[)
       (guard-legal
        (check-parameter read-square-bracket-as-paren config)
        c
        (read-vector read-one #\[ #\[ #\] in config))]
      [(#\{)
       (guard-legal
        (check-parameter read-curly-brace-as-paren config)
        c
        (read-vector read-one #\{ #\{ #\} in config))]
      [(#\s)
       (read-struct read-one dispatch-c in config)]
      [(#\&)
       (read-box read-one dispatch-c in config)]
      [(#\')
       (read-quote read-one 'syntax "quoting #'" c in config)]
      [(#\`)
       (read-quote read-one 'quasisyntax "quasiquoting #`" c in config)]
      [(#\,)
       (define c2 (peek-char-or-special in))
       (if (eqv? c2 #\@)
           (begin
             (consume-char in c2)
             (read-quote read-one 'unsyntax-splicing "unquoting #,@" c in config))
           (read-quote read-one 'unsyntax "unquoting #," c in config))]
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
         (read-fixnum-or-flonum-vector read-one dispatch-c c c2 in config)]
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
