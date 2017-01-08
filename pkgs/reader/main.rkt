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
         "symbol-or-number.rkt"
         "string.rkt"
         "char.rkt")

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
       [else (reader-error in config "bad syntax `~a~a`" dispatch-c c)]))
    (case c
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       ;; Vector, graph definition, or graph reference
       (read-vector-or-graph dispatch-c c in config)]
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

(define (read-vector-or-graph dispatch-c init-c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str init-c)
  
  (define init-v (digit->number init-c))
  
  (define v (read-digits in config accum-str
                         #:base 10 #:max-count +inf.0
                         #:init init-v
                         #:zero-digits-result init-v))
  (define-values (post-line post-col post-pos) (port-next-location in))
  
  (define (get-accum c)
    (format "~a~a~a" dispatch-c (accum-string-get! accum-str config) c))
  (define-syntax-rule (guard-legal e c body ...)
    (cond
     [e body ...]
     [else (reader-error in config
                         "bad syntax `~a`"
                         (get-accum c))]))
  
  (define c (read-char-or-special in))
  (define ec (effective-char c config))
  (case ec
    [(#\()
     (accum-string-abandon! accum-str config)
     (read-vector read-one #\( #\) in config #:length v)]
    [(#\[)
     (accum-string-abandon! accum-str config)
     (guard-legal
      (check-parameter read-square-bracket-as-paren config)
      (get-accum c)
      (read-vector read-one #\[ #\] in config #:length v))]
    [(#\{)
     (accum-string-abandon! accum-str config)
     (guard-legal
      (check-parameter read-curly-brace-as-paren config)
      (get-accum c)
      (read-vector read-one #\{ #\} in config #:length v))]
    [(#\= #\#)
     (when (or (read-config-for-syntax? config)
               (not (check-parameter read-accept-graph config)))
       (reader-error in config
                     "`#...~a` forms not ~a"
                     c
                     (if (read-config-for-syntax? config)
                         "enabled"
                         "allowed in `read-syntax` mode")))
     (case ec
       [(#\=)
        (define ph (make-placeholder 'placeholder))
        (define st (read-config-st config))
        (define ht (or (read-config-state-graph st)
                       (let ([ht (make-hasheqv)])
                         (set-read-config-state-graph! st ht)
                         ht)))
        (when (hash-ref ht v #f)
          (reader-error in config
                        "multiple ~a~a~a tags"
                        dispatch-c (accum-string-get! accum-str config) c))
        (hash-set! ht v ph)
        (define result-v (read-one in config))
        (when (eof-object? result-v)
          (reader-error in config
                        "expected an element for graph after `~a~a~a`, found end-of-file"
                        dispatch-c (accum-string-get! accum-str config) c))
        (accum-string-abandon! accum-str config)
        (placeholder-set! ph result-v)
        ph]
       [(#\#)
        (begin0
         (hash-ref 
          (or (read-config-state-graph (read-config-st config))
              #hash())
          v
          (lambda ()
            (reader-error in config
                          "no preceding `~a~a=` for `~a~a~a`"
                          dispatch-c v
                          dispatch-c (accum-string-get! accum-str config) c)))
         (accum-string-abandon! accum-str config))])]
    [else
     (reader-error in config
                   #:eof? (eof-object? c)
                   "bad syntax `~a`"
                   (get-accum c))]))
