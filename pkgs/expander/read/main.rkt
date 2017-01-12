#lang racket/base
(require "config.rkt"
         "special.rkt"
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
         "primitive-parameter.rkt"
         "special-comment.rkt"
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
         "box.rkt"
         "regexp.rkt"
         "extension.rkt")

(provide read
         read/recursive

         current-readtable
         make-readtable
         readtable?
         readtable-mapping

         (all-from-out "primitive-parameter.rkt")
         (all-from-out "special-comment.rkt"))

;; This is not the `read` to be exposed from `racket/base`, but a
;; general entry to point implement `read` and variants like
;; `read-syntax` and `read/recursive`. To support syntax objects, the
;; caller should provide the `dynamic-require`, `read-compiled`,
;; `module-declared?`, and `corece` functions, even when implementing
;; a plain `read`, since those might be needed by a
;; `read-syntax/recursive`.
(define (read in
              #:wrap [wrap #f]
              #:init-c [init-c #f]
              #:readtable [readtable (current-readtable)]
              #:next-readtable [next-readtable readtable]
              #:recursive? [recursive? #f]
              #:local-graph? [local-graph? #f] ; ignored unless `recursive?`
              #:source [source #f]
              #:for-syntax? [for-syntax? #f]
              #:read-compiled [read-compiled #f]       ; see "config.rkt"
              #:dynamic-require [dynamic-require #f]   ; see "config.rkt"
              #:module-declared? [module-declared? #f] ; see "config.rkt"
              #:coerce [coerce #f]                     ; see "config.rkt"
              #:keep-special-comment? [keep-special-comment? recursive?])
  (define config
    (cond
     [(and recursive?
           (current-read-config))
      => (lambda (config)
           (read-config-update config
                               #:for-syntax? for-syntax?
                               #:wrap wrap
                               #:readtable readtable
                               #:next-readtable (read-config-readtable config)
                               #:reset-graph? local-graph?))]
     [else
      (make-read-config #:readtable readtable
                        #:next-readtable next-readtable
                        #:source source
                        #:for-syntax? for-syntax?
                        #:wrap wrap
                        #:read-compiled read-compiled
                        #:dynamic-require dynamic-require
                        #:module-declared? module-declared?
                        #:coerce coerce)]))
  (define v (read-one/readtable-set init-c in config
                                    #:keep-special-comment? keep-special-comment?))
  (cond
   [(and (or (not recursive?) local-graph?)
         (read-config-state-graph (read-config-st config)))
    (make-reader-graph v)]
   [(and recursive? (not (eof-object? v)))
    (make-placeholder v)]
   [else v]))

;; ----------------------------------------
;; The top-level reading layer that takes care of parsing into
;; `#%cdot`. The `read-one` function is used for recursive reads, in
;; which case the "next" readtable may need to be installed, while
;; `read-one/readtable-set` uses the readtable setting as-is.

(define (read-one in config)
  (read-one/readtable-set #f in (next-readtable config)))

(define (read-one/readtable-set init-c in config
                                #:keep-special-comment? [keep-special-comment? #f])
  (cond
   [(not (check-parameter read-cdot config))
    ;; No parsing of `.` as `#%dot`
    (read-undotted/readtable-set init-c in config #:keep-special-comment? keep-special-comment?)]
   [(check-parameter read-cdot config)
    ;; Look for `<something> . <something>`
    (define-values (line col pos) (port-next-location in))
    (define v (read-undotted/readtable-set init-c in config #:keep-special-comment? keep-special-comment?))
    (cond
     [(special-comment? v) v]
     [else
      (let loop ([v v])
        (define c (peek-char/special in config))
        (define ec (effective-char c config))
        (cond
         [(not (char? ec))
          (if (special? v)
              (special-value v)
              v)]
         [(char-whitespace? ec)
          (consume-char in c)
          (loop v)]
         [(char=? ec #\.)
          (define-values (dot-line dot-col dot-pos) (port-next-location in))
          (consume-char in c)
          (define cdot (wrap '#%dot in (reading-at config dot-line dot-col dot-pos) #\.))
          (define post-v (read-undotted in config))
          (loop (wrap (list '#%dot v post-v) in (reading-at config line col pos) #\.))]
         [else v]))])]))

;; ----------------------------------------
;; The top-level reading layer within `#%cdot` handling --- which is
;; the reader's main dispatch layer.

(define (read-undotted in config)
  (read-undotted/readtable-set #f in (next-readtable config)))

(define (read-undotted/readtable-set init-c in config
                                     #:keep-special-comment? [keep-special-comment? #f])
  (skip-whitespace-and-comments! read-one in config
                                 #:keep-special-comment? keep-special-comment?)
  (define-values (line col pos) (port-next-location in))
  (define c (or init-c (read-char/special in config)))
  (cond
   [(eof-object? c) eof]
   [(not (char? c))
    (define v (special-value c))
    (cond
     [(special-comment? v)
      (if keep-special-comment?
          v
          (read-undotted/readtable-set #f in config))]
     [else v])]
   [(readtable-handler config c)
    => (lambda (handler)
         (readtable-apply handler c in config line col pos))]
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
        (define c2 (peek-char/special in config))
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
       (read-number-or-symbol c in r-config #:mode 'symbol)]
      [else
       (read-number-or-symbol c in r-config)])]))

;; Dispatch on `#` character
(define (read-dispatch dispatch-c in config)
  (define c (read-char/special in config))
  (cond
   [(eof-object? c)
    (reader-error in config #:eof? #t "bad syntax `~a`" dispatch-c)]
   [(not (char? c))
    (reader-error in config "bad syntax `~a`" dispatch-c)]
   [(readtable-dispatch-handler config c)
    => (lambda (handler)
         (define line (read-config-line config))
         (define col (read-config-col config))
         (define pos (read-config-pos config))
         (readtable-apply handler c in config line col pos))]
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
       (define c2 (peek-char/special in config))
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
        [(eqv? #\< (peek-char/special in config))
         (consume-char in #\<)
         (read-here-string in config)]
        [else
         (reader-error in config "bad syntax `~a<`" dispatch-c)])]
      [(#\%)
       (read-number-or-symbol c in config #:extra-prefix dispatch-c #:mode 'symbol)]
      [(#\:)
       (read-number-or-symbol #f in config #:mode 'keyword)]
      [(#\t #\T)
       (define c2 (peek-char/special in config))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [else (read-delimited-constant c '(#\r #\u #\e) #t in config)])]
      [(#\f #\F)
       (define c2 (peek-char/special in config))
       (cond
        [(char-delimiter? c2 config) (wrap #f in config c)]
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
      [(#\c #\C)
       (define c2 (read-char/special in config))
       (case c2
         [(#\s #\S) (read-one in (override-parameter read-case-sensitive config #t))]
         [(#\i #\I) (read-one in (override-parameter read-case-sensitive config #f))]
         [else
          (reader-error in config
                        "expected `s', `S`, `i', or `I` after `~a~a`"
                        dispatch-c c)])]
      [(#\h #\H) (read-hash read-one dispatch-c c in config)]
      [(#\r)
       ;; Maybe regexp or `#reader`
       (define accum-str (accum-string-init! config))
       (accum-string-add! accum-str dispatch-c)
       (accum-string-add! accum-str c)
       (define c2 (read-char/special in config))
       (when (char? c2) (accum-string-add! accum-str c2))
       (case c2
         [(#\x) (read-regexp c accum-str in config)]
         [(#\e) (read-extension-reader read-one read-undotted dispatch-c in config)]
         [else
          (bad-syntax-error in config
                                 #:eof? (eof-object? c2)
                                 (accum-string-get! accum-str config))])]
      [(#\p)
       ;; Maybe pregexp
       (define accum-str (accum-string-init! config))
       (accum-string-add! accum-str dispatch-c)
       (accum-string-add! accum-str c)
       (define c2 (read-char/special in config))
       (when (char? c2) (accum-string-add! accum-str c2))
       (case c2
         [(#\x) (read-regexp c accum-str in config)]
         [else (bad-syntax-error in config #:eof? (eof-object? c2)
                                 (accum-string-get! accum-str config))])]
      [(#\l)
       ;; Maybe `#lang`
       (read-extension-lang read-undotted dispatch-c in config)]
      [(#\!)
       ;; Maybe `#lang`
       (read-extension-#! read-undotted dispatch-c in config)]
      [(#\~)
       ;; Compiled code
       (cond
        [(check-parameter read-accept-compiled config)
         (wrap ((read-config-read-compiled config) in) in config c)]
        [else
         (reader-error in config
                       "`~a~~` compiled expressions not enabled"
                       dispatch-c)])]
      [else
       (reader-error in config "bad syntax `~a~a`" dispatch-c c)])]))
