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
         "sequence.rkt"
         "indentation.rkt"
         "parameter.rkt")

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
       (read-dispatch in r-config)]
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
       (reader-error in r-config (indentation-unexpected-closer-message ec c r-config))]
      [(#\[)
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (wrap (read-unwrapped-sequence read-one #\[ #\] in r-config #:shape-tag? #t) in r-config ec))]
      [(#\])
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (reader-error in r-config (indentation-unexpected-closer-message ec c r-config)))]
      [(#\{)
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (wrap (read-unwrapped-sequence read-one #\{ #\} in r-config #:shape-tag? #t) in r-config ec))]
      [(#\})
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (reader-error in r-config (indentation-unexpected-closer-message ec c r-config)))]
      [(#\")
       (read-string in r-config)]
      [else
       (read-number-or-symbol c in r-config)])]))

(define (read-dispatch in config)
  (define c (read-char-or-special in))
  (cond
   [(eof-object? c)
    (reader-error in config "bad syntax `#`" #:eof? #t)]
   [(not (char? c))
    (reader-error in config "bad syntax `#`")]
   [else
    (define-syntax-rule (guard-legal e body ...)
      (cond
       [e body ...]
       [else (reader-error in config "bad syntax `#~a`" c)]))
    (case c
      [(#\()
       (wrap (list->vector (read-unwrapped-sequence read-one #\( #\) in config #:dot-mode #f)) in config c)]
      [(#\[)
       (guard-legal
        (check-parameter read-square-bracket-as-paren config)
        (wrap (list->vector (read-unwrapped-sequence read-one #\[ #\] in config #:dot-mode #f)) in config c))]
      [(#\{)
       (guard-legal
        (check-parameter read-curly-brace-as-paren config)
        (wrap (list->vector (read-unwrapped-sequence read-one #\{ #\} in config #:dot-mode #f)) in config c))]
      [(#\t #\T)
       (define c2 (peek-char-or-special in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [else (read-delimited-constant c '(#\r #\u #\e) #t in config)])]
      [(#\f #\F)
       (define c2 (peek-char-or-special in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [else (read-delimited-constant c '(#\a #\l #\s #\e) #f in config)])]
      [else
       (reader-error in config "bad syntax `#~a`" c)])]))

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

(define (read-number-or-symbol c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str c)
  (let loop ()
    (define c (peek-char-or-special in))
    (define ec (effective-char c config))
    (cond
     [(char-delimiter? ec config) (void)]
     [else
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop)]))
  (define str (accum-string-get! accum-str config))
  (wrap (or (string->number str)
            (string->symbol str))
        in
        config
        str))

(define (read-string in config)
  (define accum-str (accum-string-init! config))
  (let loop ()
    (define c (read-char-or-special in))
    (define ec (effective-char c config))
    (cond
     [(eof-object? ec)
      (reader-error in config #:eof? #t "expected a closing `\"`")]
     [(not (char? ec))
      (reader-error in config "found non-character while reading a string")]
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

(define (read-quote sym desc c in config)
  (define wrapped-sym (wrap sym in config c))
  (define e (read-one in config))
  (when (eof-object? e)
    (reader-error in config #:eof? #t
                  "expected an element for ~a (found end-of-file)"))
  (wrap (list wrapped-sym e) in config #f))

;; ----------------------------------------

(read-one (open-input-string "(a b c)") (make-read-config))
(read-one (open-input-string "(a b . c)") (make-read-config))
(read-one (open-input-string "(b . a #| a |# . c)") (make-read-config))
(read-one (open-input-string "(a 1.0 ; comment\n c)") (make-read-config))
(read-one (open-input-string "(a \"1.0\" c)") (make-read-config))
(read-one (open-input-string "'('a `b ,c ,@d ,@ e)") (make-read-config))
(read-one (open-input-string "(#t)") (make-read-config))
(read-one (open-input-string "(#TRUE)") (make-read-config))
(read-one (open-input-string "(#fAlSe)") (make-read-config))
(read-one (open-input-string "#(fAl Se)") (make-read-config))
(read-one (open-input-string "{fAl Se}") (make-read-config))
(parameterize ([read-curly-brace-with-tag #t])
  (read-one (open-input-string "{fAl Se}") (make-read-config)))
(with-handlers ([exn:fail:read? exn-message])
  (read-one (let ([p (open-input-string "{  fAl\n Se)")])
              (port-count-lines! p)
              p)
            (make-read-config)))

(define s (format "~a" (for/list ([i 100000]) i)))
(void (time (read-one (open-input-string s) (make-read-config))))
(void (time (read (open-input-string s))))
