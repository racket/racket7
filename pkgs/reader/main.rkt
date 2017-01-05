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
         "indentation.rkt")

(define (read-one in config)
  (skip-whitespace-and-comments! in config)
  (define-values (line col pos) (port-next-location in))
  (define c (read-char in))
  (cond
   [(eof-object? c) eof]
   [else
    ;; Map character via readtable:
    (define ec (effective-char c config))

    ;; Track indentation, unless it's a spurious closer:
    (when (not (char-closer? ec config))
      (track-indentation! config line col))
    (define r-config (reading-at config line col pos))
    
    ;; Dispatch on character:
    (case ec
      [(#\#)
       (read-dispatch in r-config)]
      [(#\')
       (read-quote 'quote "quoting '" in r-config)]
      [(#\`)
       (read-quote 'quasiquote "quasiquoting `" in r-config)]
      [(#\,)
       (define ec (peek-char in))
       (if (eqv? ec #\@)
           (begin
             (consume-char in ec)
             (read-quote 'unquote-splicing "unquoting ,@" in r-config))
           (read-quote 'unquote "unquoting ," in r-config))]
      [(#\()
       (define seq-config r-config)
       (wrap (read-unwrapped-sequence read-one #\( #\) in seq-config) in seq-config)]
      [(#\[)
       (define seq-config r-config)
       (wrap (read-unwrapped-sequence read-one #\[ #\] in seq-config) in seq-config)]
      [(#\{)
       (define seq-config r-config)
       (wrap (read-unwrapped-sequence read-one #\{ #\} in seq-config) in seq-config)]
      [(#\")
       (read-string in r-config)]
      [else
       (when (char-closer? ec config)
         (reader-error in r-config (indentation-unexpected-closer-message ec c r-config)))
       (read-number-or-symbol c in r-config)])]))

(define (read-dispatch in config)
  (define c (read-char in))
  (cond
   [(eof-object? c)
    (reader-error in config "bad syntax `#`" #:eof? #t)]
   [else
    (case c
      [(#\()
       (wrap (list->vector (read-unwrapped-sequence read-one #\( #\) in config #:dot-mode #f)) in config)]
      [(#\[)
       (wrap (list->vector (read-unwrapped-sequence read-one #\[ #\] in config #:dot-mode #f)) in config)]
      [(#\{)
       (wrap (list->vector (read-unwrapped-sequence read-one #\{ #\} in config #:dot-mode #f)) in config)]
      [(#\t #\T)
       (define c2 (peek-char in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config)]
        [else (read-delimited-constant c '(#\r #\u #\e) #t in config)])]
      [(#\f #\F)
       (define c2 (peek-char in))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config)]
        [else (read-delimited-constant c '(#\a #\l #\s #\e) #f in config)])]
      [else
       (reader-error in config "bad syntax `#~a`" c)])]))

(define (read-delimited-constant init-c chars val in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str init-c)
  (let loop ([chars chars])
    (define c (peek-char in))
    (cond
     [(or (eof-object? c)
          (char-delimiter? c config))
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
  (accum-string-abandon! accum-str config)
  (wrap val in config))

(define (read-number-or-symbol c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str c)
  (let loop ()
    (define c (peek-char in))
    (define ec (effective-char c config))
    (cond
     [(eof-object? ec) null]
     [(char-delimiter? ec config) null]
     [else
      (consume-char in c)
      (accum-string-add! accum-str c)
      (loop)]))
  (define str (accum-string-get! accum-str config))
  (wrap (or (string->number str)
            (string->symbol str))
        in
        config))

(define (read-string in config)
  (define accum-str (accum-string-init! config))
  (let loop ()
    (define c (read-char in))
    (define ec (effective-char c config))
    (cond
     [(eof-object? ec)
      (reader-error in config #:eof? #t "expected a closing `\"`")]
     [(char=? #\" ec)
      null]
     [else
      (accum-string-add! accum-str c)
      (loop)]))
  (define str (accum-string-get! accum-str config))
  (wrap str
        in
        config))

(define (read-quote sym desc in config)
  (define wrapped-sym (wrap sym in config))
  (define e (read-one in config))
  (when (eof-object? e)
    (reader-error in config #:eof? #t
                  "expected an element for ~a (found end-of-file)"))
  (wrap (list wrapped-sym e) in config))

;; ----------------------------------------

(read-one (open-input-string "(a b c)") (make-read-config))
(read-one (open-input-string "(a b . c)") (make-read-config))
(read-one (open-input-string "(b . a . c)") (make-read-config))
(read-one (open-input-string "(a 1.0 ; comment\n c)") (make-read-config))
(read-one (open-input-string "(a \"1.0\" c)") (make-read-config))
(read-one (open-input-string "'('a `b ,c ,@d ,@ e)") (make-read-config))
(read-one (open-input-string "(#t)") (make-read-config))
(read-one (open-input-string "(#TRUE)") (make-read-config))
(read-one (open-input-string "(#fAlSe)") (make-read-config))
(read-one (open-input-string "#(fAl Se)") (make-read-config))
(read-one (open-input-string "{fAl Se}") (make-read-config))
(with-handlers ([exn:fail:read? exn-message])
  (read-one (let ([p (open-input-string "{  fAl\n Se)")])
              (port-count-lines! p)
              p)
            (make-read-config)))

(define s (format "~a" (for/list ([i 100000]) i)))
(void (time (read-one (open-input-string s) (make-read-config))))
(void (time (read (open-input-string s))))
