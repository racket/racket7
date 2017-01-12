#lang racket/base
(require "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "error.rkt"
         "accum-string.rkt"
         "indentation.rkt"
         "closer.rkt"
         "parameter.rkt"
         "wrap.rkt"
         "sequence.rkt")

(provide read-hash)

;; `#` and `h` or `H` have been read
(define (read-hash read-one dispatch-c init-c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str dispatch-c)
  (accum-string-add! accum-str init-c)
  
  (define (get-next! expect-c expect-alt-c)
    (define c (read-char/special in config))
    (unless (or (eqv? c expect-c) (eqv? c expect-alt-c))
      (reader-error in config
                    #:eof? (eof-object? c)
                    "expected `~a` after `~a`"
                    expect-c (accum-string-get! accum-str config)))
    (accum-string-add! accum-str c))
  
  (get-next! #\a #\A)
  (get-next! #\s #\S)
  (get-next! #\h #\H)
  
  (define-values (content opener mode)
    (let loop ([mode 'equal])
      (define c (read-char/special in config))
      (define ec (effective-char c config))
      (case ec
        [(#\()
         (define read-one-key+value (make-read-one-key+value read-one c #\)))
         (values (read-unwrapped-sequence read-one-key+value c #\( #\) in config #:dot-mode #f)
                 ec
                 mode)]
        [(#\[)
         (cond
          [(check-parameter read-square-bracket-as-paren config)
           (define read-one-key+value (make-read-one-key+value read-one c #\]))
           (values (read-unwrapped-sequence read-one-key+value c #\[ #\] in config #:dot-mode #f)
                   ec
                   mode)]
          [else
           (reader-error in config "illegal use of `~a`" c)])]
        [(#\{)
         (cond
          [(check-parameter read-curly-brace-as-paren config)
           (define read-one-key+value (make-read-one-key+value read-one c #\}))
           (values (read-unwrapped-sequence read-one-key+value c #\{ #\} in config #:dot-mode #f)
                   ec
                   mode)]
          [else
           (reader-error in config "illegal use of `~a`" c)])]
        [(#\e #\E)
         (accum-string-add! accum-str c)
         (get-next! #\q #\Q)
         (loop 'eq)]
        [(#\v #\V)
         (accum-string-add! accum-str c)
         (if (eq? mode 'eq)
             (loop 'eqv)
             (reader-error in config
                           "bad syntax `~a`"
                           (accum-string-get! accum-str config)))]
        [else
         (when (char? c)
           (accum-string-add! accum-str c))
         (reader-error in config
                       #:eof? (eof-object? c)
                       "bad syntax `~a`"
                       (accum-string-get! accum-str config))])))
  
  (define graph? (and (read-config-state-graph
                       (read-config-st config))
                      #t))
  
  (wrap (case mode
          [(equal)
           (if graph?
               (make-hash-placeholder content)
               (make-immutable-hash content))]
          [(eq)
           (if graph?
               (make-hasheq-placeholder content)
               (make-immutable-hasheq content))]
          [(eqv)
           (if graph?
               (make-hasheqv-placeholder content)
               (make-immutable-hasheqv content))])
        in
        config
        opener))

;; ----------------------------------------

(define ((make-read-one-key+value read-one overall-opener-c overall-closer-ec) in config)
  (define c (skip-whitespace-and-comments! read-one in config))
  (define-values (open-line open-col open-pos) (port-next-location in))
  (consume-char in c)
  (define ec (effective-char c config))
  
  (define closer
    (case ec
      [(#\() #\)]
      [(#\[) (and (check-parameter read-square-bracket-as-paren config)
                  #\])]
      [(#\{) (and (check-parameter read-curly-brace-as-paren config)
                  #\})]
      [else #f]))
  
  (unless closer
    (cond
     [(eof-object? c)
      (reader-error in (reading-at config open-line open-col open-pos)
                    #:eof? (eof-object? c)
                    "expected ~a to close `~a`"
                    (closer-name overall-closer-ec config) overall-opener-c)]
     [(char-closer? ec config)
      (reader-error in (reading-at config open-line open-col open-pos)
                    "~a"
                    (indentation-unexpected-closer-message ec c config))]
     [else
      (reader-error in (reading-at config open-line open-col open-pos)
                    "expected ~a to start a hash pair"
                    (all-openers-str config))]))
  
  (define k (read-one in (disable-wrapping config)))
  
  (define dot-c (skip-whitespace-and-comments! read-one in config))
  (define-values (dot-line dot-col dot-pos) (port-next-location in))
  (consume-char in dot-c)
  (define dot-ec (effective-char dot-c config))

  (unless (eqv? dot-ec #\.)
    (reader-error in (reading-at config dot-line dot-col dot-pos)
                  #:eof? (eof-object? dot-c)
                  "expected ~a and value for hash"
                  (dot-name config)))
  
  (define v (read-one in config))
  
  (define closer-c (skip-whitespace-and-comments! read-one in config))
  (define-values (closer-line closer-col closer-pos) (port-next-location in))
  (consume-char in closer-c)
  (define closer-ec (effective-char closer-c config))
  
  (unless (eqv? closer-c closer)
    (reader-error in (reading-at config closer-line closer-col closer-pos)
                  #:eof? (eof-object? closer-c)
                  "expected ~a after value within a hash"
                  (closer-name closer config)))
  
  (cons k v))
