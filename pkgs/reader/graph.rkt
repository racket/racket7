#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "accum-string.rkt"
         "parameter.rkt"
         "error.rkt"
         "digit.rkt"
         "vector.rkt")

(provide read-vector-or-graph)

(define (read-vector-or-graph read-one dispatch-c init-c in config)
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
