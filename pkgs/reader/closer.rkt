#lang racket/base
(require "parameter.rkt")

(provide char-closer?
         closer-name
         closer->opener
         opener-name
         dot-name
         all-openers-str)

(define (char-closer? ec config)
  (and (not (eof-object? ec))
       (or (char=? ec #\))
           (char=? ec #\])
           (char=? ec #\}))))

(define (closer-name c config)
  (format "`~a`" c))

(define (closer->opener c)
  (case c
    [(#\)) #\(]
    [(#\]) #\[]
    [(#\}) #\{]
    [else c]))

(define (opener-name c config)
  (format "`~a`" c))

(define (dot-name config)
  "`.`")

(define (all-openers-str config)
  (define p (opener-name #\( config))
  (define s (and (check-parameter read-square-bracket-as-paren config)
                 (opener-name #\[ config)))
  (define c (and (check-parameter read-curly-brace-as-paren config)
                 (opener-name #\{ config)))
  (cond
   [(and s c) (format "~a, ~a, or ~a" p s c)]
   [(or s c) (format "~a or ~a" p (or s c))]
   [else p]))
