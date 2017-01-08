#lang racket/base

(provide char-closer?
         closer-name
         closer->opener
         opener-name
         dot-name)

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
