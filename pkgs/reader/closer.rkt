#lang racket/base

(provide char-closer?
         closer-name
         closer->opener
         opener-name)

(define (char-closer? c config)
  (and (not (eof-object? c))
       (or (char=? c #\))
           (char=? c #\])
           (char=? c #\}))))

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
