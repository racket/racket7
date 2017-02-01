#lang racket/base

(provide drive-letter?)

(define (drive-letter? c)
  (and (c . < . 128)
       (char-alphabetic? (integer->char c))))

