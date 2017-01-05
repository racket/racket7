#lang racket/base

(provide char-delimiter?)

(define (char-delimiter? c config)
  (or (eof-object? c)
      (char-whitespace? c)
      (char=? c #\()
      (char=? c #\))
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\,)
      (char=? c #\")))
