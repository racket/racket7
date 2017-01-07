#lang racket/base

(provide char-delimiter?)

(define (char-delimiter? c config)
  (or (not (char? c))
      (char-whitespace? c)
      (char=? c #\()
      (char=? c #\))
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\')
      (char=? c #\`)
      (char=? c #\,)
      (char=? c #\;)
      (char=? c #\")))
