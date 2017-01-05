#lang racket/base

(provide consume-char)

;; Consume a previously peek character. We could
;; double-check that the read character matches `c`
(define (consume-char in c)
  (read-char in)
  (void))
