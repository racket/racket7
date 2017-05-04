#lang racket/base
(require racket/include
         racket/unsafe/ops)

;; Run using the built-in C implementation:
'|C -----------------|
(include "regexp.scm")

;; Run the Racket implementation:
'|Racket -----------------|
(let ()
  (define-syntax-rule (linklet () ([int-id ext-id] ...) body ...)
    (begin
      (define ext-id #f) ...
      (let ()
        body ...
        (set! ext-id int-id) ...)))
  (include "../linklet/regexp.rktl")

  ;; Run using the Racket implementation:
  (include "regexp.scm"))
