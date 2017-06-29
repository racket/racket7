#lang racket/base
(require "../host/evt.rkt")

(provide atomically
         non-atomically)

(define-syntax-rule (atomically e ...)
  (begin
    (start-atomic)
    (begin0
      (let () e ...)
      (end-atomic))))

(define-syntax-rule (non-atomically e ...)
  (begin
    (end-atomic)
    (begin0
      (let () e ...)
      (start-atomic))))
