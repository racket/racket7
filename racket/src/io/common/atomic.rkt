#lang racket/base
(require "../host/evt.rkt")

(provide atomically)

(define-syntax-rule (atomically e ...)
  (begin
    (start-atomic)
    (begin0
      (let () e ...)
      (end-atomic))))
