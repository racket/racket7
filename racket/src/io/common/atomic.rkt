#lang racket/base

(provide atomically)

(define-syntax-rule (atomically e ...)
  (let () e ...))
