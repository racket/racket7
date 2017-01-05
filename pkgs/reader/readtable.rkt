#lang racket/base
(require "config.rkt")

(provide effective-char)

;; Map EOF to EOF
(define (effective-char c config)
  c)
