#lang racket/base
(require "../error.rkt")

(provide parse-error)

(define (parse-error s pos config fmt . args)
  (apply regexp-error fmt args))
