#lang racket/base
(require "engine.rkt")

(provide current-thread)

(define thread #f)
(define current-thread
  (case-lambda
    [() thread]
    [(v) (set! thread v)]))
