#lang racket/base
(require "config.rkt")

(provide check-parameter)

(define unknown (gensym 'unknown))

(define (check-parameter param config)
  (define cache (read-config-parameter-cache config))
  (define v (hash-ref cache param unknown))
  (cond
   [(eq? v unknown)
    (define v (param))
    (hash-set! cache param v)
    v]
   [else v]))
