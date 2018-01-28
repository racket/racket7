#lang racket/base

(provide hash-union)

(define (hash-union a b)
  (for/fold ([a a]) ([(k v) (in-hash b)])
    (hash-set a k v)))
