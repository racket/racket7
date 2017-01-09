#lang racket/base
(require "config.rkt")

(provide check-parameter
         override-parameter)

(define unknown (gensym 'unknown))

(define (check-parameter param config)
  (define cache (read-config-parameter-cache config))
  (define v (hash-ref (read-config-parameter-override config)
                      param
                      (hash-ref cache param unknown)))
  (cond
   [(eq? v unknown)
    (define v (param))
    (hash-set! cache param v)
    v]
   [else v]))

(define (override-parameter param config v)
  (struct-copy read-config config
               [parameter-override (hash-set
                                    (read-config-parameter-override config)
                                    param
                                    v)]))
