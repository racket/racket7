#lang racket/base
(require "../common/set.rkt")

(provide clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!)

(define cache (make-weak-box #f))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define c (weak-box-value cache))
     (when c
       (hash-remove! c sym))]
    [()
     (set! cache (make-weak-box (make-hasheq)))]))

(struct entry (scopes phase binding))

(define (resolve-cache-get sym phase scopes)
  (define c (weak-box-value cache))
  (and c
       (let ([v (hash-ref c sym #f)])
         (and v
              (eqv? phase (entry-phase v))
              (set=? scopes (entry-scopes v))
              (entry-binding v)))))

(define (resolve-cache-set! sym phase scopes b)
  (define c (weak-box-value cache))
  (cond
   [(not c)
    (clear-resolve-cache!)
    (resolve-cache-set! sym phase scopes b)]
   [else
    (hash-set! c sym (entry scopes phase b))]))
