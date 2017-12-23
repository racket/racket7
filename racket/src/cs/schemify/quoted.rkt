#lang racket/base
(require racket/extflonum)

(provide lift-quoted?)

;; Check whether a quoted value needs to be lifted to run-time construction
(define (lift-quoted? q)
  (cond
    [(impersonator? q) #t] ; i.e., strip impersonators when serializaing
    [(path? q) #t]
    [(regexp? q) #t]
    [(byte-regexp? q) #t]
    [(keyword? q) #t]
    [(hash? q) #t]
    [(string? q) #t]
    [(bytes? q) #t]
    [(pair? q) (or (lift-quoted? (car q))
                   (lift-quoted? (cdr q)))]
    [(vector? q) (for/or ([e (in-vector q)])
                   (lift-quoted? e))]
    [(box? q) (lift-quoted? (unbox q))]
    [(prefab-struct-key q) #t]
    [(extflonum? q) #t]
    [else #f]))
