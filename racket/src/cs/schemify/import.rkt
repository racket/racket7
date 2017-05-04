#lang racket/base
(provide (struct-out import)
         (struct-out import-group)
         
         import-group-knowns
         import-lookup
         
         hash-ref-either)

(struct import (grp id ext-id))
(struct import-group ([knowns/thunk #:mutable])) ; starts as a procedure to get table

(define (import-group-knowns grp)
  (define knowns/thunk (import-group-knowns/thunk grp))
  (cond
   [(procedure? knowns/thunk)
    (define knowns (or (knowns/thunk)
                       (hasheq)))
    (set-import-group-knowns/thunk! grp knowns)
    knowns]
   [else knowns/thunk]))

(define (import-lookup im)
  (hash-ref (import-group-knowns (import-grp im)) (import-ext-id im) #f))

(define (hash-ref-either knowns imports key)
  (or (hash-ref knowns key #f)
      (let ([im (hash-ref imports key #f)])
        (and im
             (import-lookup im)))))
