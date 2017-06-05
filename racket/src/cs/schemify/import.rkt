#lang racket/base
(provide (struct-out import)
         (struct-out import-group)

         import-group-lookup-ready?
         
         import-group-lookup
         import-lookup
         
         hash-ref-either)

(struct import (grp id ext-id))
(struct import-group ([knowns/thunk #:mutable] ; starts as a procedure to get table
                      [converter #:mutable])) ; converts table entries to `known`s

(define (import-group-knowns grp)
  (define knowns/thunk (import-group-knowns/thunk grp))
  (cond
   [(procedure? knowns/thunk)
    (define-values (knowns converter) (knowns/thunk))
    (define knowns-or-empty (or knowns (hasheq)))
    (set-import-group-knowns/thunk! grp knowns-or-empty)
    (set-import-group-converter! grp converter)
    knowns-or-empty]
   [else knowns/thunk]))

(define (import-group-lookup-ready? grp)
  (define knowns/thunk (import-group-knowns/thunk grp))
  (not (procedure? knowns/thunk)))

(define (import-group-lookup g id)
  (define v (hash-ref (import-group-knowns g) id #f))
  (if v
      (let ([converter (import-group-converter g)])
        (if converter
            (converter v)
            v))
      v))

(define (import-lookup im)
  (import-group-lookup (import-grp im) (import-ext-id im)))

(define (hash-ref-either knowns imports key)
  (or (hash-ref knowns key #f)
      (let ([im (hash-ref imports key #f)])
        (and im
             (import-lookup im)))))
