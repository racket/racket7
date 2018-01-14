#lang racket/base
(provide (struct-out import)
         (struct-out import-group)

         import-group-lookup-ready?
         
         import-group-lookup
         import-lookup

         hash-ref-either
         
         make-add-import!)

(struct import (grp id int-id ext-id))
(struct import-group (index
                      [knowns/thunk #:mutable] ; starts as a procedure to get table
                      [converter #:mutable]    ; converts table entries to `known`s
                      [imports #:mutable]))    ; starts as declared imports, but inlining can grow

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

(define (make-add-import! imports grps get-import-knowns add-group!)
  (lambda (im ext-id index)
    ;; The `im` argument represents an import into the current
    ;; linklet. Let L be the linklet for that import. Map `ext-id` as
    ;; either defined in L (if `index` is #f) or imported into L from
    ;; its `index`th group to a new name in the current module,
    ;; potentially adding an import into the current module.

    ;; As a first step, just support a #f `index`
    (cond
      [index #f]
      [else
       (define grp (import-grp im))
       (or (for/or ([im (in-list (import-group-imports grp))])
             (and (eq? ext-id (import-ext-id im))
                  (import-int-id im)))
           (let ([id (gensym ext-id)]
                 [int-id (gensym ext-id)])
             (define im (import grp id int-id ext-id))
             (set-import-group-imports! grp (cons im (import-group-imports grp)))
             (hash-set! imports int-id im)
             int-id))])))
