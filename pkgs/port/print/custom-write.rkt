#lang racket/base

(provide prop:custom-write
         custom-write?
         custom-write-ref)

(define-values (prop:custom-write custom-write? custom-write-ref)
  (make-struct-type-property 'custom-write
                             (lambda (v info)
                               (unless (and (procedure? v)
                                            (procedure-arity-includes? v 3))
                                 (raise-argument-error
                                  'guard-for-prop:custom-write
                                  "(procedure-arity-includes?/c 3)"
                                  v))
                               v)))
