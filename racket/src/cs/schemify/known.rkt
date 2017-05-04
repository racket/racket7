#lang racket/base

;; Ths module uses `#:omit-define-syntaxes` and doesn't use
;; `struct-out` so that none of the exports are syntax bindings.

(provide known-constant known-constant?
         known-unknown known-unknown?
         known-procedure known-procedure?
         known-struct-type known-struct-type? known-struct-type-type known-struct-type-field-count
         known-constructor known-constructor? known-constructor-type known-constructor-field-count
         known-predicate known-predicate? known-predicate-type
         known-accessor known-accessor? known-accessor-type
         known-mutator known-mutator? known-mutator-type
         known-struct-type-property/immediate-guard known-struct-type-property/immediate-guard?
         
         a-known-constant
         a-known-unknown
         a-known-procedure
         a-known-struct-type-property/immediate-guard)

(struct known-constant () #:prefab #:omit-define-syntaxes)
(struct known-unknown () #:prefab #:omit-define-syntaxes #:super struct:known-constant)
(struct known-procedure () #:prefab #:omit-define-syntaxes #:super struct:known-constant)
(struct known-struct-type (type field-count) #:prefab #:omit-define-syntaxes)
(struct known-constructor (type field-count) ; field count can be 'any
        #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-predicate (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-accessor (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-mutator (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-struct-type-property/immediate-guard () #:prefab #:omit-define-syntaxes)

(define a-known-constant (known-constant))
(define a-known-unknown (known-unknown))
(define a-known-procedure (known-procedure))
(define a-known-struct-type-property/immediate-guard (known-struct-type-property/immediate-guard))
