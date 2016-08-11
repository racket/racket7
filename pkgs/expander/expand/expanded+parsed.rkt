#lang racket/base
(require "../syntax/syntax.rkt"
         "parsed.rkt")

(provide (struct-out expanded+parsed)
         (struct-out semi-parsed-define-values)
         (struct-out semi-parsed-begin-for-syntax)
         parsed-only
         syntax-only)

;; When expanding a module, we may need to compile and instantiate it,
;; too (as or for submodules), so keep both expanded and compiled
;; variants of a form together:
(struct expanded+parsed (s parsed))

;; A `define-values` or `begin-for-syntax-form` is in limbo though
;; some passes.
(struct semi-parsed-define-values (s syms ids rhs))
(struct semi-parsed-begin-for-syntax (s body))

(define (parsed-only l)
  (for/list ([i (in-list l)]
             #:when (or (parsed? i)
                        (expanded+parsed? i)
                        (semi-parsed-begin-for-syntax? i)))
    (cond
     [(expanded+parsed? i)
      (expanded+parsed-parsed i)]
     [(semi-parsed-begin-for-syntax? i)
      (parsed-begin-for-syntax (semi-parsed-begin-for-syntax-s i)
                               (parsed-only (semi-parsed-begin-for-syntax-body i)))]
     [else i])))

(define (syntax-only l)
  (for/list ([i (in-list l)]
             #:when (or (syntax? i)
                        (expanded+parsed? i)))
    (if (expanded+parsed? i)
        (expanded+parsed-s i)
        i)))
