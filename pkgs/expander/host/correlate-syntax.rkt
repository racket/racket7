#lang racket/base
(require '#%linklet)

;; Get host notion of syntax for `compile-linklet`.

;; This module looks like "syntax.rkt", but it uses `primitive-table`
;; from '#%linklet instead of from "linklet.rkt". When bootstrapping,
;; the underlying values are different.

(define kernel-primitive-table (primitive-table '#%kernel))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (hash-ref kernel-primitive-table 'id))
    ...))

(bounce datum->syntax syntax->datum syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
