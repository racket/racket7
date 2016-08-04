#lang racket/base
(require "linklet.rkt")

;; Get host notion of syntax for `read-syntax`, etc. Bounce the
;; refereneces to thesed operations through `primitive-table`, so that
;; the bootstrapping process doesn't complain about using them.

;; Note that if the host has a `compile-linklet`, these syntax objects
;; may not be compatible with it. See "correlate-syntax.rkt" for
;; `compile-linklet`-compatible variants.

(define kernel-primitive-table (primitive-table '#%kernel))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (hash-ref kernel-primitive-table 'id))
    ...))

(bounce read-syntax read-syntax/recursive make-readtable
        datum->syntax syntax->datum syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
