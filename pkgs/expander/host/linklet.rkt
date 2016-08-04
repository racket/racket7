#lang racket/base
(require (prefix-in host: '#%linklet)
         "../run/linklet-operation.rkt")

;; We use only `primitive-table` directly, so that's the only function
;; needed for bootstrapping --- and generally so we can replace the
;; linklet implementation for bootstrapping. See also
;; "../run/linklet.rkt".

(define linklet-primitive-table (or
                                 ;; As a hook for bootstrapping, check for a
                                 ;; replacement of the primitive '#%linklet
                                 ;; module:
                                 (host:primitive-table '#%bootstrap-linklet)
                                 (host:primitive-table '#%linklet)))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (define id (hash-ref linklet-primitive-table 'id #f))
    ...))

(linklet-operations=> bounce)

(unless variable-reference-constant?
  (error "broken '#%linklet primitive table; maybe you need to use \"bootstrap-run.rkt\""))
