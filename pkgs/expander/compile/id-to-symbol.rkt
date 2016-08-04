#lang racket/base
(require "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "context.rkt")

;; Extract the symbol genrated by the expander for a defined
;; identifier; we also double-check that he binding is consistent
;; with being a defined identifier

(provide def-ids-to-binding-syms)
  
(define (def-ids-to-binding-syms ids phase self cctx)
  (for/list ([id (in-list ids)])
    (define top-level-scope (compile-context-top-level-bind-scope cctx))
    (define tl-id (if top-level-scope
                      (add-scope id top-level-scope)
                      id))
    (define b (resolve+shift tl-id phase #:immediate? #t))
    (unless (and (module-binding? b)
                 (eq? self (module-binding-module b))
                 (eqv? phase (module-binding-phase b)))
      (error "bad binding for definition:" id
             self "vs." (and b (module-binding-module b))
             phase))
    (module-binding-sym b)))
