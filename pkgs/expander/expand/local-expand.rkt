#lang racket/base
(require "../common/struct-star.rkt"
         "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../namespace/core.rkt"
         "context.rkt"
         "main.rkt"
         "syntax-local.rkt"
         "definition-context.rkt"
         "already-expanded.rkt"
         "lift-key.rkt"
         "log.rkt"
         "../common/performance.rkt")

(provide local-expand
         local-expand/capture-lifts
         local-transformer-expand
         local-transformer-expand/capture-lifts
         syntax-local-expand-expression)

(define (local-expand s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs))

(define (local-expand/capture-lifts s context stop-ids [intdefs #f] [lift-key (generate-lift-key)])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:capture-lifts? #t
                   #:lift-key lift-key))

(define (local-transformer-expand s context stop-ids [intdefs #f])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:as-transformer? #t))

(define (local-transformer-expand/capture-lifts s context stop-ids [intdefs #f] [lift-key (generate-lift-key)])
  (do-local-expand 'local-expand s context stop-ids intdefs
                   #:as-transformer? #t
                   #:capture-lifts? #t
                   #:lift-key lift-key))

(define (syntax-local-expand-expression s)
  (define exp-s (do-local-expand 'syntax-local-expand-expression s 'expression null #f
                                 #:skip-log-exit? #t))
  (define ctx (get-current-expand-context))
  ;; Move introduction scope from the already-expanded syntax object to
  ;; its wrapper. The expander will later check that the wrapper ends up
  ;; with an empty set of scopes, and then the already-expanded inside has
  ;; the scopes suitably flipped
  (define ae (already-expanded
              (flip-introduction-scopes exp-s ctx)
              (expand-context-binding-layer ctx)))
  (log-expand ctx 'opaque-expr ae)
  (log-expand ctx 'exit-local exp-s)
  (values exp-s (flip-introduction-scopes (datum->syntax #f ae) ctx)))

;; ----------------------------------------

(define (do-local-expand who s context stop-ids [intdefs #f]
                         #:capture-lifts? [capture-lifts? #f]
                         #:as-transformer? [as-transformer? #f]
                         #:lift-key [lift-key (and (or capture-lifts?
                                                       as-transformer?)
                                                   (generate-lift-key))]
                         #:skip-log-exit? [skip-log-exit? #f])
  (performance-region
   ['expand 'local-expand]
   
   (unless (syntax? s)
     (raise-argument-error who "syntax?" s))
   (unless (or (list? context)
               (memq context (if as-transformer?
                                 '(expression top-level)
                                 '(expression top-level module module-begin))))
     (raise-argument-error who
                           (if as-transformer?
                               "(or/c 'expression 'top-level list?)"
                               "(or/c 'expression 'top-level 'module 'module-begin list?)")
                           context))
   (unless (or (not stop-ids)
               (and (list? stop-ids)
                    (andmap identifier? stop-ids)))
     (raise-argument-error who "(or/c (listof identifier?) #f)" stop-ids))
   (unless (or (not intdefs)
               (internal-definition-context? intdefs)
               (and (list? intdefs) (andmap internal-definition-context? intdefs)))
     (raise-argument-error who
                           "(or/c #f internal-definitionc-context? (listof internal-definitionc-context?))" 
                           intdefs))

   (define ctx (get-current-expand-context who))
   (define phase (if as-transformer?
                     (add1 (expand-context-phase ctx))
                     (expand-context-phase ctx)))
   (define local-ctx (make-local-expand-context ctx
                                                     #:context context
                                                     #:phase phase
                                                     #:intdefs intdefs
                                                     #:stop-ids stop-ids))

   (define input-s (add-intdef-scopes (flip-introduction-scopes s ctx) intdefs))

   (log-expand local-ctx 'enter-local)
   (when as-transformer? (log-expand local-ctx 'phase-up))
   (log-expand* local-ctx ['local-pre input-s] ['start-expand])
   
   (define output-s (cond
                     [(and as-transformer? capture-lifts?)
                      (expand-transformer input-s local-ctx
                                          #:context context
                                          #:expand-lifts? #f
                                          #:begin-form? #t
                                          #:lift-key lift-key
                                          #:always-wrap? #t)]
                     [as-transformer?
                      (expand-transformer input-s local-ctx
                                          #:context context
                                          #:expand-lifts? #f
                                          #:begin-form? (eq? 'top-level context)
                                          #:lift-key lift-key)]
                     [capture-lifts?
                      (expand/capture-lifts input-s local-ctx
                                            #:begin-form? #t
                                            #:lift-key lift-key
                                            #:always-wrap? #t)]
                     [else
                      (expand input-s local-ctx)]))
   
   (log-expand local-ctx 'local-post output-s)
   
   (define result-s (flip-introduction-scopes output-s ctx))
   
   (unless skip-log-exit?
     (log-expand local-ctx 'exit-local result-s))
   
   result-s))
