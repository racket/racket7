#lang racket/base
(require "../common/set.rkt"
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/memo.rkt"
         "syntax.rkt"
         "property.rkt"
         "scope.rkt"
         "../common/phase.rkt"
         "full-binding.rkt"
         "module-binding.rkt"
         "local-binding.rkt"
         "../expand/rename-trans.rkt"
         "../common/module-path.rkt")

(provide
 binding-frame-id
 binding-free=id
 (all-from-out "module-binding.rkt")
 (all-from-out "local-binding.rkt")

 free-identifier=?
 same-binding?
 same-binding-nominals?
 identifier-binding
 identifier-binding-symbol
 
 maybe-install-free=id!
 binding-set-free=id

 resolve+shift
 syntax-module-path-index-shift

 apply-syntax-shifts
 syntax-apply-shifts
 binding-module-path-index-shift
 syntax-transfer-shifts
 
 syntax-source-module
 identifier-prune-to-source-module)

;; ----------------------------------------

(define (free-identifier=? a b a-phase b-phase)
  (define ab (resolve+shift a a-phase #:unbound-sym? #t))
  (define bb (resolve+shift b b-phase #:unbound-sym? #t))
  (cond
   [(or (symbol? ab) (symbol? bb))
    (eq? ab bb)]
   [else
    (same-binding? ab bb)]))

(define (same-binding? ab bb)
  (cond
   [(module-binding? ab)
    (and (module-binding? bb)
         (eq? (module-binding-sym ab)
              (module-binding-sym bb))
         (eqv? (module-binding-phase ab)
               (module-binding-phase bb))
         (eq? (module-path-index-resolve (module-binding-module ab))
              (module-path-index-resolve (module-binding-module bb))))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]
   [else (error "bad binding" ab)]))

;; Check whether two bindings that are `same-binding?` also provide
;; the same nominal info (i.e., claim to be required through the same
;; immediate path):
(define (same-binding-nominals? ab bb)
  (and (eq? (module-path-index-resolve (module-binding-nominal-module ab))
            (module-path-index-resolve (module-binding-nominal-module bb)))
       (eqv? (module-binding-nominal-require-phase ab)
             (module-binding-nominal-require-phase bb))
       (eqv? (module-binding-nominal-sym ab)
             (module-binding-nominal-sym bb))))

(define (identifier-binding-symbol id phase)
  (define b (resolve+shift id phase #:unbound-sym? #t))
  (cond
   [(symbol? b) b]
   [(module-binding? b)
    (module-binding-sym b)]
   [(local-binding? b)
    (local-binding-key b)]
   [else (syntax-e id)]))

(define (identifier-binding id phase)
  (define b (resolve+shift id phase))
  (cond
   [(module-binding? b)
    (if (top-level-module-path-index? (module-binding-module b))
        #f ; => top level, indistinguishable from unbound in this legacy API
        (list (module-binding-module b)
              (module-binding-sym b)
              (module-binding-nominal-module b)
              (module-binding-nominal-sym b)
              (module-binding-phase b)
              (module-binding-nominal-require-phase b)
              (module-binding-nominal-phase b)))]
   [(local-binding? b)
    'lexical]
   [else #f]))

;; ----------------------------------------

(define (maybe-install-free=id! val id phase)
  (when (rename-transformer? val)
    (define free=id (rename-transformer-target val))
    (unless (syntax-property free=id 'not-free-identifier=?)
      (define b (resolve+shift id phase #:exactly? #t #:immediate? #t))
      (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) (binding-set-free=id b free=id)))))

;; Helper to add a `free-identifier=?` equivance to a binding
(define (binding-set-free=id b free=id)
  (cond
   [(module-binding? b) (module-binding-update b #:free=id free=id)]
   [(local-binding? b) (local-binding-update b #:free=id free=id)]
   [else (error "bad binding for free=id:" b)]))

; ----------------------------------------

;; Adjust `s` (recursively) so that if `resolve+shift` would
;; report `form-mpi`, the same operation on the result will
;; report `to-mpi`. A non-#f `inspector` is provided when shifting
;; syntax literals in a module to match the module's declaration-time
;; inspector.
(define (syntax-module-path-index-shift s from-mpi to-mpi [inspector #f])
  (cond
   [(eq? from-mpi to-mpi)
    (if inspector
        (syntax-set-inspector s inspector)
        s)]
   [else
    (let ([shift (cons from-mpi to-mpi)])
      (define-memo-lite (add-shift shifts)
        (cons shift shifts))
      (syntax-map s
                  (lambda (tail? d) d)
                  (lambda (s d)
                    (struct-copy syntax s
                                 [content d]
                                 [mpi-shifts (add-shift (syntax-mpi-shifts s))]
                                 [inspector (or (syntax-inspector s)
                                                inspector)]))
                  syntax-e/no-taint))]))

;; Use `resolve` instead of `resolve+shift` when the module of a
;; module binding is relevant or when `free-identifier=?` equivalences
;; (as installed by a binding to a rename transfomer) are relevant;
;; module path index shifts attached to `s` are taken into account in
;; the result
(define (resolve+shift s phase
                       #:ambiguous-value [ambiguous-value #f]
                       #:exactly? [exactly? #f]
                       #:immediate? [immediate? exactly?]
                       #:unbound-sym? [unbound-sym? #f]
                       ;; For resolving bulk bindings in `free-identifier=?` chains:
                       #:extra-shifts [extra-shifts null])
  (define immediate-b (resolve s phase
                               #:ambiguous-value ambiguous-value
                               #:exactly? exactly?
                               #:extra-shifts extra-shifts))
  (define b (if (and immediate-b
                     (not immediate?)
                     (binding-free=id immediate-b))
                (resolve+shift (binding-free=id immediate-b) phase
                               #:extra-shifts (append extra-shifts (syntax-mpi-shifts s))
                               #:ambiguous-value ambiguous-value
                               #:exactly? exactly?
                               #:unbound-sym? unbound-sym?)
                immediate-b))
  (cond
   [(module-binding? b)
    (define mpi-shifts (syntax-mpi-shifts s))
    (cond
     [(null? mpi-shifts)
      b]
     [else
      (define mod (module-binding-module b))
      (define shifted-mod (apply-syntax-shifts mod mpi-shifts))
      (define nominal-mod (module-binding-nominal-module b))
      (define shifted-nominal-mod (if (eq? mod nominal-mod)
                                      shifted-mod
                                      (apply-syntax-shifts nominal-mod mpi-shifts)))
      (if (and (eq? mod shifted-mod)
               (eq? nominal-mod shifted-nominal-mod)
               (not (binding-free=id b))
               (null? (module-binding-extra-nominal-bindings b)))
          b
          (module-binding-update b
                                 #:module shifted-mod
                                 #:nominal-module shifted-nominal-mod
                                 #:free=id (and (binding-free=id b)
                                                (syntax-transfer-shifts (binding-free=id b) s))
                                 #:extra-nominal-bindings
                                 (for/list ([b (in-list (module-binding-extra-nominal-bindings b))])
                                   (apply-syntax-shifts-to-binding b mpi-shifts))))])]
   [(and (not b) unbound-sym?)
    (syntax-e s)]
   [else b]))

;; Apply accumulated module path index shifts
(define (apply-syntax-shifts mpi shifts)
  (cond
   [(null? shifts) mpi]
   [else
    (define shifted-mpi (apply-syntax-shifts mpi (cdr shifts)))
    (module-path-index-shift shifted-mpi (caar shifts) (cdar shifts))]))

;; Apply accumulated module path index shifts to a module binding
(define (apply-syntax-shifts-to-binding b shifts)
  (cond
   [(null? shifts) b]
   [else
    (define shifted-b (apply-syntax-shifts-to-binding b (cdr shifts)))
    (binding-module-path-index-shift shifted-b (caar shifts) (cdar shifts))]))

;; Apply a syntax object's shifts to a given module path index
(define (syntax-apply-shifts s mpi)
  (apply-syntax-shifts mpi (syntax-mpi-shifts s)))

;; Apply a single shift to a single binding
(define (binding-module-path-index-shift b from-mpi to-mpi)
  (cond
   [(module-binding? b)
    (module-binding-update b
                           #:module (module-path-index-shift (module-binding-module b)
                                                             from-mpi
                                                             to-mpi)
                           #:nominal-module (module-path-index-shift (module-binding-nominal-module b)
                                                                     from-mpi
                                                                     to-mpi)
                           #:extra-nominal-bindings (for/list ([b (in-list (module-binding-extra-nominal-bindings b))])
                                                      (binding-module-path-index-shift b from-mpi to-mpi)))]
   [else b]))

(define (syntax-transfer-shifts to-s from-s [inspector #f])
  (define shifts (syntax-mpi-shifts from-s))
  (cond
   [(and (null? shifts) inspector)
    (syntax-set-inspector to-s inspector)]
   [else
    (for/fold ([s to-s]) ([shift (in-list (reverse shifts))]
                          [i (in-naturals)])
      (syntax-module-path-index-shift s (car shift) (cdr shift) (and (zero? i) inspector)))]))

(define (syntax-set-inspector s insp)
  (syntax-map s
              (lambda (tail? d) d)
              (lambda (s d)
                (struct-copy syntax s
                             [content d]
                             [inspector (or (syntax-inspector s)
                                            insp)]))
              syntax-content))

;; ----------------------------------------

(define (syntax-source-module s [source? #f])
  (unless (syntax? s)
    (raise-argument-error 'syntax-track-origin "syntax?" s))
  ;; The concept of a source module is a hack. We try to infer
  ;; a module from the module-path-index shifts that are attached
  ;; to the syntax object by starting with the initial shift and
  ;; working our way back.
  (for/or ([shift (in-list (reverse (syntax-mpi-shifts s)))])
    (define from-mpi (car shift))
    (define-values (path base) (module-path-index-split from-mpi))
    (and (not path)
         (module-path-index-resolved from-mpi)
         (apply-syntax-shifts from-mpi (syntax-mpi-shifts s)))))

(define (identifier-prune-to-source-module id)
  (unless (identifier? id)
    (raise-argument-error 'identifier-prune-to-source-module "identifier?" id))
  (struct-copy syntax (datum->syntax #f (syntax-e id) id id)
               [mpi-shifts (syntax-mpi-shifts id)]))
