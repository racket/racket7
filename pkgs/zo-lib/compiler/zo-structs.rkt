#lang racket/base
(require racket/match
         racket/contract
         racket/list
         racket/set)

;; ----------------------------------------
;;  Structures to represent bytecode

(define-syntax-rule (define-form-struct* id id+par ([field-id field-contract . options] ...))
  (begin
    (define-struct id+par ([field-id . options] ...) #:prefab)
    (provide
     (contract-out
      [struct id ([field-id field-contract] ...)]))))

(define-struct zo () #:prefab)
(provide (struct-out zo))

(define-syntax define-form-struct
  (syntax-rules ()
    [(_ (id sup) . rest)
     (define-form-struct* id (id sup) . rest)]
    [(_ id . rest)
     (define-form-struct* id (id zo) . rest)]))

(define-form-struct function-shape ([arity procedure-arity?]
                                    [preserves-marks? boolean?]))

(define-form-struct struct-shape ())
(define-form-struct (constructor-shape struct-shape) ([arity exact-nonnegative-integer?]))
(define-form-struct (predicate-shape struct-shape) ())
(define-form-struct (accessor-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (mutator-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (struct-type-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (struct-other-shape struct-shape) ())

(define-form-struct form ())
(define-form-struct (expr form) ())

(define-form-struct (toplevel expr) ([depth exact-nonnegative-integer?] 
                                     [pos exact-nonnegative-integer?] 
                                     [const? boolean?] 
                                     [ready? boolean?]))  ; access binding via prefix array (which is on stack)

(define-form-struct (seq form) ([forms (listof (or/c form? any/c))])) ; `begin'

(define-form-struct (inline-variant form) ([direct expr?]
                                           [inline expr?]))

;; Definitions (top level or within module):
(define-form-struct (def-values form) ([ids (listof (or/c toplevel? symbol?))]
                                       [rhs (or/c expr? seq? inline-variant? any/c)]))

(define-form-struct (linkl form) ([name symbol?]
                                  [importss (listof (listof symbol?))]
                                  [import-shapess (listof (listof  (or/c #f 'constant 'fixed 
                                                                         function-shape? 
                                                                         struct-shape?)))]
                                  [exports (listof symbol?)]
                                  [internals (listof symbol?)]
                                  [lifts (listof symbol?)]
                                  [source-names (hash/c symbol? symbol?)]
                                  [body (listof (or/c form? any/c))]
                                  [max-let-depth exact-nonnegative-integer?]))

(define-form-struct linkl-directory ([table (hash/c (listof symbol?) linkl-bundle?)]))
(define-form-struct linkl-bundle    ([table (hash/c (or/c symbol? fixnum?)
                                                    any/c)])) ; can be anythin, but especially a linklet

(define-form-struct (lam expr) ([name (or/c symbol? vector? empty?)]
                                [flags (listof (or/c 'preserves-marks 'is-method 'single-result
                                                     'only-rest-arg-not-used 'sfs-clear-rest-args))]
                                [num-params exact-nonnegative-integer?]
                                [param-types (listof (or/c 'val 'ref 'flonum 'fixnum 'extflonum))]
                                [rest? boolean?]
                                [closure-map (vectorof exact-nonnegative-integer?)]
                                [closure-types (listof (or/c 'val/ref 'flonum 'fixnum 'extflonum))]
                                [toplevel-map (or/c #f (set/c exact-nonnegative-integer?))]
                                [max-let-depth exact-nonnegative-integer?]
                                [body (or/c expr? seq? any/c)])) ; `lambda'
(define-form-struct (closure expr) ([code lam?] [gen-id symbol?])) ; a static closure (nothing to close over)
(define-form-struct (case-lam expr) ([name (or/c symbol? vector? empty?)] [clauses (listof (or/c lam? closure?))]))

(define-form-struct (let-one expr) ([rhs (or/c expr? seq? any/c)]  ; pushes one value onto stack
                                    [body (or/c expr? seq? any/c)] 
                                    [type (or/c #f 'flonum 'fixnum 'extflonum)]
                                    [unused? boolean?]))
(define-form-struct (let-void expr) ([count exact-nonnegative-integer?] [boxes? boolean?] [body (or/c expr? seq? any/c)])) ; create new stack slots
(define-form-struct (install-value expr) ([count exact-nonnegative-integer?] 
                                          [pos exact-nonnegative-integer?] 
                                          [boxes? boolean?] 
                                          [rhs (or/c expr? seq? any/c)] 
                                          [body (or/c expr? seq? any/c)])) ; set existing stack slot(s)
(define-form-struct (let-rec expr) ([procs (listof lam?)] [body (or/c expr? seq? any/c)])) ; put `letrec'-bound closures into existing stack slots
(define-form-struct (boxenv expr) ([pos exact-nonnegative-integer?] [body (or/c expr? seq? any/c)])) ; box existing stack element

(define-form-struct (localref expr) ([unbox? boolean?] 
                                     [pos exact-nonnegative-integer?] 
                                     [clear? boolean?] 
                                     [other-clears? boolean?] 
                                     [type (or/c #f 'flonum 'fixnum 'extflonum)])) ; access local via stack


(define-form-struct (application expr) ([rator (or/c expr? seq? any/c)] [rands (listof (or/c expr? seq? any/c))])) ; function call
(define-form-struct (branch expr) ([test (or/c expr? seq? any/c)] [then (or/c expr? seq? any/c)] [else (or/c expr? seq? any/c)])) ; `if'
(define-form-struct (with-cont-mark expr) ([key (or/c expr? seq? any/c)] 
                                           [val (or/c expr? seq? any/c)] 
                                           [body (or/c expr? seq? any/c)])) ; `with-continuation-mark'
(define-form-struct (beg0 expr) ([seq (listof (or/c expr? seq? any/c))])) ; `begin0'
(define-form-struct (varref expr) ([toplevel (or/c toplevel? #t)] [dummy (or/c toplevel? #f)])) ; `#%variable-reference'
(define-form-struct (assign expr) ([id toplevel?] [rhs (or/c expr? seq? any/c)] [undef-ok? boolean?])) ; top-level or module-level set!
(define-form-struct (apply-values expr) ([proc (or/c expr? seq? any/c)] [args-expr (or/c expr? seq? any/c)])) ; `(call-with-values (lambda () ,args-expr) ,proc)
(define-form-struct (with-immed-mark expr) ([key (or/c expr? seq? any/c)] 
                                            [def-val (or/c expr? seq? any/c)] 
                                            [body (or/c expr? seq? any/c)]))
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive
