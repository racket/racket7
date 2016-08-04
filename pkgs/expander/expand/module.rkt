#lang racket/base
(require racket/promise
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../syntax/track.rkt"
         "../common/phase.rkt"
         "../syntax/track.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../syntax/binding.rkt"
         "dup-check.rkt"
         "free-id-set.rkt"
         "require+provide.rkt"
         "../common/module-path.rkt"
         "lift-context.rkt"
         "../namespace/core.rkt"
         "context.rkt"
         "use-site.rkt"
         "main.rkt"
         "require.rkt"
         "provide.rkt"
         "def-id.rkt"
         "log.rkt"
         "../compile/main.rkt"
         "../eval/top.rkt"
         "../eval/module.rkt"
         "cross-phase.rkt"
         "../syntax/debug.rkt"
         "../common/performance.rkt")

(add-core-form!
 'module
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (log-expand ctx 'prim-module)
     (raise-syntax-error #f "allowed only at the top level" s))
   (performance-region
    ['expand 'module]
    (expand-module s ctx #f))))

(add-core-form!
 'module*
 (lambda (s ctx)
   (log-expand ctx 'prim-module)
   (raise-syntax-error #f "illegal use (not in a module top-level)" s)))

(add-core-form!
 '#%module-begin
 (lambda (s ctx)
   (log-expand ctx 'prim-module-begin)
   (unless (eq? (expand-context-context ctx) 'module-begin)
     (raise-syntax-error #f "not in a module-definition context" s))
   (unless (expand-context-module-begin-k ctx)
     (raise-syntax-error #f "not currently transforming a module" s))
   ;; This `#%module-begin` must be in a `module`; the
   ;; `module-begin-k` function continues that module's
   ;; expansion
   ((expand-context-module-begin-k ctx)
    s
    (struct-copy expand-context ctx
                 [module-begin-k #f]))))

(add-core-form!
 '#%declare
 (lambda (s ctx)
   (log-expand ctx 'prim-declare)
   ;; The `#%module-begin` expander handles `#%declare`
   (raise-syntax-error #f "not allowed outside of a module body" s)))

;; ----------------------------------------

(define (expand-module s init-ctx enclosing-self
                       #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                       #:enclosing-all-scopes-stx [enclosing-all-scopes-stx #f]
                       #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                       #:enclosing-requires+provides [enclosing-r+p #f]
                       #:mpis-for-enclosing-reset [mpis-for-enclosing-reset #f])
   (log-expand init-ctx 'prim-module)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(module id:module-name initial-require body ...))
   
   (define initial-require (syntax->datum (m 'initial-require)))
   (unless (or keep-enclosing-scope-at-phase
               (module-path? initial-require))
     (raise-syntax-error #f "not a module path" s (m 'initial-require)))
   
   ;; All module bodies start at phase 0
   (define phase 0)
   
   (define module-name-sym (syntax-e (m 'id:module-name)))
   
   (define outside-scope (new-scope 'module))
   (define inside-scope (new-multi-scope module-name-sym))

   (define self (make-self-module-path-index (if enclosing-self
                                                 module-name-sym
                                                 (string->uninterned-symbol
                                                  (symbol->string module-name-sym)))
                                             enclosing-self))
   
   (define enclosing-mod (and enclosing-self
                              (module-path-index-join '(submod "..") self)))
   (when (and enclosing-mod mpis-for-enclosing-reset)
     (set-box! mpis-for-enclosing-reset
               (cons enclosing-mod (unbox mpis-for-enclosing-reset))))
   
   (define apply-module-scopes
     (make-apply-module-scopes outside-scope inside-scope 
                               init-ctx keep-enclosing-scope-at-phase
                               self enclosing-self enclosing-mod))

   ;; Initial require name provides the module's base scopes
   (define all-scopes-s (apply-module-scopes 
                         (if enclosing-all-scopes-stx
                             (syntax-shift-phase-level enclosing-all-scopes-stx
                                                       keep-enclosing-scope-at-phase)
                             (m 'initial-require))))
   (define initial-require-s (apply-module-scopes (m 'initial-require)))

   (define root-ctx (make-root-expand-context
                     #:initial-scopes (if keep-enclosing-scope-at-phase
                                          (root-expand-context-module-scopes init-ctx)
                                          null)
                     #:outside-scope outside-scope
                     #:post-expansion-scope inside-scope
                     #:all-scopes-stx all-scopes-s))
   
   ;; Extract combined scopes
   (define new-module-scopes (root-expand-context-module-scopes root-ctx))

   ;; A frame-id is used to determine when use-site scopes are needed
   (define frame-id (root-expand-context-frame-id root-ctx))

   ;; Make a namespace for module expansion
   (define m-ns (make-module-namespace (expand-context-namespace init-ctx)
                                       #:mpi self
                                       #:root-expand-context root-ctx
                                       #:for-submodule? (and enclosing-self #t)))
   
   ;; Initial context for all body expansions:
   (define ctx (struct-copy expand-context (copy-root-expand-context init-ctx root-ctx)
                            [allow-unbound? #f]
                            [namespace m-ns]
                            [post-expansion-scope-action add-scope]
                            [phase phase]
                            [just-once? #f]))
   
   ;; Add the module's scope to the body forms; use `disarmed-s` and
   ;; re-match to extract the body forms, because that improves sharing
   (define bodys (let ([scoped-s (apply-module-scopes disarmed-s)])
                   (define-match m scoped-s '(_ _ _ body ...))
                   (m 'body)))
   
   ;; To keep track of all requires and provides
   (define requires+provides (make-requires+provides self))

   ;; Table of symbols picked for each binding in this module:
   (define defined-syms (root-expand-context-defined-syms root-ctx)) ; phase -> sym -> id

   ;; Accumulate module path indexes used by submodules to refer to this module
   (define mpis-to-reset (box null))
     
   ;; Initial require
   (define (initial-require!)
     (cond
      [(not keep-enclosing-scope-at-phase)
       ;; Install the initial require
       (perform-initial-require! initial-require self
                                 all-scopes-s
                                 m-ns
                                 requires+provides
                                 #:who 'module)]
      [else
       ;; For `(module* name #f ....)`, just register the enclosing module
       ;; as an import and visit it
       (add-required-module! requires+provides
                             enclosing-mod
                             keep-enclosing-scope-at-phase
                             enclosing-is-cross-phase-persistent?)
       (add-enclosing-module-defined-and-required! requires+provides
                                                   #:enclosing-requires+provides enclosing-r+p
                                                   enclosing-mod
                                                   keep-enclosing-scope-at-phase)
       (namespace-module-visit! m-ns enclosing-mod
                                keep-enclosing-scope-at-phase)]))
   (log-expand init-ctx 'prepare-env)
   (initial-require!)

   ;; To detect whether the body is expanded multiple times:
   (define again? #f)
   
   ;; The primitive `#%module-body` form calls this function to expand the
   ;; current module's body
   (define (module-begin-k mb-s ctx)
     ;; In case the module body is expanded multiple times, we clear
     ;; the requires, provides and definitions information each time
     (when again?
       (requires+provides-reset! requires+provides)
       (initial-require!))
     (set! again? #t)
     
     ;; In case `#%module-begin` expansion is forced on syntax that
     ;; that wasn't already introduced into the mdoule's inside scope,
     ;; add it to all the given body forms
     (define added-s (add-scope mb-s inside-scope))
     (log-expand ctx 'rename-one added-s)

     (define disarmed-mb-s (syntax-disarm added-s))
     (define-match mb-m disarmed-mb-s '(#%module-begin body ...))
     (define bodys (mb-m 'body))
     
     ;; For variable repeferences before corresponding binding (phase >= 1)
     (define need-eventually-defined (make-hasheqv)) ; phase -> list of id
     
     ;; For `syntax-local-lift-module-end-declaration`, which is accumulated
     ;; across phases:
     (define module-ends (make-shared-module-ends))
     
     ;; Accumulate `#%declare` content
     (define declared-keywords (make-hasheq))
     
     ;; Accumulated declared submodule names for `syntax-local-submodules`
     (define declared-submodule-names (make-hasheq))
     
     ;; The expansion of the module body happens in 4 passes:
     ;;  Pass 1: Partial expansion to determine imports and definitions
     ;;  Pass 2: Complete expansion of remaining expressions
     ;;  Pass 3: Parsing of provide forms
     ;;  Pass 4: Parsing of `module*` submodules
     
     ;; Passes 1 and 2 are nested via `begin-for-syntax`:
     (define expression-expanded-bodys
       (let pass-1-and-2-loop ([bodys bodys] [phase phase])

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 1: partially expand to discover all bindings and install all 
         ;; defined macro transformers
         
         ;; Need to accumulate definition contexts created during
         ;; partial expansion:
         (define def-ctx-scopes (box null))
         
         (define partial-body-ctx (struct-copy expand-context ctx
                                               [context 'module]
                                               [phase phase]
                                               [namespace (namespace->namespace-at-phase m-ns phase)]
                                               [only-immediate? #t]
                                               [def-ctx-scopes def-ctx-scopes]
                                               [need-eventually-defined need-eventually-defined] ; used only at phase 1 and up
                                               [declared-submodule-names declared-submodule-names]
                                               [lifts (make-lift-context
                                                       (make-wrap-as-definition self frame-id
                                                                                inside-scope all-scopes-s
                                                                                defined-syms requires+provides))]
                                               [module-lifts (make-module-lift-context phase #t)]
                                               [require-lifts (make-require-lift-context
                                                               phase
                                                               (make-parse-lifted-require m-ns self requires+provides
                                                                                          #:declared-submodule-names declared-submodule-names))]
                                               [to-module-lifts (make-to-module-lift-context
                                                                 phase
                                                                 #:shared-module-ends module-ends
                                                                 #:end-as-expressions? #f)]))

         (define partially-expanded-bodys
           (partially-expand-bodys bodys
                                   #:original s
                                   #:tail? (zero? phase)
                                   #:phase phase
                                   #:ctx partial-body-ctx
                                   #:namespace m-ns
                                   #:self self
                                   #:frame-id frame-id
                                   #:requires-and-provides requires+provides
                                   #:need-eventually-defined need-eventually-defined
                                   #:all-scopes-stx all-scopes-s
                                   #:defined-syms defined-syms
                                   #:declared-keywords declared-keywords
                                   #:declared-submodule-names declared-submodule-names
                                   #:mpis-to-reset mpis-to-reset
                                   #:loop pass-1-and-2-loop))

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 2: finish expanding expressions
         
         (log-expand partial-body-ctx 'next-group)
         
         (define body-ctx (struct-copy expand-context (accumulate-def-ctx-scopes partial-body-ctx def-ctx-scopes)
                                       [only-immediate? #f]
                                       [def-ctx-scopes #f]
                                       [post-expansion-scope #:parent root-expand-context #f]
                                       [to-module-lifts (make-to-module-lift-context phase
                                                                                     #:shared-module-ends module-ends
                                                                                     #:end-as-expressions? #t)]))
         
         (finish-expanding-body-expressons partially-expanded-bodys
                                           #:tail? (zero? phase)
                                           #:phase phase
                                           #:ctx body-ctx
                                           #:self self
                                           #:declared-submodule-names declared-submodule-names
                                           #:mpis-to-reset mpis-to-reset)))

     ;; Check that any tentatively allowed reference at phase >= 1 is ok
     (check-defined-by-now need-eventually-defined self)
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 3: resolve provides at all phases
     
     (log-expand ctx 'next-group)
     
     (define fully-expanded-bodys-except-post-submodules
       (resolve-provides expression-expanded-bodys
                         #:original s
                         #:requires-and-provides requires+provides
                         #:declared-submodule-names declared-submodule-names
                         #:namespace m-ns
                         #:phase phase
                         #:self self
                         #:ctx ctx))

     ;; Validate any cross-phase persistence request
     (define is-cross-phase-persistent? (hash-ref declared-keywords '#:cross-phase-persistent #f))
     (when is-cross-phase-persistent?
       (unless (requires+provides-can-cross-phase-persistent? requires+provides)
         (raise-syntax-error #f "cannot be cross-phase persistent due to required modules" s
                             (hash-ref declared-keywords '#:cross-phase-persistent)))
       (check-cross-phase-persistent-form fully-expanded-bodys-except-post-submodules))

     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 4: expand `module*` submodules
     
     (log-expand ctx 'next)

     (define submod-ctx (struct-copy expand-context ctx
                                     [frame-id #:parent root-expand-context #f]
                                     [post-expansion-scope #:parent root-expand-context #f]))
     
     (define declare-enclosing-module
       ;; Ensure this module on demand for `module*` submodules that might use it
       (delay (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                            #:module-name-id (m 'id:module-name)
                                            #:module-intial-require (m 'initial-require)
                                            #:module-begin-id (mb-m '#%module-begin)
                                            #:requires-and-provides requires+provides
                                            #:namespace m-ns
                                            #:self self
                                            #:enclosing enclosing-self
                                            #:root-ctx root-ctx)))
     
     (define fully-expanded-bodys
       (cond
        [(stop-at-module*? ctx)
         fully-expanded-bodys-except-post-submodules]
        [else
         (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                 #:declare-enclosing declare-enclosing-module
                                 #:original s
                                 #:phase phase
                                 #:self self
                                 #:requires-and-provides requires+provides
                                 #:enclosing-is-cross-phase-persistent? is-cross-phase-persistent?
                                 #:all-scopes-s all-scopes-s
                                 #:mpis-to-reset mpis-to-reset
                                 #:declared-submodule-names declared-submodule-names
                                 #:ctx ctx)]))
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Finish
     
     ;; Assemble the `#%module-begin` result:
     (rebuild
      mb-s disarmed-mb-s
      `(,(mb-m '#%module-begin) ,@fully-expanded-bodys)))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Actually expand the `#%module-body` form
   
   ;; The preceding function performs the expansion; here's where we
   ;; trigger it
   
   (define mb-ctx
     (struct-copy expand-context ctx
                  [context 'module-begin]
                  [module-begin-k module-begin-k]))
   
   (define mb-scopes-s
     (if keep-enclosing-scope-at-phase
         ;; for `(module* name #f)`, use the `(module* ...)` form
         disarmed-s
         ;; otherwise, use the initial require
         all-scopes-s))
   
   ;; Add `#%module-begin` around the body if it's not already present;
   ;; also logs 'rename-one
   (define mb
     (ensure-module-begin bodys
                          #:module-name-sym module-name-sym
                          #:scopes-s mb-scopes-s
                          #:m-ns m-ns
                          #:ctx mb-ctx
                          #:phase phase
                          #:s s))
   
   ;; Expand the body
   (define expanded-mb (performance-region
                        ['expand 'module-begin]
                        (expand mb mb-ctx)))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Assemble the `module` result
   
   ;; Shift the "self" reference that we have been using for expansion
   ;; to a generic and constant (for a particualr submodule path)
   ;; "self", so that we can reocognize it for compilation or to shift
   ;; back on any future re-expansion:
   (define generic-self (make-generic-self-module-path-index self))
   
   ;; Make `self` like `generic-self`; this hacky update plays the
   ;; role of applying a shift to identifiers that are in syntax
   ;; properties, such as the 'origin property
   (imitate-generic-module-path-index! self)
   (for ([mpi (in-list (unbox mpis-to-reset))])
     (imitate-generic-module-path-index! mpi))

   (let* ([result-s
           (rebuild
            s disarmed-s
            `(,(m 'module) ,(m 'id:module-name) ,initial-require-s ,expanded-mb))]
          [result-s 
           (syntax-module-path-index-shift result-s self generic-self)]
          [result-s 
           (attach-require-provide-properties requires+provides result-s self generic-self)]
          [result-s (attach-root-expand-context-properties result-s root-ctx self generic-self)]
          [result-s (if (requires+provides-all-bindings-simple? requires+provides)
                        (syntax-property result-s 'module-body-context-simple? #t)
                        result-s)])
     (log-expand ctx 'rename-one result-s)
     result-s))

;; ----------------------------------------

;; Add `#%module-begin` to `bodys`, if needed
(define (ensure-module-begin bodys
                             #:module-name-sym module-name-sym
                             #:scopes-s scopes-s
                             #:m-ns m-ns
                             #:ctx ctx 
                             #:phase phase
                             #:s s)
  (define (make-mb-ctx)
    (struct-copy expand-context ctx
                 [context 'module-begin]
                 [only-immediate? #t]))
  (define mb
    (cond
     [(= 1 (length bodys))
      ;; Maybe it's already a `#%module-begin` form, or maybe it
      ;; will expand to one
      (log-expand ctx 'rename-one (car bodys))
      (cond
       [(eq? '#%module-begin (core-form-sym (syntax-disarm (car bodys)) phase))
        ;; Done
        (car bodys)]
       [else
        ;; A single body form might be a macro that expands to
        ;; the primitive `#%module-begin` form:
        (define partly-expanded-body
          (performance-region
           ['expand 'module-begin]
           (expand (add-enclosing-name-property (car bodys) module-name-sym)
                   (make-mb-ctx))))
        (cond
         [(eq? '#%module-begin (core-form-sym (syntax-disarm partly-expanded-body) phase))
          ;; Yes, it expanded to `#%module-begin`
          partly-expanded-body]
         [else
          ;; No, it didn't expand to `#%module-begin`
          (add-module-begin (list partly-expanded-body) s scopes-s phase module-name-sym 
                            (make-mb-ctx)
                            #:log-rename-one? #f)])])]
     [else
      ;; Multiple body forms definitely need a `#%module-begin` wrapper
      (add-module-begin bodys s scopes-s phase module-name-sym
                        (make-mb-ctx))]))
  (add-enclosing-name-property mb module-name-sym))

;; Add `#%module-begin`, because it's needed
(define (add-module-begin bodys s scopes-s phase module-name-sym mb-ctx
                          #:log-rename-one? [log-rename-one? #t])
  (define disarmed-scopes-s (syntax-disarm scopes-s))
  (define mb-id (datum->syntax disarmed-scopes-s '#%module-begin))
  ;; If `mb-id` is not bound, we'd like to give a clear error message
  (unless (resolve mb-id phase)
    (raise-syntax-error #f "no #%module-begin binding in the module's language" s))
  (define mb (datum->syntax disarmed-scopes-s `(,mb-id ,@bodys) s))
  (log-expand mb-ctx 'tag mb)
  (when log-rename-one?
    (log-expand mb-ctx 'rename-one mb))
  (define partly-expanded-mb (performance-region
                              ['expand 'module-begin]
                              (expand (add-enclosing-name-property mb module-name-sym)
                                      mb-ctx)))
  (unless (eq? '#%module-begin (core-form-sym (syntax-disarm partly-expanded-mb) phase))
    (raise-syntax-error #f "expansion of #%module-begin is not a #%plain-module-begin form" s
                        partly-expanded-mb))
  partly-expanded-mb)

(define (add-enclosing-name-property stx module-name-sym)
  (syntax-property stx 'enclosing-module-name module-name-sym))

;; ----------------------------------------

;; Make function to adjust syntax that appears in the original module body
(define (make-apply-module-scopes inside-scope outside-scope
                                  init-ctx keep-enclosing-scope-at-phase
                                  self enclosing-self enclosing-mod)
  (lambda (s)
    (performance-region
     ['expand 'module 'scopes]
     (define s-without-enclosing
       (if keep-enclosing-scope-at-phase
           ;; Keep enclosing module scopes for `(module* _ #f ....)`
           s
           ;; Remove the scopes of the top level or a module outside of
           ;; this module, as well as any relevant use-site scopes
           (remove-use-site-scopes
            (for/fold ([s s]) ([sc (in-list (root-expand-context-module-scopes init-ctx))])
              (remove-scope s sc))
            init-ctx)))
     ;; Add outside- and inside-edge scopes
     (define s-with-edges
       (add-scope (add-scope s-without-enclosing
                             outside-scope)
                  inside-scope))
     (define s-with-suitable-enclosing
       (cond
        [keep-enclosing-scope-at-phase
         ;; Shift any references to the enclosing module to be relative to the
         ;; submodule
         (syntax-module-path-index-shift
          s-with-edges
          enclosing-self
          enclosing-mod)]
        [else s-with-edges]))
     ;; In case we're expanding syntax that was previously expanded,
     ;; shift the generic "self" to the "self" for the current expansion:
     (syntax-module-path-index-shift
      s-with-suitable-enclosing
      (make-generic-self-module-path-index self)
      self
      ;; Also preserve the expansion-time code inspector
      (current-code-inspector)))))

;; ----------------------------------------

;; Pass 1 of `module` expansion, which uncovers definitions,
;; requires, and `module` submodules
(define (partially-expand-bodys bodys
                                #:original s
                                #:tail? tail?
                                #:phase phase
                                #:ctx partial-body-ctx
                                #:namespace m-ns
                                #:self self
                                #:frame-id frame-id
                                #:requires-and-provides requires+provides
                                #:need-eventually-defined need-eventually-defined
                                #:all-scopes-stx all-scopes-stx
                                #:defined-syms defined-syms
                                #:declared-keywords declared-keywords
                                #:declared-submodule-names declared-submodule-names
                                #:mpis-to-reset mpis-to-reset
                                #:loop pass-1-and-2-loop)
  (namespace-visit-available-modules! m-ns phase)
  (let loop ([tail? tail?] [bodys bodys])
    (cond
     [(null? bodys)
      (cond
       [tail?
        ;; Were at the very end of the module; if there are any lifted-to-end
        ;; declarations, keep going
        (define bodys
          (append
           (get-and-clear-end-lifts! (expand-context-to-module-lifts partial-body-ctx))
           (get-and-clear-provide-lifts! (expand-context-to-module-lifts partial-body-ctx))))
        (if (null? bodys)
            null
            (loop #t (add-post-expansion-scope bodys partial-body-ctx)))]
       [else null])]
     [else
      (log-expand partial-body-ctx 'next)
      (define exp-body (performance-region
                        ['expand 'form-in-module/1]
                        (expand (car bodys) partial-body-ctx)))
      (define disarmed-exp-body (syntax-disarm exp-body))
      (define lifted-defns (get-and-clear-lifts! (expand-context-lifts partial-body-ctx)))
      (cond
       [(null? lifted-defns)
        (log-expand partial-body-ctx 'rename-list lifted-defns)
        (log-expand partial-body-ctx 'module-lift-loop (append lifted-defns (cons exp-body (cdr bodys))))]
       [else
        (log-expand partial-body-ctx 'module-lift-end-loop (cons exp-body (cdr bodys)))])
      (log-expand partial-body-ctx 'rename-one exp-body)
      (append
       ;; Save any requires lifted during partial expansion
       (get-and-clear-require-lifts! (expand-context-require-lifts partial-body-ctx))
       ;; Ditto for expressions
       lifted-defns
       ;; Ditto for modules, which need to be processed
       (loop #f (get-and-clear-module-lifts! (expand-context-module-lifts partial-body-ctx)))
       ;; Dispatch on form revealed by partial expansion
       (case (core-form-sym disarmed-exp-body phase)
         [(begin)
          (define-match m disarmed-exp-body '(begin e ...))
          (define (track e) (syntax-track-origin e exp-body))
          (define spliced-bodys (append (map track (m 'e)) (cdr bodys)))
          (log-expand partial-body-ctx 'splice spliced-bodys)
          (loop tail? spliced-bodys)]
         [(begin-for-syntax)
          (log-expand partial-body-ctx 'enter-prim exp-body)
          (log-expand partial-body-ctx 'enter-prim-begin-for-syntax)
          (define-match m disarmed-exp-body '(begin-for-syntax e ...))
          (define nested-bodys (pass-1-and-2-loop (m 'e) (add1 phase)))
          (define ct-m-ns (namespace->namespace-at-phase m-ns (add1 phase)))
          (namespace-run-available-modules! m-ns (add1 phase)) ; to support running `begin-for-syntax`
          (eval-nested-bodys nested-bodys (add1 phase) ct-m-ns self partial-body-ctx)
          (namespace-visit-available-modules! m-ns phase) ; since we're shifting back a phase
          (log-expand partial-body-ctx 'exit-prim)
          (cons
           (rebuild
            exp-body disarmed-exp-body
            `(,(m 'begin-for-syntax) ,@nested-bodys))
           (loop tail? (cdr bodys)))]
         [(define-values)
          (log-expand partial-body-ctx 'enter-prim exp-body)
          (log-expand partial-body-ctx 'enter-prim-define-values)
          (define-match m disarmed-exp-body '(define-values (id ...) rhs))
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-no-duplicate-ids ids phase exp-body)
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind! ids defined-syms 
                                                      self phase all-scopes-stx
                                                      #:frame-id frame-id
                                                      #:requires+provides requires+provides
                                                      #:in exp-body))
          (add-defined-syms! requires+provides syms phase)
          (log-expand partial-body-ctx 'exit-prim)
          (cons (rebuild 
                 exp-body disarmed-exp-body
                 `(,(m 'define-values) ,ids ,(m 'rhs)))
                (loop tail? (cdr bodys)))]
         [(define-syntaxes)
          (log-expand partial-body-ctx 'enter-prim exp-body)
          (log-expand partial-body-ctx 'enter-prim-define-syntaxes)
          (define-match m disarmed-exp-body '(define-syntaxes (id ...) rhs))
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-no-duplicate-ids ids phase exp-body)
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind! ids defined-syms
                                                      self phase all-scopes-stx
                                                      #:frame-id frame-id
                                                      #:requires+provides requires+provides
                                                      #:in exp-body
                                                      #:as-transformer? #t))
          (add-defined-syms! requires+provides syms phase)
          ;; Expand and evaluate RHS:
          (define-values (exp-rhs vals)
            (expand+eval-for-syntaxes-binding (m 'rhs) ids
                                              (struct-copy expand-context partial-body-ctx
                                                           [need-eventually-defined need-eventually-defined])))
          ;; Install transformers in the namespace for expansion:
          (for ([sym (in-list syms)]
                [val (in-list vals)]
                [id (in-list ids)])
            (maybe-install-free=id! val id phase)
            (namespace-set-transformer! m-ns phase sym val))
          (log-expand partial-body-ctx 'exit-prim)
          (cons (rebuild 
                 exp-body disarmed-exp-body
                 `(,(m 'define-syntaxes) ,ids ,exp-rhs))
                (loop tail? (cdr bodys)))]
         [(#%require)
          (log-expand partial-body-ctx 'enter-prim exp-body)
          (log-expand partial-body-ctx 'enter-prim-require)
          (define ready-body (remove-use-site-scopes disarmed-exp-body partial-body-ctx))
          (define-match m ready-body '(#%require req ...))
          (parse-and-perform-requires! (m 'req) exp-body #:self self
                                       m-ns phase #:run-phase phase
                                       requires+provides
                                       #:declared-submodule-names declared-submodule-names
                                       #:who 'module)
          (log-expand partial-body-ctx 'exit-prim)
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(#%provide)
          ;; save for last pass
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(module)
          ;; Submodule to parse immediately
          (define ready-body (remove-use-site-scopes exp-body partial-body-ctx))
          (define submod
            (expand-submodule ready-body self partial-body-ctx
                              #:is-star? #f
                              #:declared-submodule-names declared-submodule-names
                              #:mpis-to-reset mpis-to-reset))
          (cons submod
                (loop tail? (cdr bodys)))]
         [(module*)
          ;; Submodule to save for after this module
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [(#%declare)
          (define-match m disarmed-exp-body '(#%declare kw ...))
          (for ([kw (in-list (m 'kw))])
            (unless (keyword? (syntax-e kw))
              (raise-syntax-error #f "expected a keyword" exp-body kw))
            (unless (memq (syntax-e kw) '(#:cross-phase-persistent #:empty-namespace))
              (raise-syntax-error #f "not an allowed declaration keyword" exp-body kw))
            (when (hash-ref declared-keywords (syntax-e kw) #f)
              (raise-syntax-error #f "keyword declared multiple times" exp-body kw))
            (hash-set! declared-keywords (syntax-e kw) kw))
          (cons exp-body
                (loop tail? (cdr bodys)))]
         [else
          ;; save expression for next pass
          (cons exp-body
                (loop tail? (cdr bodys)))]))])))

;; Convert lifted identifiers plus expression to a `define-values` form:
(define (make-wrap-as-definition self frame-id
                                 inside-scope all-scopes-stx
                                 defined-syms requires+provides)
  (lambda (ids rhs phase)
    (define scoped-ids (for/list ([id (in-list ids)])
                         (add-scope id inside-scope)))
    (select-defined-syms-and-bind! scoped-ids defined-syms
                                   self phase all-scopes-stx
                                   #:frame-id frame-id
                                   #:requires+provides requires+provides)
    (values scoped-ids
            (add-scope (datum->syntax
                        #f
                        (list (datum->syntax (syntax-shift-phase-level core-stx phase)
                                             'define-values)
                              scoped-ids
                              rhs))
                       inside-scope))))

(define (add-post-expansion-scope bodys ctx)
  (define sc (root-expand-context-post-expansion-scope ctx))
  (for/list ([body (in-list bodys)])
    (add-scope body sc)))

;; ----------------------------------------

;; Pass 2 of `module` expansion, which expands all expressions
(define (finish-expanding-body-expressons partially-expanded-bodys
                                          #:tail? tail?
                                          #:phase phase
                                          #:ctx body-ctx
                                          #:self self
                                          #:declared-submodule-names declared-submodule-names
                                          #:mpis-to-reset mpis-to-reset)
  (let loop ([tail? tail?] [bodys partially-expanded-bodys])
    (cond
     [(null? bodys)
      (cond
       [tail? 
        ;; Were at the very end of the module, again, so check for lifted-to-end
        ;; declarations
        (define bodys
          (append
           (get-and-clear-end-lifts! (expand-context-to-module-lifts body-ctx))
           (get-and-clear-provide-lifts! (expand-context-to-module-lifts body-ctx))))
        (if (null? bodys)
            null
            (loop #t bodys))]
       [else bodys])]
     [else
      (log-expand body-ctx 'next)
      (define disarmed-body (syntax-disarm (car bodys)))
      (define exp-body
        (case (core-form-sym disarmed-body phase)
          [(define-values)
           (define-match m disarmed-body '(define-values (id ...) rhs))
           (define exp-rhs (performance-region
                            ['expand 'form-in-module/2]
                            (expand (m 'rhs) (as-named-context (as-expression-context body-ctx)
                                                               (m 'id)))))
           (rebuild 
            (car bodys) disarmed-body
            `(,(m 'define-values) ,(m 'id) ,exp-rhs))]
          [(define-syntaxes #%require #%provide begin-for-syntax module module* #%declare)
           (car bodys)]
          [else
           (performance-region
            ['expand 'form-in-module/2]
            (expand (car bodys) (as-expression-context body-ctx)))]))
      (define lifted-defns
        ;; If there were any lifts, the right-hand sides need to be expanded
        (loop #f (get-and-clear-lifts! (expand-context-lifts body-ctx))))
      (define lifted-requires
        ;; Get any requires and provides, keeping them as-is
        (get-and-clear-require-lifts! (expand-context-require-lifts body-ctx)))
      (define lifted-modules
        ;; If there were any module lifts, the `module` forms need to
        ;; be expanded
        (expand-non-module*-submodules (get-and-clear-module-lifts!
                                        (expand-context-module-lifts body-ctx))
                                       phase
                                       self
                                       body-ctx
                                       #:mpis-to-reset mpis-to-reset
                                       #:declared-submodule-names declared-submodule-names))
      (cond
       [(null? lifted-defns)
        (log-expand body-ctx 'module-lift-loop lifted-defns)]
       [else
        (log-expand body-ctx 'module-lift-end-loop (cons exp-body (cdr bodys)))])
      (append
       lifted-requires
       lifted-defns
       lifted-modules
       (cons exp-body
             (loop tail? (cdr bodys))))])))

(define (check-defined-by-now need-eventually-defined self)
  ;; If `need-eventually-defined` is not empty, report an error
  (for ([(phase l) (in-hash need-eventually-defined)])
    (for ([id (in-list l)])
      (define b (resolve+shift id phase))
      ;; FIXME: check that the binding is for a variable
      (unless (and b
                   (module-binding? b)
                   (eq? (module-binding-sym b) (syntax-e id))
                   (eq? (module-binding-module b) self))
        (raise-syntax-error #f "reference to an unbound identifier" id)))))

;; ----------------------------------------

;; Pass 3 of `module` expansion, which parses `provide` forms and
;; matches them up with defintiions and requires
(define (resolve-provides expression-expanded-bodys
                          #:original s
                          #:requires-and-provides requires+provides
                          #:declared-submodule-names declared-submodule-names
                          #:namespace m-ns
                          #:phase phase
                          #:self self
                          #:ctx ctx)
  (performance-region
   ['expand 'provide]
   (let loop ([bodys expression-expanded-bodys] [phase phase])
     (cond
      [(null? bodys) null]
      [else
       (define disarmed-body (syntax-disarm (car bodys)))
       (case (core-form-sym disarmed-body phase)
         [(#%provide)
          (log-expand ctx 'enter-prim (car bodys))
          (log-expand ctx 'enter-prim-provide)
          (define-match m disarmed-body '(#%provide spec ...))
          (define-values (track-stxes specs)
            (parse-and-expand-provides! (m 'spec) (car bodys)
                                        requires+provides self
                                        phase (struct-copy expand-context ctx
                                                           [context 'top-level]
                                                           [phase phase]
                                                           [namespace (namespace->namespace-at-phase m-ns phase)]
                                                           [requires+provides requires+provides]
                                                           [declared-submodule-names declared-submodule-names])))
          (log-expand ctx 'exit-prim)
          (cons (syntax-track-origin*
                 track-stxes
                 (rebuild
                  (car bodys) disarmed-body
                  `(,(m '#%provide) ,@specs)))
                (loop (cdr bodys) phase))]
         [(begin-for-syntax)
          (define-match m disarmed-body '(begin-for-syntax e ...))
          (define nested-bodys (loop (m 'e) (add1 phase)))
          (cons (rebuild (car bodys) disarmed-body `(,(m 'begin-for-syntax) ,@nested-bodys))
                (loop (cdr bodys) phase))]
         [else
          (cons (car bodys)
                (loop (cdr bodys) phase))])]))))

;; ----------------------------------------

;; In support of pass 4, declare a module (in a temporary namespace)
;; before any `module*` submodule is expanded
(define (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                      #:module-name-id module-name-id
                                      #:module-intial-require module-initial-require
                                      #:module-begin-id module-begin-id
                                      #:requires-and-provides requires+provides
                                      #:namespace m-ns
                                      #:self self
                                      #:enclosing enclosing-self
                                      #:root-ctx root-ctx)
  (define tmp-mod (attach-root-expand-context-properties
                   (attach-require-provide-properties
                    requires+provides
                    (datum->syntax
                     #f
                     `(,(datum->syntax core-stx 'module) ,module-name-id ,module-initial-require
                       (,module-begin-id
                        ,@fully-expanded-bodys-except-post-submodules)))
                    self
                    self)
                   root-ctx
                   self self))

  (define module-name (module-path-index-resolve (or enclosing-self self)))
  (define root-module-name (resolved-module-path-root-name module-name))
  (parameterize ([current-namespace m-ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (eval-module
     (compile-module tmp-mod
                     (make-compile-context #:namespace m-ns
                                           #:module-self enclosing-self
                                           #:full-module-name (and enclosing-self
                                                                   (resolved-module-path-name module-name)))
                     #:self self
                     #:with-submodules? #f)
     #:with-submodules? #f)))

(define (attach-root-expand-context-properties s root-ctx orig-self new-self)
  ;; For `module->namespace`
  (let ([s (syntax-property s
                            'module-root-expand-context
                            (root-expand-context-encode-for-module root-ctx orig-self new-self))])
    ;; Original API:
    (let* ([s (syntax-property s 'module-body-context (root-expand-context-all-scopes-stx root-ctx))]
           [s (syntax-property s
                               'module-body-inside-context
                               (add-scope empty-syntax
                                          (root-expand-context-post-expansion-scope root-ctx)))])
      s)))

;; ----------------------------------------

;; Pass 4 of `module` expansion, which expands `module*` forms;
;; this pass muct happen after everything else for the module, since a
;; `module*` submodule can require from its enclosing module
(define (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                #:declare-enclosing declare-enclosing-module
                                #:original s
                                #:phase phase
                                #:self self
                                #:requires-and-provides requires+provides
                                #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                #:all-scopes-s all-scopes-s
                                #:mpis-to-reset mpis-to-reset
                                #:declared-submodule-names declared-submodule-names
                                #:ctx submod-ctx)
  (let loop ([bodys fully-expanded-bodys-except-post-submodules] [phase phase])
    (cond
     [(null? bodys) null]
     [else
      (define disarmed-body (syntax-disarm (car bodys)))
      (case (core-form-sym disarmed-body phase)
        [(module*)
         ;; Ensure that the enclosing module is declared:
         (force declare-enclosing-module)
         (define ready-body (remove-use-site-scopes (car bodys) submod-ctx))
         (define-match f-m  disarmed-body #:try '(module* name #f . _))
         (define submod
           (cond
            [(f-m)
             ;; Need to shift the submodule relative to the enclosing module:
             (define neg-phase (phase- 0 phase))
             (define shifted-s (syntax-shift-phase-level ready-body neg-phase))
             (define submod
               (expand-submodule shifted-s self submod-ctx
                                 #:is-star? #t
                                 #:keep-enclosing-scope-at-phase neg-phase
                                 #:enclosing-all-scopes-stx all-scopes-s
                                 #:enclosing-requires+provides requires+provides
                                 #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                 #:mpis-to-reset mpis-to-reset
                                 #:declared-submodule-names declared-submodule-names))
             (syntax-shift-phase-level submod phase)]
            [else
             (expand-submodule ready-body self submod-ctx
                               #:is-star? #t
                               #:mpis-to-reset mpis-to-reset
                               #:declared-submodule-names declared-submodule-names)]))
         (cons submod
               (loop (cdr bodys) phase))]
        [(begin-for-syntax)
         (define-match m disarmed-body '(begin-for-syntax e ...))
         (define nested-bodys (loop (m 'e) (add1 phase)))
         (cons (rebuild (car bodys) disarmed-body `(,(m 'begin-for-syntax) ,@nested-bodys))
               (loop (cdr bodys) phase))]
        [else
         (cons (car bodys)
               (loop (cdr bodys) phase))])])))

(define (stop-at-module*? ctx)
  (free-id-set-member? (expand-context-stops ctx)
                       (expand-context-phase ctx)
                       (syntax-shift-phase-level (datum->syntax core-stx 'module*)
                                                 (expand-context-phase ctx))))

;; ----------------------------------------

(define (check-ids-unbound ids phase requires+provides #:in s)
  (for ([id (in-list ids)])
    (check-not-defined requires+provides id phase #:in s #:who 'module)))

;; ----------------------------------------

(define (eval-nested-bodys bodys phase m-ns self ctx)
  ;; The defitions and expression bodys are fully expanded;
  ;; evaluate them
  (for ([body (in-list bodys)])
    (define disarmed-body (syntax-disarm body))
    (case (core-form-sym disarmed-body phase)
      [(define-values)
       (define-match m disarmed-body '(define-values (id ...) rhs))
       (define ids (m 'id))
       (define vals (eval-for-bindings ids (m 'rhs) phase m-ns ctx))
       (for ([id (in-list ids)]
             [val (in-list vals)])
         (define b (resolve id phase))
         (unless (and (module-binding? b)
                      (equal? self (module-binding-module b)))
           (error "internal error: nested binding is not to self"))
         (namespace-set-variable! m-ns phase (module-binding-sym b) val))]
      [(define-syntaxes)
       ;; already evaluated during expandion
       (void)]
      [(#%provide #%require module module* begin-for-syntax #%declare)
       ;; handled earlier or later
       (void)]
      [else
       ;; an expression
       (parameterize ([current-expand-context ctx]
                      [current-namespace m-ns])
         (eval-single-top
          (compile-single body (make-compile-context
                                #:namespace m-ns
                                #:phase phase))
          m-ns))])))

;; ----------------------------------------

(define (expand-submodule s self ctx
                          #:is-star? is-star?
                          #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                          #:enclosing-requires+provides [enclosing-r+p #f]
                          #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                          #:enclosing-all-scopes-stx [enclosing-all-scopes-stx #f]
                          #:mpis-to-reset mpis-to-reset
                          #:declared-submodule-names declared-submodule-names)
  (log-expand ctx 'enter-prim s)
  (log-expand ctx (if is-star? 'enter-prim-submodule* 'enter-prim-submodule))
  
  ;; Register name and check for duplicates
  (define-match m s '(module name . _))
  (define name (syntax-e (m 'name)))
  (when (hash-ref declared-submodule-names name #f)
    (raise-syntax-error #f "submodule already declared with the same name" s name))
  (hash-set! declared-submodule-names name (syntax-e (m 'module)))

  (define submod
    (expand-module s
                   (struct-copy expand-context ctx
                                [context 'module]
                                [only-immediate? #f]
                                [post-expansion-scope #:parent root-expand-context #f])
                   self
                   #:keep-enclosing-scope-at-phase keep-enclosing-scope-at-phase
                   #:enclosing-all-scopes-stx enclosing-all-scopes-stx
                   #:enclosing-requires+provides enclosing-r+p
                   #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                   #:mpis-for-enclosing-reset mpis-to-reset))
  
  ;; Compile and declare the submodule for use by later forms
  ;; in the enclosing module:
  (define ns (expand-context-namespace ctx))
  (define module-name (module-path-index-resolve self))
  (define root-module-name (resolved-module-path-root-name module-name))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (eval-module
     (compile-module submod 
                     (make-compile-context #:namespace ns
                                           #:module-self self
                                           #:full-module-name (resolved-module-path-name module-name))
                     #:with-submodules? #f)
     #:with-submodules? #f))
  
  (log-expand ctx 'exit-prim submod)

  ;; Return the expanded submodule
  submod)

;; Expand `module` forms, leave `module*` forms alone:
(define (expand-non-module*-submodules bodys phase self ctx
                                       #:mpis-to-reset mpis-to-reset
                                       #:declared-submodule-names declared-submodule-names)
  (for/list ([body (in-list bodys)])
    (case (core-form-sym (syntax-disarm body) phase)
      [(module)
       (expand-submodule body self ctx
                         #:is-star? #f
                         #:mpis-to-reset mpis-to-reset
                         #:declared-submodule-names declared-submodule-names)]
      [else body])))

;; ----------------------------------------

(define (make-parse-lifted-require m-ns self requires+provides
                                   #:declared-submodule-names declared-submodule-names)
  (lambda (s phase)
    (define-match m (syntax-disarm s) '(#%require req))
    (parse-and-perform-requires! (list (m 'req)) s #:self self
                                 m-ns phase #:run-phase phase
                                 requires+provides
                                 #:declared-submodule-names declared-submodule-names
                                 #:who 'require)))
