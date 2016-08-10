#lang racket/base
(require "../common/set.rkt"
         "../common/performance.rkt"
         "../syntax/syntax.rkt"
         "../syntax/to-list.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/property.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../syntax/binding.rkt"
         "../syntax/match.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "built-in-symbol.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "self-quoting.rkt"
         "../host/correlate.rkt"
         "correlate.rkt")

(provide compile
         compile-quote-syntax)

;; Convert an expanded syntax object to an expression that is
;; represented by a plain S-expression plus source location info (so,
;; still represented as a syntax object). The expression is compiled
;; for a particular phase, but if the expression is in a module, its
;; phase can be shifted at run time by the amount bound to
;; `phase-shift-id`. Module bindings are accessed through a namespace
;; that is bound to `ns-id` at run time.
;; The `result-used?` hint lets us drop `quote-syntax` forms that will
;; not be used in the result, so we can avoid serializing them; a value
;; of `#f` for `result-used?` means that the expression can be replaced
;; by a boolean-equivalent value if it has no side effect.
(define (compile in-s cctx [name #f] [result-used? #t])
  (let ([compile (lambda (s name result-used?) (compile s cctx name result-used?))])
    (define s (syntax-disarm in-s))
    (cond
     [(pair? (syntax-e s))
      (define phase (compile-context-phase cctx))
      (define core-sym (core-form-sym s phase))
      (case core-sym
        [(#f)
         (error "internal error; not a core form:" s "at phase:" phase)]
        [(module module*)
         (error "not a core expression form:" s)]
        [(lambda)
         (cond
          [result-used?
           (define-match m s '(lambda formals body ...+))
           (add-lambda-properties
            (correlate* s `(lambda ,@(compile-lambda (m 'formals) (m 'body) cctx)))
            name
            s)]
          [else (correlate* s `(quote ,(syntax->datum s)))])]
        [(case-lambda)
         (cond
          [result-used?
           (define-match m s '(case-lambda [formals body ...+] ...))
           (add-lambda-properties
            (correlate* s `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                                                [body (in-list (m 'body))])
                                       (compile-lambda formals body cctx))))
            name
            s)]
          [else (correlate* s `(quote ,(syntax->datum s)))])]
        [(#%app)
         (define-match m s '(#%app . rest))
         (define es (let ([es (m 'rest)])
                      (if (syntax? es)
                          (syntax->list (syntax-disarm es))
                          es)))
         (for/list ([s (in-list es)])
           (compile s #f #t))]
        [(if)
         (define-match m s '(if tst thn els))
         (define tst-e (compile (m 'tst) #f #f))
         ;; Ad hoc optimization of `(if #t ... ...)` or `(if #f ... ...)`
         ;; happens to help avoid syntax literals in pattern matching.
         (cond
          [(eq? (correlated-e tst-e) #t) (compile (m 'thn) name result-used?)]
          [(eq? (correlated-e tst-e) #f) (compile (m 'els) name result-used?)]
          [else
           (correlate* s `(if
                           ,tst-e
                           ,(compile (m 'thn) name result-used?)
                           ,(compile (m 'els) name result-used?)))])]
        [(with-continuation-mark)
         (define-match m s '(if key val body))
         (correlate* s `(with-continuation-mark
                         ,(compile (m 'key) #f #t)
                         ,(compile (m 'val) #f #t)
                         ,(compile (m 'body) name result-used?)))]
        [(begin0)
         (define-match m s '(begin0 e ...+))
         (define es (m 'e))
         (correlate* s `(begin0
                         ,(compile (car es) name result-used?)
                         ,@(for/list ([e (in-list (cdr es))])
                             (compile e #f #f))))]
        [(begin)
         (define-match m s '(begin e ...+))
         (correlate* s (compile-begin (m 'e) cctx name result-used?))]
        [(set!)
         (define-match m s '(set! id rhs))
         (correlate* s `(,@(compile-identifier (m 'id) cctx
                                               #:set-to (compile (m 'rhs) (m 'id) #t))))]
        [(let-values)
         (compile-let core-sym s cctx name #:rec? #f result-used?)]
        [(letrec-values)
         (compile-let core-sym s cctx name #:rec? #f result-used?)]
        [(#%expression)
         (define-match m s '(#%expression e))
         (compile (m 'e) name result-used?)]
        [(quote)
         (define-match m s '(quote datum))
         (define datum (syntax->datum (m 'datum)))
         (cond
          [(self-quoting-in-linklet? datum)
           (correlate* s datum)]
          [else
           (correlate* s `(quote ,datum))])]
        [(quote-syntax)
         (define-match m s '(quote-syntax datum . _))
         (if result-used?
             (compile-quote-syntax (m 'datum) cctx)
             (correlate* s `(quote ,(syntax->datum (m 'datum)))))]
        [(#%variable-reference)
         (define-match id-m s #:try '(#%variable-reference id))
         (define-match top-m s #:unless (id-m) #:try '(#%variable-reference (#%top . id)))
         (define id (or (and (id-m) (id-m 'id))
                        (and (top-m) (top-m 'id))))
         (correlate* s 
                     (if id
                         `(#%variable-reference ,(compile-identifier id cctx))
                         `(#%variable-reference)))]
        [(#%top)
         (when (compile-context-module-self cctx)
           (error "found `#%top` in a module body:" s))
         (define-match m s '(#%top . id))
         (compile-identifier (m 'id) cctx #:top? #t)]
        [else
         (error "unrecognized core form:" core-sym)])]
     [(identifier? s)
      (compile-identifier s cctx)]
     [else
      (error "bad syntax after expansion:" s)])))

(define (compile-lambda formals bodys cctx)
  (define phase (compile-context-phase cctx))
  (define gen-formals
    (let loop ([formals formals])
      (cond
       [(identifier? formals) (local-id->symbol formals phase)]
       [(syntax? formals) (loop (syntax-e formals))]
       [(pair? formals) (cons (loop (car formals))
                              (loop (cdr formals)))]
       [else null])))
  `(,gen-formals ,(compile-sequence bodys cctx #f #t)))

(define (compile-sequence bodys cctx name result-used?)
  (if (null? (cdr bodys))
      (compile (car bodys) cctx name result-used?)
      (compile-begin bodys cctx name result-used?)))

(define (compile-begin es cctx name result-used?)
  (define used-pos (sub1 (length es)))
  `(begin ,@(for/list ([e (in-list es)]
                       [i (in-naturals)])
              (define used? (= i used-pos))
              (compile e cctx (and used? name) (and used? result-used?)))))

(define (add-lambda-properties s inferred-name orig-s)
  ;; Allow pairs formed by origin tracking to provide the
  ;; same name multiple times:
  (define (simplify-name v)
    (cond
     [(pair? v)
      (define n1 (simplify-name (car v)))
      (define n2 (simplify-name (cdr v)))
      (if (eq? n1 n2) n1 v)]
     [else v]))
  ;; Get either a declared 'inferred-name or one accumulated by the compiler
  (define name (or (let ([v (simplify-name (syntax-property orig-s 'inferred-name))])
                     (and (or (symbol? v) (syntax? v) (void? v))
                          v))
                   inferred-name))
  (define named-s (if name
                      (correlated-property s
                                           'inferred-name
                                           (if (syntax? name) (syntax-e name) name))
                      s))
  (define as-method (syntax-property orig-s 'method-arity-error))
  (if as-method
      (correlated-property named-s 'method-arity-error as-method)
      named-s))

(define (compile-let core-sym s cctx name #:rec? rec? result-used?)
  (define rec? (eq? core-sym 'letrec-values))
  (define-match m s '(let-values ([(id ...) rhs] ...) body ...+))
  (define phase (compile-context-phase cctx))
  (define idss (m 'id))
  (define symss (for/list ([ids (in-list idss)])
                  (for/list ([id (in-list ids)])
                    (define sym (local-id->symbol id phase))
                    (if rec?
                        (add-undefined-error-name-property sym id)
                        sym))))
  (correlate* s
              `(,core-sym ,(for/list ([syms (in-list symss)]
                                      [ids (in-list idss)]
                                      [rhs (in-list (m 'rhs))])
                             `[,syms ,(compile rhs
                                               cctx
                                               (and (= 1 (length ids)) (car ids)))])
                ,(compile-sequence (m 'body) cctx name result-used?))))

(define (add-undefined-error-name-property sym orig-id)
  (define id (correlate* orig-id sym))
  (correlated-property id 'undefined-error-name
                       (or (syntax-property orig-id 'undefined-error-name)
                           (syntax-e orig-id))))

(define (compile-identifier s cctx #:set-to [rhs #f] #:top? [top? #f])
  (define phase (compile-context-phase cctx))
  (define normal-b (resolve+shift s phase))
  (define b
    (or normal-b
        ;; Try adding the temporary scope for top-level expansion:
        (resolve-with-top-level-bind-scope s phase cctx)
        ;; Assume a variable reference
        (make-module-binding (compile-context-self cctx)
                             phase
                             (syntax-e s))))
  (define sym
    (cond
     [(local-binding? b)
      (define sym (local-key->symbol (local-binding-key b)))
      (unless sym
        (error "missing a binding after expansion:" s))
      sym]
     [(module-binding? b)
      (define mpi (if top?
                      (compile-context-self cctx)
                      (module-binding-module b)))
      (define mod-name (module-path-index-resolve mpi))
      (define ns (compile-context-namespace cctx))
      (define mod (namespace->module ns mod-name))
      (cond
       [(and mod (module-primitive? mod))
        ;; Direct reference to a runtime primitive:
        (unless (zero? (module-binding-phase b))
          (error "internal error: non-zero phase for a primitive"))
        (when rhs
          (error "internal error: cannot assign to a primitive:" s))
        (namespace-module-instantiate! ns mpi 0)
        (define m-ns (namespace->module-namespace ns mod-name 0))
        ;; Expect each primitive to be bound:
        (module-binding-sym b)]
       [(eq? mpi (compile-context-module-self cctx))
        ;; Direct reference to a variable defined in the same module:
        (define header (compile-context-header cctx))
        (hash-ref (header-binding-sym-to-define-sym header)
                  (module-binding-sym b))]
       [else
        ;; Reference to a variable defined in another module or in an
        ;; environment (such as the top level) other than a module
        ;; context; register as a linklet import
        (register-required-variable-use! (compile-context-header cctx)
                                         mpi
                                         (module-binding-phase b)
                                         (module-binding-sym b)
                                         (or (module-binding-extra-inspector b)
                                             (syntax-inspector s)))])]
     [else
      (error "not a reference to a module or local binding:" s)]))
  (correlate* s (if rhs
                    `(set! ,sym ,rhs)
                    sym)))

(define (resolve-with-top-level-bind-scope s phase cctx)
  (define top-level-scope (compile-context-top-level-bind-scope cctx))
  (cond
   [top-level-scope
    (define tl-s (add-scope s top-level-scope))
    (resolve+shift tl-s phase)]
   [else #f]))

;; Pick a symbol to represent a local binding, given the identifier
(define (local-id->symbol id phase)
  (define b (resolve id phase))
  (unless (local-binding? b)
    (error "bad binding:" id phase b))
  (local-key->symbol (local-binding-key b)))

(define (compile-quote-syntax q cctx)
  (define pos (add-syntax-literal! (compile-context-header cctx) q))
  (cond
   [(compile-context-lazy-syntax-literals? cctx)
    (generate-lazy-syntax-literal-lookup pos)]
   [else
    (generate-eager-syntax-literal-lookup pos)]))
