#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "simple.rkt"
         "literal.rkt"
         "inline.rkt")

(provide infer-known
         lambda?)

(define (infer-known rhs defn rec? id knowns prim-knowns imports mutated)
  (cond
    [(lambda? rhs)
     (define-values (lam inlinable?) (extract-lambda rhs))
     (define arity-mask (lambda-arity-mask lam))
     (if (and inlinable?
              (or (can-inline? lam)
                  (wrap-property defn 'compiler-hint:cross-module-inline)))
         (known-procedure/can-inline arity-mask lam)
         (known-procedure arity-mask))]
    [(and (literal? rhs)
          (not (hash-ref mutated (unwrap id) #f)))
     (known-literal (unwrap-literal rhs))]
    [(and (symbol? (unwrap rhs))
          (not (hash-ref mutated (unwrap id) #f)))
     (define u-rhs (unwrap rhs))
     (cond
       [(or (hash-ref prim-knowns u-rhs #f)
            (and (not (hash-ref mutated u-rhs #f))
                 (hash-ref-either knowns imports u-rhs)))
        => (lambda (known)
             (if (known-procedure/can-inline/need-imports? known)
                 ;; can't propagate, since it loses the connection to the import
                 a-known-constant
                 known))]
       [else (and defn a-known-constant)])]
    [(and defn
          (simple? rhs prim-knowns knowns imports mutated))
     a-known-constant]
    [else #f]))
  
;; ----------------------------------------

;; Recognize forms that produce plain procedures
(define (lambda? v)
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                            (lambda? body))]
    [`(letrec-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                               (lambda? body))]
    [`(let-values ,_ ,body) (lambda? body)]
    [`(letrec-values ,_ ,body) (lambda? body)]
    [`,_ #f]))

;; Recognize forms that produce plain procedures
(define (extract-lambda v)
  (match v
    [`(lambda . ,_) (values v #t)]
    [`(case-lambda . ,_) (values v #t)]
    [`(let-values ([(,id) ,rhs]) ,body)
     (if (wrap-eq? id body)
         (extract-lambda rhs)
         (extract-lambda* body))]
    [`(letrec-values ([(,id) ,rhs]) ,body)
     (if (wrap-eq? id body)
         (extract-lambda rhs)
         (extract-lambda* body))]
    [`(let-values ,_ ,body) (extract-lambda* body)]
    [`(letrec-values ,_ ,body) (extract-lambda* body)]))

(define (extract-lambda* v)
  (define-values (lam inlinable?) (extract-lambda v))
  (values lam #f))

(define (lambda-arity-mask v)
  (match v
    [`(lambda ,args . ,_) (args-arity-mask args)]
    [`(case-lambda [,argss . ,_] ...)
     (for/fold ([mask 0]) ([args (in-list argss)])
       (bitwise-ior mask (args-arity-mask args)))]))

(define (args-arity-mask args)
  (cond
    [(wrap-null? args) 1]
    [(wrap-pair? args)
     (arithmetic-shift (args-arity-mask (wrap-cdr args)) 1)]
    [else -1]))
