#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt")

(provide init-inline-fuel
         can-inline?
         inline-clone
         known-inline->export-known)

(define inline-factor 3)
(define init-inline-fuel 8)

(define (can-inline? v)
  (match v
    [`(lambda ,args . ,bodys)
     (smaller-than? bodys (* inline-factor (args-size args)))]
    [`(case-lambda [,argss . ,bodyss] ...)
     (for/and ([args (in-list argss)]
               [bodys (in-list bodyss)])
       (smaller-than? bodys (* inline-factor (args-size args))))]
    [`,_ #f]))

(define (args-size args)
  (cond
    [(wrap-pair? args) (+ 1 (args-size (wrap-cdr args)))]
    [else 1]))

(define (smaller-than? v size)
  (positive?
   (let loop ([v v] [size size])
     (cond
       [(zero? size) 0]
       [(wrap-pair? v)
        (loop (wrap-cdr v) (loop (wrap-car v) size))]
       [else (sub1 size)]))))

;; ----------------------------------------

;; All binding identifiers in a clone must be fresh to stay consistent
;; with the unique-variable invariant of expanded/schemified form.

(define (inline-clone v mutated reannotate)
  (match v
    [`(lambda ,args . ,bodys)
     (define-values (new-args env) (clone-args args '() mutated))
     `(lambda ,new-args . ,(clone-body bodys env mutated reannotate))]
    [`(case-lambda [,argss . ,bodyss] ...)
     `(case-lambda ,@(for/list ([args (in-list argss)]
                                [bodys (in-list bodyss)])
                       (define-values (new-args env) (clone-args args '() mutated))
                       `[,new-args . ,(clone-body bodys env mutated reannotate)]))]))

(define (clone-args args base-env mutated)
  (define env
    (let loop ([args args])
      (cond
        [(wrap-null? args) base-env]
        [(wrap-pair? args)
         (define u (unwrap (wrap-car args)))
         (define g (gensym u))
         (define m (hash-ref mutated u #f))
         (when m
           (hash-set! mutated g m))
         (cons (cons u g)
               (loop (wrap-cdr args)))]
        [else
         (define u (unwrap args))
         (list (cons u (gensym u)))])))
  (values (let loop ([args args] [env env])
            (cond
              [(wrap-null? args) '()]
              [(wrap-pair? args)
               (define u (unwrap (wrap-car args)))
               (cons (cdr (car env))
                     (loop (wrap-cdr args) (cdr env)))]
              [else
               (cdr (car env))]))
          env))

(define (clone-body l env mutated reannotate)
  (for/list ([e (in-wrap-list l)])
    (clone-expr e env mutated reannotate)))

(define (clone-let v env mutated reannotate)
  (match v
    [`(,let-id ([,idss ,rhss] ...) ,bodys ...)
     (define-values (rev-new-idss new-env)
       (for/fold ([rev-new-idss null] [env env]) ([ids (in-list idss)])
         (define-values (new-ids new-env) (clone-args ids env mutated))
         (values (cons new-ids rev-new-idss) new-env)))
     `(,let-id ,(for/list ([ids (in-list (reverse rev-new-idss))]
                           [rhs (in-list rhss)])
                  `[,ids ,(clone-expr rhs new-env mutated reannotate)])
               . ,(clone-body bodys new-env mutated reannotate))]))

(define (clone-expr v env mutated reannotate)
  (reannotate
   v
   (match v
     [`(lambda ,args . ,bodys)
      `(lambda ,args . ,(clone-body bodys env mutated reannotate))]
     [`(case-lambda [,argss . ,bodyss] ...)
      `(case-lambda ,@(for/list ([args (in-list argss)]
                                 [bodys (in-list bodyss)])
                        `[,args . ,(clone-body bodys env mutated reannotate)]))]
     [`(quote ,_) v]
     [`(let-values . ,_) (clone-let v env mutated reannotate)]
     [`(letrec-values . ,_) (clone-let v env mutated reannotate)]
     [`(if ,tst ,thn ,els)
      `(if ,(clone-expr tst env mutated reannotate)
           ,(clone-expr thn env mutated reannotate)
           ,(clone-expr els env mutated reannotate))]
     [`(with-continuation-mark ,key ,val ,body)
      `(with-continuation-mark ,(clone-expr key env mutated reannotate)
                               ,(clone-expr val env mutated reannotate)
                               ,(clone-expr body env mutated reannotate))]
     [`(begin ,exps ...)
      `(begin . ,(clone-body exps env mutated reannotate))]
     [`(begin0 ,exps ...)
      `(begin0 . ,(clone-body exps env mutated reannotate))]
     [`(set! ,id ,rhs)
      `(set! ,id ,(clone-expr rhs env mutated reannotate))]
     [`(#%variable-reference) v]
     [`(#%variable-reference ,id)
      `(#%variable-reference ,(clone-expr id env mutated reannotate))]
     [`(,rator . ,_)
      (clone-body v env mutated reannotate)]
     [`,_
      (let ([u-v (unwrap v)])
        (cond
          [(symbol? u-v)
           (lookup env u-v v)]
          [else v]))])))

(define (lookup env sym default)
  (cond
    [(null? env) default]
    [(eq? (caar env) sym)
     (cdar env)]
    [else (lookup (cdr env) sym default)]))

;; ----------------------------------------

(define (known-inline->export-known k prim-knowns imports exports)
  (cond
    [(known-procedure/can-inline? k)
     ;; For now, export inlinable only if there are no
     ;; references besides locals and primitives
     (if (any-free-variables? (known-procedure/can-inline-expr k) prim-knowns '())
         a-known-procedure
         k)]
    [else k]))

(define (any-free-variables? v prim-knowns env)
  (match v
    [`(lambda ,args . ,bodys)
     (body-any-free-variables? bodys prim-knowns (add-args env args))]
    [`(case-lambda [,argss . ,bodyss] ...)
     (for/or ([args (in-list argss)]
              [bodys (in-list bodyss)])
       (body-any-free-variables? bodys prim-knowns (add-args env args)))]
    [`(quote ,_) #f]
    [`(let-values . ,_) (let-any-free-variables? v prim-knowns env)]
    [`(letrec-values . ,_) (let-any-free-variables? v prim-knowns env)]
    [`(if ,tst ,thn ,els)
     (or (any-free-variables? tst prim-knowns env)
         (any-free-variables? thn prim-knowns env)
         (any-free-variables? els prim-knowns env))]
    [`(with-continuation-mark ,key ,val ,body)
     (or (any-free-variables? key prim-knowns env)
         (any-free-variables? val prim-knowns env)
         (any-free-variables? body prim-knowns env))]
    [`(begin ,exps ...)
     (body-any-free-variables? exps prim-knowns env)]
    [`(begin0 ,exps ...)
     (body-any-free-variables? exps prim-knowns env)]
    [`(set! ,id ,rhs)
     (any-free-variables? rhs prim-knowns env)]
    [`(#%variable-reference . ,_)
     ;; Cannot inline a variable reference
     #t]
    [`(,rator . ,_)
     (body-any-free-variables? v prim-knowns env)]
    [`,_
     (let ([u-v (unwrap v)])
       (cond
         [(symbol? u-v)
          (not (or (memq u-v env)
                   (hash-ref prim-knowns u-v #f)))]
         [else #f]))]))

(define (body-any-free-variables? l prim-knowns env)
  (for/or ([e (in-wrap-list l)])
    (any-free-variables? e prim-knowns env)))

(define (let-any-free-variables? v prim-knowns env)
  (match v
    [`(,let-id ([,idss ,rhss] ...) ,bodys ...)
     (define new-env (for*/fold ([env env]) ([ids (in-list idss)]
                                             [id (in-list ids)])
                       (cons (unwrap id) env)))
     (or (for/or ([rhs (in-list rhss)])
           (any-free-variables? rhs prim-knowns new-env))
         (body-any-free-variables? bodys prim-knowns new-env))]))

(define (add-args env args)
  (cond
    [(wrap-null? args) env]
    [(wrap-pair? args)
     (add-args (cons (unwrap (wrap-car args)) env)
               (wrap-cdr args))]
    [else
     (cons (unwrap args) env)]))
