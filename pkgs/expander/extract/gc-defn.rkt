#lang racket/base
(require racket/list
         racket/match
         "../host/correlate.rkt"
         "../common/set.rkt"
         "../compile/side-effect.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "symbol.rkt")

(provide garbage-collect-definitions simplify-definitions)

(define (union-all . args)
  (if (null? args)
      (seteq)
      (set-union (car args) (apply union-all (cdr args)))))

(define (frees e)
  (match e
    [`(let-values ,cl ,e) 
     (define cl* (map (lambda (c)  (frees (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-union (apply union-all (map frees cl*)) (set-remove (frees e) binds))]
    [`(letrec-values ,cl ,e) 
     (define cl* (map (lambda (c)  (frees (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-remove (set-union (frees e) (apply union-all (map frees cl*))) binds)]
    [`(lambda (,args ...) ,e) (set-remove (frees e) (apply seteq args))]
    [`(lambda ,args ,e) (frees e)]
    [`(case-lambda [,args ,e] ...) (apply union-all (map frees e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark if set!))
     (apply union-all (map frees e))]
    [`(quote ,_) (seteq)]
    [e #:when (or (hash? e) (boolean? e) (number? e) (string? e) (bytes? e) (symbol? e))
       (seteq)]
    [(list app ...) (apply union-all (map frees app))]))

(define (simplify-expr e vars safe-ref?)
  (define (simp e) (simplify-expr e vars safe-ref?))
  (match e
    [`(if ,e0 ,e1 ,e2)
     (define e0* (simp e0))
     (case e0*
       [(#t) (simp e1)]
       [(#f) (simp e2)]
       [else `(if ,e0* ,(simp e1) ,(simp e2))])]
    [`(let-values ,cl ,e) 
     (define body-frees (frees e))
     (define names (apply append (map car cl)))
     (define cl* (filter-map 
                  (lambda (c)
                    (define vars (car c))
                    (define rhs (simp (cadr c)))
                    (if (and (for/and ([v (in-list vars)]) (not (set-member? body-frees v)))
                             (symbol? rhs)
                             (safe-ref? rhs))
                        #f
                        (list vars rhs)))
                  cl))
     `(let-values ,cl* ,(simplify-expr e vars (lambda (e) (or (memq e names) (safe-ref? e)))))]
    [`(letrec-values ,cl ,e) 
     (define names (apply append (map car cl)))
     (define cl* (map (lambda (c) (list (car c) (simp (cadr c)))) cl))
     `(letrec-values ,cl* ,(simplify-expr e vars (lambda (e) (or (memq e names) (safe-ref? e)))))]
    [`(lambda (,args ...) ,e) `(lambda ,args ,(simplify-expr e vars (lambda (e) (or (memq e args) (safe-ref? e)))))]
    [`(lambda ,args ,e) `(lambda ,args ,(simp e))]
    [`(case-lambda ,cl ...)
     (cons 'case-lambda (for/list ([c (in-list cl)])
                          (list (car c)
                                (simp (cadr c)))))]
    [`(variable-reference-constant? (#%variable-reference ,x))
     (if (hash-ref vars x #f) #f #t)]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark set!))
     `(,sym ,@(map simp e))]
    [(? symbol? e) e]
    [`(quote ,_) e]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
     e]
    [(list app ...) (map simp app)]))

(define (mutated-vars e)
  (match e
    [`(set! ,i ,e) (set-add (mutated-vars e) i)]
    [`(let-values ,cl ,e) 
     (define cl* (map (lambda (c)  (mutated-vars (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-union (apply union-all (map mutated-vars cl*)) (set-remove (mutated-vars e) binds))]
    [`(letrec-values ,cl ,e) 
     (define cl* (map (lambda (c) (mutated-vars (cadr c))) cl))
     (define binds (apply seteq (apply append (map car cl))))
     (set-remove (set-union (mutated-vars e) (apply union-all (map mutated-vars cl*))) binds)]
    [`(lambda ,args ,e) (mutated-vars e)]
    [`(case-lambda [,args ,e] ...) (apply union-all (map mutated-vars e))]
    [`(,sym ,e ...)
     #:when (memq sym '(begin begin0 with-continuation-mark if))
     (apply union-all (map mutated-vars e))]
    [(? symbol? e) (seteq)]
    [`(quote ,_) (seteq)]
    [e #:when (or (boolean? e) (number? e) (string? e) (bytes? e))
       (seteq)]
    [(list app ...) (apply union-all (map mutated-vars app))]
    [(? hash?) (seteq)]))

(define (simplify-definitions linklet-expr)
  (log-status "Simplifying definitions...")
  (define body (bootstrap:s-expr-linklet-body linklet-expr))
  
  (define all-mutated-vars
    (for/fold ([s (seteq)]) ([e (in-list body)])
      (cond [(defn? e)
             (set-union s (mutated-vars (defn-rhs e)))]
            [else (set-union s (mutated-vars e))])))

  (define seen-defns (make-hasheq))
  (define (ref? s) (hash-ref seen-defns s (lambda () #f)))

  (define (safe-defn? e)
    (and (defn? e)
         (or (not (any-side-effects? (defn-rhs e) (length (defn-syms e)) ref?)))))

  
  (define new-body
    (let loop ([body body])
      (cond [(null? body) null]
            [(defn? (car body))
             (for* ([d (in-list body)]
                    #:break (not (safe-defn? d))
                    [s (in-list (defn-syms d))])
               (hash-set! seen-defns s #t))
             (define e (car body))
             (define new-defn 
               (list 'define-values (defn-syms e) (simplify-expr (defn-rhs e) all-mutated-vars ref?)))
             (for ([s (in-list (defn-syms e))])
               (hash-set! seen-defns s #t))
             (cons new-defn (loop (cdr body)))]
            [else (cons (simplify-expr (car body) all-mutated-vars ref?)
                        (loop (cdr body)))])))

  (append (take linklet-expr 3)
          new-body))


(define (garbage-collect-definitions linklet-expr)
  (log-status "Removing unused definitions...")
    
  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define used-syms (make-hasheq))
  
  (define seen-defns (make-hasheq))

  ;; Map symbols to definition right-hand sides
  (define sym-to-rhs (make-hasheq))
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (for ([sym (in-list (defn-syms e))])
        (hash-set! sym-to-rhs sym (defn-rhs e)))]))
  
  ;; A "mark"-like traversal of an expression:
  (define (set-all-used! init-sym e)
    (for ([sym (in-set (all-used-symbols e))])
      (unless (hash-ref used-syms sym #f)
        (when (hash-ref sym-to-rhs sym #f)
          (log-info "adding ~s for because it's referenced by ~s" sym init-sym))
        (hash-set! used-syms sym #t)
        (set-all-used! sym (hash-ref sym-to-rhs sym #f)))))
  
  ;; Mark each body form, delaying the righthand side of definitions
  ;; if the definition has no side-effect
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (begin0
          (if (any-side-effects? (defn-rhs e) (length (defn-syms e)) (lambda (s) (hash-ref seen-defns s #f)))
              (begin 
                (log-info "adding syms for RHS side effects: ~s" (defn-syms e))
                (set-all-used! (defn-syms e) (defn-rhs e)))
              (seteq))
        (for ([s (in-list (defn-syms e))])
          (hash-set! seen-defns s #t)))]
     [else (set-all-used! '<toplevel> e)]))
  
  ;; Mark each export:
  (for ([ex+sym (in-list (bootstrap:s-expr-linklet-exports+locals linklet-expr))])
    (set-all-used! '<export> (cdr ex+sym)))
  
  (define can-remove-count
    (for/sum ([e (in-list body)])
      (cond
       [(defn? e)
        (if (for/or ([sym (in-list (defn-syms e))])
              (hash-ref used-syms sym #f))
            0
            (length (defn-syms e)))]
       [else 0])))
  (log-status "Can remove ~s of ~s defined names, keeping ~s"
              can-remove-count
              (hash-count sym-to-rhs)
              (- (hash-count sym-to-rhs) can-remove-count))
  
  (define new-body
    (for/list ([e (in-list body)]
               #:when (or (not (defn? e))
                          (for/or ([sym (in-list (defn-syms e))])
                            (hash-ref used-syms sym #f))))
      e))

  (append (take linklet-expr 3)
          new-body))

;; ----------------------------------------
  
(define (defn? e)
  (and (pair? e)
       (eq? (car e) 'define-values)))


(define defn-syms cadr)
(define defn-rhs caddr)
