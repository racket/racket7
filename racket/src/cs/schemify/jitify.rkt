#lang racket/base
(require "match-annotation.rkt"
         "wrap-annotation.rkt"
         "known.rkt")

;; Convert `lambda`s to make them JITted on demand.

;; JITted-on-demand code must be a constant across all invocations of
;; the linklet. An environment maps a variables that needs to be passed
;; into the JITted code:
;;
;;   * id -> '#:direct --- ready by the time it's needed and immutable
;;
;;   * id -> expression --- rewrite access to expression
;;
;;   * id -> `(self ,m) --- a reference to the enclosing function; can
;;                          use directly in rator position, otherwise
;;                          use m
;;
;; The result has no `lambda` forms left except as arguments to
;; `[#%]call-with-values`, either immediate or under `let[rec[*]]`.

(provide jitify-schemified-linklet)

(define (jitify-schemified-linklet v prim-knowns jitable-annotation reannotate strip-annotations)

  ;; Constucts a JIT stub:
  (define (make-jit-on-call free-vars argss v name env)
    (define ids (for/list ([id (in-hash-keys free-vars)])
                  id))
    (define (extract-id m id)
      (match m
        [`(variable-ref ,var) var]
        [`(unbox ,var) var]
        [`(unbox/check-undefined ,var ,_) var]
        [`(self ,m ,orig-id) orig-id]
        [`(self ,m) (extract-id m id)]
        [`,_ id]))
    (define captures (for/list ([id (in-list ids)])
                       (extract-id (hash-ref env id) id)))
    (define jitted-proc
      (or (match (and (hash-ref free-vars (unwrap name) #f)
                      (hash-ref env (unwrap name) #f))
            [`(self ,m ,orig-name)
             (cond
               [(eq? orig-name name)
                (define self-id (extract-id m name))
                `(let ([,self-id ,orig-name])
                   (letrec ([,name ,v])
                     ,name))]
               [else #f])]
            [`,_ #f])
          (cond
            [name `(letrec ([,name ,v])
                     ,name)]
            [else v])))
    (define arity-mask (argss->arity-mask argss))
    (cond
      [(null? captures)
       `(jit-extract-closed ',(jitable-annotation jitted-proc
                                                  arity-mask
                                                  name))]
      [`((jit-extract ',(jitable-annotation `(lambda ,captures
                                               ,jitted-proc)
                                            arity-mask
                                            name))
                   . ,captures)]))

  ;; ----------------------------------------

  (define (top)
    ;; Match outer shape of a linklet produced by `schemify-linklet`
    ;; and lift in the linklet body:
    (let loop ([v v] [env #hasheq()])
      (match v
        [`(lambda ,args . ,body)
         (define new-body (jitify-schemified-body body (add-args env args '#hasheq())))
         (if (for/and ([old (in-list body)]
                       [new (in-list new-body)])
               (eq? old new))
             v
             (reannotate v `(lambda ,args . ,new-body)))]
        [`(let* ,bindings ,body)
         (define new-body (loop body (add-bindings env bindings)))
         (if (eq? body new-body)
             v
             (reannotate v `(let* ,bindings ,new-body)))])))

  (define (jitify-schemified-body body env)
    (define top-env
      (for/fold ([env env]) ([v (in-list body)])
        (let loop ([v v] [env env])
          (match v
            [`(variable-set! ,var-id ,id . ,_)
             (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,_ (begin (variable-set! ,var-id ,id . ,_) (void)))
             (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,id ,rhs) (add-args env id #hasheq())]
            [`(define-values ,ids ,rhs) (add-args env ids #hasheq())]
            [`(begin . ,vs)
             (for/fold ([env env]) ([v (in-wrap-list vs)])
               (loop v env))]
            [`,_ env]))))
    (let loop ([body body])
      (for/list ([v (in-list body)])
        (match v
          [`(variable-set! ,var-id ,id . ,_) v]
          [`(define ,_ (begin (variable-set! ,var-id ,id . ,_) (void))) v]
          [`(define ,id ,rhs)
           ;; If there's a direct reference to `id` in `rhs`, then
           ;; `id` must not be mutable
           (define self-env (add-self top-env #hasheq() id))
           (reannotate v `(define ,id ,(jitify-top-expr rhs self-env id)))]
          [`(define-values ,ids ,rhs)
           (reannotate v `(define-values ,ids ,(jitify-top-expr rhs top-env #f)))]
          [`(begin . ,vs)
           (reannotate v `(begin . ,(loop vs)))]
          [`,_ (jitify-top-expr v top-env #f)]))))

  (define (jitify-top-expr v env name)
    ;; The `mutables` table doesn't track shadowing on the assumption
    ;; that local variable names are sufficiently distuished to prevent
    ;; one mutable variable from polluting another in a different scope
    (define mutables (find-mutable #hasheq() v #hasheq()))
    (define-values (new-v free) (jitify-expr v env mutables #hasheq() #f name #f))
    new-v)

  (define (jitify-expr v env mutables free called? name in-name)
    (match v
      [`(lambda ,args . ,body)
       (define self-env (activate-self (deactivate-self env in-name) name))
       (define-values (new-body lam-body-free)
         (jitify-body body (add-args self-env args mutables) mutables #hasheq() #f #f (or name '#:anonymous)))
       (define lam-free (remove-args lam-body-free args))
       (define new-v (reannotate v `(lambda ,args . ,(mutable-box-bindings args mutables new-body))))
       (values (if called?
                   new-v
                   (make-jit-on-call lam-free (list args) new-v name self-env))
               (union-free free lam-free))]
      [`(case-lambda [,argss . ,bodys] ...)
       (define self-env (activate-self (deactivate-self env in-name) name))
       (define-values (rev-new-bodys lam-free)
         (for/fold ([rev-new-bodys '()] [lam-free #hasheq()]) ([args (in-list argss)]
                                                               [body (in-list bodys)])
           (define-values (new-body lam-body-free)
             (jitify-body body (add-args self-env args mutables) mutables #hasheq() #f #f (or name '#:anonymous)))
           (values (cons new-body rev-new-bodys)
                   (union-free (remove-args lam-body-free args)
                               lam-free))))
       (define new-v (reannotate v
                                 `(case-lambda
                                    ,@(for/list ([args (in-list argss)]
                                                 [body (in-list (reverse rev-new-bodys))])
                                        `[,args . ,(mutable-box-bindings args mutables body)]))))
       (values (if called?
                   new-v
                   (make-jit-on-call lam-free argss new-v name self-env))
               (union-free free lam-free))]
      [`(let . ,_) (jitify-let v env mutables free called? name in-name)]
      [`(letrec . ,_) (jitify-let v env mutables free called? name in-name)]
      [`(letrec* . ,_) (jitify-let v env mutables free called? name in-name)]
      [`(begin . ,vs)
       (define-values (new-body new-free) (jitify-body vs env mutables free called? name in-name))
       (values (reannotate v `(begin . ,new-body))
               new-free)]
      [`(begin0 . ,vs)
       (define-values (new-body new-free) (jitify-body vs env mutables free called? name in-name))
       (values (reannotate v `(begin0 . ,new-body))
               new-free)]
      [`(pariah ,e)
       (define-values (new-e new-free) (jitify-expr e env mutables free called? name in-name))
       (values (reannotate v `(pariah ,new-e))
               new-free)]
      [`(if ,tst ,thn ,els)
       (define-values (new-tst new-free/tst) (jitify-expr tst env mutables free #f #f in-name))
       (define-values (new-thn new-free/thn) (jitify-expr thn env mutables new-free/tst called? name in-name))
       (define-values (new-els new-free/els) (jitify-expr els env mutables new-free/thn called? name in-name))
       (values (reannotate v `(if ,new-tst ,new-thn ,new-els))
               new-free/els)]
      [`(with-continuation-mark ,key ,val ,body)
       (define-values (new-key new-free/key) (jitify-expr key env mutables free #f #f in-name))
       (define-values (new-val new-free/val) (jitify-expr val env mutables new-free/key #f #f in-name))
       (define-values (new-body new-free/body) (jitify-expr body env mutables new-free/val called? name in-name))
       (values (reannotate v `(with-continuation-mark ,new-key ,new-val ,new-body))
               new-free/body)]
      [`(quote ,_) (values v free)]
      [`(set! ,var ,rhs)
       (define-values (new-rhs new-free) (jitify-expr rhs env mutables free #f var in-name))
       (cond
         [(not in-name)
          ;; Not under lambda: don't rewrite references to definitions
          (values `(set! ,var ,new-rhs)
                  new-free)]
         [else
          (define id (unwrap var))
          (define dest (hash-ref env id #f))
          (define newer-free (if dest
                                 (hash-set new-free id dest)
                                 new-free))
          (define new-v
            (match (hash-ref env id '#:direct)
              [`#:direct (reannotate v `(set! ,var ,new-rhs))]
              [`(self ,m . ,_) (error 'set! "[internal error] self-referenceable ~s" id)]
              [`(variable-ref ,var-id) (reannotate v `(variable-set! ,var-id ,new-rhs '#f))]
              [`(unbox ,box-id) (reannotate v `(set-box! ,box-id ,new-rhs))]
              [`(unbox/check-undefined ,box-id ,_) (reannotate v `(set-box!/check-undefined ,box-id ,new-rhs ',var))]))
          (values new-v newer-free)])]
      [`(call-with-values ,proc1 ,proc2)
       (define-values (new-proc1 new-free1) (jitify-expr proc1 env mutables free #t #f in-name))
       (define-values (new-proc2 new-free2) (jitify-expr proc2 env mutables new-free1 #t #f in-name))
       (define call-with-values-id (if (and (lambda? new-proc1) (lambda? new-proc2))
                                       'call-with-values
                                       '#%call-with-values))
       (values (reannotate v `(,call-with-values-id ,new-proc1 ,new-proc2))
               new-free2)]
      [`(#%app ,_ ...)
       (define-values (new-vs new-free) (jitify-body (wrap-cdr v) env mutables free #f #f in-name))
       (values (reannotate v `(#%app . ,new-vs))
               new-free)]
      [`(,rator ,_ ...)
       (define u (unwrap rator))
       (match (and (symbol? u) (hash-ref env u #f))
         [`(self ,_ ,orig-id)
          ;; Keep self call as direct
          (define-values (new-vs new-free)
            (jitify-body (wrap-cdr v) env mutables free #f #f in-name))
          (values (reannotate v `(,rator . ,new-vs))
                  new-free)]
         [`,x
          (define-values (new-vs new-free)
            (jitify-body v env mutables free #f #f in-name))
          (values (reannotate v new-vs)
                  new-free)])]
      [`,var
       (cond
         [(not in-name)
          ;; Not under lambda: don't rewrite references to definitions
          (values var free)]
         [else
          (define id (unwrap var))
          (define dest (hash-ref env id #f))
          (define new-var
            (match dest
              [`#f var]
              [`#:direct var]
              [`(self ,u . ,_) (reannotate v u)]
              [`,u (reannotate v u)]))
          (define new-free
            (if dest
                (hash-set free id dest)
                free))
          (values new-var
                  new-free)])]))

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (jitify-body vs env mutables free called? name in-name)
    (let loop ([vs vs] [free free])
      (cond
        [(wrap-null? vs) (values null free)]
        [(wrap-null? (wrap-cdr vs))
         (define-values (new-v new-free)
           (jitify-expr (wrap-car vs) env mutables free called? name in-name))
         (values (list new-v) new-free)]
        [else
         (define-values (new-v new-free)
           (jitify-expr (wrap-car vs) env mutables free #f #f in-name))
         (define-values (new-rest newer-free)
           (loop (wrap-cdr vs) new-free))
         (values (cons new-v new-rest)
                 newer-free)])))

  (define (jitify-let v env mutables free called? name in-name)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define rec?
         (case (unwrap let-form)
           [(letrec letrec*) #t]
           [else #f]))
       (define rhs-env (if rec?
                           (add-args/unbox env ids mutables
                                           (lambda (var) #t)
                                           (not (for/and ([rhs (in-list rhss)])
                                                  (lambda? rhs))))
                           env))
       (define-values (rev-new-rhss rhs-free)
         (for/fold ([rev-new-rhss '()] [free #hasheq()]) ([id (in-list ids)]
                                                          [rhs (in-list rhss)])
           (define self-env
             (if rec?
                 (add-self rhs-env mutables id)
                 rhs-env))
           (define-values (new-rhs rhs-free)
             (jitify-expr rhs self-env mutables free #f id in-name))
           (values (cons new-rhs rev-new-rhss) rhs-free)))
       (define local-env
         (add-args/unbox env ids mutables
                         (lambda (var) (and rec? (hash-ref rhs-free var #f)))
                         #f))
       (define-values (new-body new-free)
         (jitify-body body local-env mutables (union-free free rhs-free) called? name in-name))
       (define new-v
         (cond
           [(not rec?)
            ;; Wrap boxes around rhs results as needed:
            `(,let-form ,(for/list ([id (in-list ids)]
                                    [new-rhs (in-list (reverse rev-new-rhss))])
                           `[,id ,(if (hash-ref mutables (unwrap id) #f)
                                      `(box ,new-rhs)
                                      new-rhs)])
                        . ,new-body)]
           [else
            ;; Allocate boxes first, then fill in
            `(let ,(for*/list ([id (in-list ids)]
                               #:when (hash-ref rhs-free (unwrap id) #f))
                     `[,id (box unsafe-undefined)])
               ;; Using nested `let`s to force left-to-right
               ,(for/fold ([body (body->expr new-body)]) ([id (in-list (reverse ids))]
                                                          [new-rhs (in-list rev-new-rhss)])
                  `(let (,(cond
                            [(hash-ref rhs-free (unwrap id) #f)
                             `[,(gensym 'ignored) (set-box! ,id ,new-rhs)]]
                            [else `[,id ,new-rhs]]))
                     ,body)))]))
       (values (reannotate v new-v)
               (remove-args new-free ids))]))

  (define (mutable-box-bindings args mutables body)
    (define bindings
      (let loop ([args args])
        (cond
          [(wrap-null? args) null]
          [(wrap-pair? args)
           (define id (wrap-car args))
           (define var (unwrap id))
           (define rest (loop (wrap-cdr args)))
           (if (hash-ref mutables var #f)
               (cons `[,id (box ,id)] rest)
               rest)]
          [else (loop (list args))])))
    (if (null? bindings)
        body
        `((let ,bindings . ,body))))

  ;; ----------------------------------------

  (define (add-args env args mutables)
    (define (add-one id)
      (define u (unwrap id))
      (define val (if (hash-ref mutables u #f)
                      `(unbox ,id)
                      '#:direct))
      (hash-set env u val))
    (match args
      [`(,id . ,args)
       (add-args (add-one id) args mutables)]
      [`() env]
      [`,id (add-one id)]))

  (define (add-args/unbox env args mutables var-rec? maybe-undefined?)
    (define (add-one id)
      (define var (unwrap id))
      (cond
        [maybe-undefined? (hash-set env var `(unbox/check-undefined ,id ',id))]
        [(not (or (var-rec? var) (hash-ref mutables var #f))) (hash-set env var '#:direct)]
        [else (hash-set env var `(unbox ,id))]))
    (match args
      [`(,id . ,args)
       (add-args/unbox (add-one id) args mutables var-rec? maybe-undefined?)]
      [`() env]
      [`,id (add-one id)]))

  (define (remove-args env args)
    (match args
      [`(,id . ,args)
       (remove-args (hash-remove env (unwrap id)) args)]
      [`() env]
      [`,id (hash-remove env (unwrap id))]))

  (define (add-bindings env bindings)
    (match bindings
      [`([,ids ,_] ...)
       (for/fold ([env env]) ([id (in-list ids)])
         (add-args env id #hasheq()))]))

  (define (add-self env mutables name)
    (define u (unwrap name))
    (cond
      [(hash-ref mutables u #f)
       env]
      [else
       (hash-set env u `(self ,(hash-ref env u '#:direct)))]))

  (define (activate-self env name)
    (cond
      [name
       (define (genself) (gensym 'self))
       (define u (unwrap name))
       (define new-m
         (match (hash-ref env u #f)
           [`(self #:direct)
            `(self ,(genself) ,name)]
           [`(self (variable-ref ,orig-id))
            `(self (variable-ref ,orig-id) ,orig-id)]
           [`(self (unbox ,orig-id))
            `(self (unbox ,(genself)) ,orig-id)]
           [`(self (unbox/check-undefined ,orig-id ,sym))
            `(self (unbox/check-undefined ,(genself) ,sym) ,orig-id)]
           [`,_ #f]))
       (if new-m
           (hash-set env u new-m)
           env)]
      [else env]))

  (define (deactivate-self env name)
    (cond
      [name
       (define u (unwrap name))
       (match (hash-ref env u #f)
         [`(self ,m ,_) (hash-set env u m)]
         [`,_ env])]
      [else env]))

  ;; ----------------------------------------

  (define (argss->arity-mask argss)
    (for/fold ([mask 0]) ([args (in-list argss)])
      (bitwise-ior mask
                   (let loop ([args args] [count 0])
                     (cond
                       [(wrap-null? args) (arithmetic-shift 1 count)]
                       [(wrap-pair? args) (loop (wrap-cdr args) (add1 count))]
                       [else (bitwise-xor -1 (sub1 (arithmetic-shift 1 count)))])))))

  (define (de-dot args)
    (cond
      [(wrap-pair? args) (cons (wrap-car args)
                               (de-dot (wrap-cdr args)))]
      [else (list args)]))

  (define (union-free a b)
    (cond
      [((hash-count b) . < . (hash-count a)) (union-free b a)]
      [else
       (for/fold ([b b]) ([(k v) (in-hash a)])
         (hash-set b k v))]))

  (define (body->expr body)
    (cond
      [(and (wrap-pair? body) (wrap-null? (wrap-cdr body)))
       (wrap-car body)]
      [else `(begin . ,body)]))

  ;; ----------------------------------------

  (define (find-mutable env v accum)
    (match v
      [`(lambda ,args . ,body)
       (body-find-mutable (add-args env args #hasheq()) body accum)]
      [`(case-lambda [,argss . ,bodys] ...)
       (for/fold ([accum accum]) ([args (in-list argss)]
                                  [body (in-list bodys)])
         (body-find-mutable (add-args env args #hasheq()) body accum))]
      [`(let . ,_) (find-mutable-in-let env v accum)]
      [`(letrec . ,_) (find-mutable-in-let env v accum)]
      [`(letrec* . ,_) (find-mutable-in-let env v accum)]
      [`(begin . ,vs) (body-find-mutable env vs accum)]
      [`(begin0 . ,vs) (body-find-mutable env vs accum)]
      [`(if ,tst ,thn ,els)
       (find-mutable env tst
                     (find-mutable env thn
                                   (find-mutable env els accum)))]
      [`(with-continuation-mark ,key ,val ,body)
       (find-mutable env key
                     (find-mutable env val
                                   (find-mutable env body accum)))]
      [`(quote ,_) accum]
      [`(set! ,var ,rhs)
       (define id (unwrap var))
       (find-mutable env rhs (if (hash-ref env id #f)
                                 (hash-set accum id #t)
                                 accum))]
      [`(,_ ...) (body-find-mutable env v accum)]
      [`,_ accum]))

  (define (body-find-mutable env body accum)
    (for/fold ([accum accum]) ([v (in-wrap-list body)])
      (find-mutable env v accum)))

  (define (find-mutable-in-let env v accum)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define local-env
         (for/fold ([env env]) ([id (in-list ids)])
           (add-args env id #hasheq())))
       (define rhs-env
         (case (unwrap let-form)
           [(letrec letrec* letrec*-values) local-env]
           [else env]))
       (body-find-mutable local-env
                          body
                          (for/fold ([accum accum]) ([id (in-list ids)]
                                                     [rhs (in-list rhss)])
                            (find-mutable rhs-env rhs accum)))]))

  ;; ----------------------------------------
  
  (top))

;; ============================================================

(module+ main
  (require racket/pretty)
  (pretty-print
   (jitify-schemified-linklet (values ; datum->correlated
                               '(lambda (iv xv do-immediate)
                                  (define top (letrec ([odd (lambda (x) (even x))]
                                                       [even (lambda (x) (odd x))]
                                                       [selfx (lambda (x) (selfx x))]
                                                       [selfy (lambda (x) (vector (selfy x) selfy))])
                                                (odd 5)))
                                  (define top-selfx (lambda (x) (top-selfx x)))
                                  (variable-set! top-selfx-var top-selfx 'const)
                                  (define top-selfy (lambda (x) (vector (top-selfy x) top-selfy)))
                                  (variable-set! top-selfy-var top-selfy 'const)
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    (lambda (z w) 10))
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    (letrec ([selfz (lambda (z) (selfz (selfz z)))])
                                      (lambda (z w) (selfz w))))
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    void)
                                  (define y (letrec ([f (lambda (x) (f (cons x x)))]
                                                     [g (lambda (q) (set! f g) (f q))])
                                              (list (lambda (f) (list x)))))
                                  (define x (lambda (j) j))
                                  (define x2 (lambda () (letrec ([other (lambda () (other iv))])
                                                          other)))
                                  (define whatever (begin (variable-set! xv x 'const) (void)))
                                  (define end (letrec ([w (lambda (x) (let ([proc (lambda (x) x)])
                                                                        (proc q)))]
                                                       [q q])
                                                (lambda (j) (set! q j))))
                                  (define topz (letrec ([helper (lambda (x)
                                                                  (helper (topz x)))])
                                                 (lambda (y) (helper y))))
                                  (variable-set! topz-var topz 'const)
                                  (do-immediate topz)
                                  (define sets-arg (lambda (x)
                                                     (values (lambda () (set! x (add1 x)))
                                                             (lambda () x))))
                                  (letrec ([outer
                                            (lambda (x)
                                              (letrec ([inner
                                                        (lambda (y)
                                                          (outer y))])
                                                (inner x)))])
                                    (outer 5))))
                              (hasheq 'cons a-known-procedure)
                              vector
                              (lambda (v u) u)
                              values)))
