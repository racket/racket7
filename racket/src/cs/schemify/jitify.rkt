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


;; FIXME: improvements needed
;;
;;   * call-with-values => don't JIT nested lambda
;;
;;   * the `letrec` handling of boxes is too conservative

(provide jitify-schemified-linklet)

(define self-id (gensym 'self))

(define (jitify-schemified-linklet v prim-knowns jitable-annotation reannotate strip-annotations)

  ;; Constucts a JIT stub:
  (define (make-jit-on-call free-vars argss v name)
    (define ids (for/list ([id (in-hash-keys free-vars)])
                  id))
    (define subs (if name
                     (let ([u-name (unwrap name)])
                       (hash-set free-vars
                                 u-name
                                 ;; Replace `name` access with `self-id`:
                                 (match (hash-ref free-vars u-name '#:direct)
                                   [`(self (variable-ref ,_))
                                    `(self (variable-ref ,self-id))]
                                   [`(self (unbox ,_))
                                    `(self (unbox ,self-id))]
                                   [`(self (unbox/check-undefined ,_ ,sym))
                                    `(self (unbox/check-undefined ,self-id ,sym))]
                                   [`,_
                                    `(self ,self-id)])))
                     free-vars))
    (define captures (for/list ([id (in-list ids)])
                       (define val (hash-ref free-vars id))
                       (match val
                         [`(variable-ref ,var) var]
                         [`(self (variable-ref ,var)) var]
                         [`,_ id])))
    `(jit-apply ',(jitable-annotation `(lambda ,captures
                                         ,(let ([v (substitute v subs prim-knowns)]
                                                [name-arg
                                                 ;; Extra representative for `name` in `captures`:
                                                 (match (and name
                                                             (hash-ref free-vars (unwrap name) #f))
                                                   [`#f #f]
                                                   [`(self (variable-ref ,id)) id]
                                                   [`,_ name])])
                                            (if name-arg
                                                `(let ([,self-id ,name-arg])
                                                   (letrec ([,name ,v])
                                                     ,name))
                                                v)))
                                      (argss->arity-mask argss)
                                      name)
                (list . ,captures)))

  ;; ----------------------------------------

  (define (top)
    ;; Match outer shape of a linklet produced by `schemify-linklet`
    ;; and lift in the linklet body:
    (let loop ([v v] [env #hasheq()])
      (match v
        [`(lambda ,args . ,body)
         (define new-body (jitify-schemified-body body (add-args env args)))
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
             (add-args env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,_ (begin (variable-set! ,var-id ,id . ,_) (void)))
             (add-args env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,id ,rhs) (add-args env id)]
            [`(define-values ,ids ,rhs) (add-args env ids)]
            [`(begin . ,vs)
             (for/fold ([env env]) ([v (in-wrap-list vs)])
               (loop v env))]
            [`,_ env]))))
    (let loop ([body body])
      (for/list ([v (in-list body)])
        (match v
          [`(define ,id ,rhs)
           (reannotate v `(define ,id ,(jitify-top-expr rhs top-env id)))]
          [`(define-values ,ids ,rhs)
           (reannotate v `(define-values ,ids ,(jitify-top-expr rhs top-env #f)))]
          [`(begin . ,vs)
           (reannotate v `(begin . ,(loop vs)))]
          [`,_ (jitify-top-expr v top-env #f)]))))

  (define (jitify-top-expr v env name)
    (define mutables (find-mutable #hasheq() v #t #hasheq()))
    (jitify-expr v env mutables #t name))

  (define (jitify-expr v env mutables check-let? name)
    (match v
      [`(lambda ,args . ,body)
       (make-jit-on-call
        (body-free-vars (add-self (remove-args env args) name) body #hasheq())
        (list args)
        v
        name)]
      [`(case-lambda [,argss . ,bodys] ...)
       (make-jit-on-call
        (for/fold ([free #hasheq()]) ([args (in-list argss)]
                                      [body (in-list bodys)])
          (body-free-vars (add-self (remove-args env args) name) body free))
        argss
        v
        name)]
      [`(let . ,_) (jitify-let v env mutables check-let? name)]
      [`(letrec . ,_) (jitify-let v env mutables check-let? name)]
      [`(letrec* . ,_) (jitify-let v env mutables check-let? name)]
      [`(begin . ,vs)
       (reannotate v `(begin . ,(jitify-body vs env mutables check-let? name)))]
      [`(begin0 . ,vs)
       (reannotate v `(begin0 . ,(jitify-body vs env mutables check-let? name)))]
      [`(if ,tst ,thn ,els)
       (reannotate
        v 
        `(if ,(jitify-expr tst env mutables #t #f)
             ,(jitify-expr thn env mutables #t name)
             ,(jitify-expr els env mutables #t name)))]
      [`(with-continuation-mark ,key ,val ,body)
       (reannotate
        v
        `(with-continuation-mark
          ,(jitify-expr key env mutables #t #f)
          ,(jitify-expr val env mutables #t #f)
          ,(jitify-expr body env mutables #t name)))]
      [`(quote ,_) v]
      [`(set! ,var ,rhs)
       (define new-rhs (jitify-expr rhs env mutables #t var))
       (define id (unwrap var))
       (if (hash-ref mutables id #f)
           (match (hash-ref env id #f)
             [`(unbox ,id) (reannotate v `(set-box! ,id ,new-rhs))]
             [`(unbox/check-undefined ,id ,_) (reannotate v `(set-box!/check-undefined ,id ,new-rhs ',var))])
           (reannotate v `(set! ,var ,new-rhs)))]
      [`(call-with-values ,proc1 ,proc2)
       (define new-proc1 (jitify-inside proc1 env mutables))
       (define new-proc2 (jitify-inside proc2 env mutables))
       (define call-with-values-id (if (and (lambda? new-proc1) (lambda? new-proc2))
                                       'call-with-values
                                       '#%call-with-values))
       (reannotate v `(,call-with-values-id ,new-proc1 ,new-proc2))]
      [`(pariah ,e)
       (reannotate v `(pariah ,(jitify-body e env mutables #f name)))]
      [`(#%app ,_ ...)
       (reannotate v (jitify-body v env mutables #t #f))]
      [`(,rator ,_ ...)
       (define new-v (jitify-body v env mutables #t #f))
       (define u (unwrap rator))
       (reannotate v (if (and (symbol? u)
                              (known-procedure? (hash-ref prim-knowns u #f)))
                         new-v
                         `(#%app . ,new-v)))]
      [`,var
       (define id (unwrap var))
       (if (hash-ref mutables id #f)
           (reannotate v (hash-ref env id))
           v)]))

  (define (jitify-inside v env mutables)
    (match v
      [`(lambda ,args . ,body)
       (reannotate v `(lambda ,args
                        (let ,(mutable-box-bindings args mutables)
                          . ,(jitify-body body (add-args/unbox env args mutables #f) mutables #t #f))))]
      [`(case-lambda [,argss . ,bodys] ...)
       (reannotate v
                   `(case-lambda
                      ,@(for/list ([args (in-list argss)]
                                   [body (in-list bodys)])
                          `[,args
                            (let ,(mutable-box-bindings args mutables)
                              . ,(jitify-body body (add-args/unbox env args mutables #f) mutables #t #f))])))]
      [`,_
       ;; passing #f for `check-let?` to encourage separate
       ;; compilation of lifted loops
       (jitify-expr v env mutables #f #f)]))

  (define (let-to-procedure v)
    (match v
      [`(lambda ,_ . ,body) v]
      [`(case-lambda . ,_) v]
      [`(,let-form ([,ids ,rhss] ...) ,body)
       (and (or (eq? let-form 'let)
                (eq? let-form 'letrec)
                (eq? let-form 'letrec*))
            (for/and ([rhs (in-list rhss)]) (lambda? rhs))
            (let-to-procedure body))]
      [`,_ #f]))

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (jitify-body vs env mutables check-let? name)
    (let loop ([vs vs])
      (cond
        [(wrap-null? vs) null]
        [(wrap-null? (wrap-cdr vs))
         (list (jitify-expr (wrap-car vs) env mutables check-let? name))]
        [else
         (cons (jitify-expr (wrap-car vs) env mutables #t #f)
               (loop (wrap-cdr vs)))])))

  (define (jitify-let v env mutables check-let? name)
    (cond
      [(and check-let? (let-to-procedure v))
       => (lambda (proc)
            (match proc
              [`(lambda ,args . ,body)
               (make-jit-on-call
                (free-vars (add-self env name) v #hasheq())
                (list args)
                v
                name)]
              [`(case-lambda [,argss . ,bodys] ...)
               (make-jit-on-call
                (free-vars (add-self env name) v #hasheq())
                argss
                v
                name)]))]
      [else
       (match v
         [`(,let-form ([,ids ,rhss] ...) . ,body)
          (define rec?
            (case (unwrap let-form)
              [(letrec letrec*) #t]
              [else #f]))
          (define local-mutables
            (if rec?
                ;; If there are any procedures on the RHSs that refer to
                ;; bindings from this letrec, then we'll need boxes.
                ;; Conservatively mark everything as mutable:
                (add-args mutables ids)
                mutables))
          (define local-env (add-args/unbox env ids local-mutables
                                            (and rec?
                                                 (not (for/and ([rhs (in-list rhss)])
                                                        (lambda? rhs))))))
          (define rhs-env (if rec? local-env env))
          (define next-check-let? (not (for/and ([rhs (in-list rhss)])
                                         (lambda? rhs))))
          (define new-body (jitify-body body local-env local-mutables next-check-let? name))
          (define (jitify-rhs id rhs)
            (jitify-expr rhs rhs-env local-mutables
                         #t
                         ;; allow direct self-reference if no mutation
                         (and rec?
                              (symbol? (unwrap id))
                              (not (hash-ref mutables (unwrap id) #f))
                              id)))
          (reannotate
           v
           (cond
             [(not (any-mutable? ids local-mutables))
              ;; Simple case: no conversion to boxes:
              `(,let-form ,(for/list ([id (in-list ids)]
                                      [rhs (in-list rhss)])
                             `[,id ,(jitify-rhs id rhs)])
                          . ,new-body)]
             [(not rec?)
              ;; Moderately simple case: wrap boxes around rhs results
              `(,let-form ,(for/list ([id (in-list ids)]
                                      [rhs (in-list rhss)])
                             (define new-rhs (jitify-rhs id rhs))
                             `[,id ,(if (hash-ref mutables (unwrap id) #f)
                                        `(box ,new-rhs)
                                        new-rhs)])
                          . ,new-body)]
             [else
              ;; Allocate boxes first, then fill in
              `(let ,(for*/list ([id (in-list ids)]
                                 #:when (hash-ref local-mutables (unwrap id) #f))
                       `[,id (box unsafe-undefined)])
                 (,let-form ,(for/list ([id (in-list ids)]
                                        [rhs (in-list rhss)])
                               (define new-rhs (jitify-rhs id rhs))
                               (if (hash-ref local-mutables (unwrap id) #f)
                                   `[,(gensym 'dummy) (set-box! ,id ,new-rhs)]
                                   `[,id ,new-rhs]))
                            . ,new-body))]))])]))
  
  (define (any-mutable? idss mutables)
    (for/or ([ids (in-list idss)])
      (hash-ref mutables (unwrap ids) #f)))

  (define (mutable-box-bindings args mutables)
    (cond
      [(wrap-null? args) '()]
      [(wrap-pair? args)
       (define id (wrap-car args))
       (define var (unwrap id))
       (define rest (mutable-box-bindings (wrap-cdr args) mutables))
       (if (hash-ref mutables var #f)
           (cons `[,id (box ,id)] rest)
           rest)]
      [else (mutable-box-bindings (list args) mutables)]))

  ;; ----------------------------------------

  (define (add-args env args [val '#:direct])
    (match args
      [`(,id . ,args)
       (add-args (hash-set env (unwrap id) val) args val)]
      [`() env]
      [`,id (hash-set env (unwrap id) val)]))

  (define (add-args/unbox env args mutables rec?)
    (define (add-one id)
      (define var (unwrap id))
      (cond
        [(not (hash-ref mutables var #f)) (hash-set env var '#:direct)]
        [rec? (hash-set env var `(unbox/check-undefined ,id ',id))]
        [else (hash-set env var `(unbox ,id))]))
    (match args
      [`(,id . ,args)
       (add-args/unbox (add-one id) args mutables rec?)]
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
         (add-args env id))]))

  (define (add-self env name)
    (if name
        (let ([u-name (unwrap name)])
          (hash-set env u-name `(self ,(hash-ref env u-name #f))))
        env))

  ;; ----------------------------------------

  (define (free-vars env v accum)
    (match v
      [`(lambda ,args . ,body)
       (body-free-vars (remove-args env args) body accum)]
      [`(case-lambda [,argss . ,bodys] ...)
       (for/fold ([accum accum]) ([args (in-list argss)]
                                  [body (in-list bodys)])
         (body-free-vars (remove-args env args) body accum))]
      [`(let . ,_) (let-free-vars env v accum)]
      [`(letrec . ,_) (let-free-vars env v accum)]
      [`(letrec* . ,_) (let-free-vars env v accum)]
      [`(begin . ,vs) (body-free-vars env vs accum)]
      [`(begin0 . ,vs) (body-free-vars env vs accum)]
      [`(if ,tst ,thn ,els)
       (free-vars env tst
                  (free-vars env thn
                             (free-vars env els
                                        accum)))]
      [`(with-continuation-mark ,key ,val ,body)
       (free-vars env key
                  (free-vars env val
                             (free-vars env body accum)))]
      [`(quote ,_) accum]
      [`(set! ,var ,rhs)
       (free-vars env rhs (free-vars env var accum))]
      [`(,rator ,_ ...)
       (body-free-vars env v accum)]
      [`,_
       (define id (unwrap v))
       (define dest (hash-ref env id #f))
       (if dest
           (hash-set accum id dest)
           accum)]))

  (define (body-free-vars env body accum)
    (for/fold ([accum accum]) ([v (in-wrap-list body)])
      (free-vars env v accum)))

  (define (let-free-vars env v accum)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define local-env (remove-args env ids))
       (define rhs-env
         (case (unwrap let-form)
           [(letrec letrec* letrec*-values) local-env]
           [else env]))
       (define body-accum
         (for/fold ([accum accum]) ([rhs (in-list rhss)])
           (free-vars rhs-env rhs accum)))
       (body-free-vars local-env body body-accum)]))

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

  ;; ----------------------------------------
  
  (define (substitute v subs knowns)
    (match v
      [`(lambda ,args . ,body)
       (reannotate v
                   `(lambda ,args . ,(substitute-body body (remove-args subs args) (remove-args knowns args))))]
      [`(case-lambda [,argss . ,bodys] ...)
       (reannotate v
                   `(case-lambda
                      ,@(for/list ([args (in-list argss)]
                                   [body (in-list bodys)])
                          `[,args . ,(substitute-body body (remove-args subs args) (remove-args knowns args))])))]
      [`(let . ,_) (substitute-let v subs knowns)]
      [`(letrec . ,_) (substitute-let v subs knowns)]
      [`(letrec* . ,_) (substitute-let v subs knowns)]
      [`(begin . ,vs)
       (reannotate v
                   `(begin . ,(substitute-body vs subs knowns)))]
      [`(begin0 . ,vs)
       (reannotate v
                   `(begin0 . ,(substitute-body vs subs knowns)))]
      [`(if ,tst ,thn ,els)
       (reannotate v `(if ,(substitute tst subs knowns)
                          ,(substitute thn subs knowns)
                          ,(substitute els subs knowns)))]
      [`(with-continuation-mark ,key ,val ,body)
       (reannotate v `(with-continuation-mark ,(substitute key subs knowns)
                                              ,(substitute val subs knowns)
                                              ,(substitute body subs knowns)))]
      [`(quote ,_) v]
      [`(set! ,var ,rhs)
       (define new-rhs (substitute rhs subs knowns))
       (let loop ([m (hash-ref subs (unwrap var) '#:direct)])
         (match m
           [`#:direct (reannotate v `(set! ,var ,new-rhs))]
           [`(self ,m) (loop m)]
           [`(variable-ref ,var-id) (reannotate v `(variable-set! ,var-id ,new-rhs '#f))]
           [`(unbox ,box-id) (reannotate v `(set-box! ,box-id ,new-rhs))]
           [`(unbox/check-undefined ,box-id ,_) (reannotate v `(set-box!/check-undefined ,box-id ,new-rhs ',var))]))]
      [`(call-with-values ,proc1 ,proc2)
       (define new-proc1 (substitute proc1 subs knowns))
       (define new-proc2 (substitute proc2 subs knowns))
       (define call-with-values-id (if (and (lambda? new-proc1) (lambda? new-proc2))
                                       'call-with-values
                                       '#%call-with-values))
       (reannotate v `(,call-with-values-id ,new-proc1 ,new-proc2))]
      [`(pariah ,e)
       (reannotate v `(pariah ,(substitute e subs knowns)))]
      [`(#%app ,_ ...)
       (reannotate v (substitute-body v subs knowns))]
      [`(,rator ,_ ...)
       (define u (unwrap rator))
       (match (and (symbol? u) (hash-ref subs u #f))
         [`(self ,_)
          ;; Keep self call as direct
          (reannotate v `(,rator . ,(substitute-body (wrap-cdr v) subs knowns)))]
         [`,_
          (define new-v (substitute-body v subs knowns))
          (reannotate v (if (or (and (symbol? u)
                                     (known-procedure? (hash-ref knowns u #f)))
                                (lambda? rator))
                            new-v
                            ;; Use `#%app` in case the rator turns out to be JITted
                            `(#%app . ,new-v)))])]
      [`,_
       (match (hash-ref subs (unwrap v) '#:direct)
         [`#:direct v]
         [`(self ,u) (reannotate v u)]
         [`,u (reannotate v u)])]))

  (define (substitute-body vs subs knowns)
    (for/list ([v (in-wrap-list vs)])
      (substitute v subs knowns)))

  (define (substitute-let v subs knowns)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define local-subs (remove-args subs ids))
       (define local-knowns (for/fold ([knowns knowns]) ([id (in-list ids)]
                                                         [rhs (in-list rhss)])
                              (if (lambda? rhs)
                                  (hash-set knowns (unwrap id) a-known-procedure)
                                  (hash-remove knowns (unwrap id)))))
       (define rec?
         (case (unwrap let-form)
           [(letrec letrec*) #f]
           [else #f]))
       (define rhs-subs (if rec? local-subs subs))
       (define rhs-knowns (if rec? local-knowns knowns))
       (reannotate v
                   `(,let-form ,(for/list ([id (in-list ids)]
                                           [rhs (in-list rhss)])
                                  `[,id ,(substitute rhs rhs-subs rhs-knowns)])
                               . ,(substitute-body body local-subs local-knowns)))]))
          
  ;; ----------------------------------------

  (define (find-mutable env v more-binds? accum)
    (match v
      [`(lambda ,args . ,body)
       (if (and (not more-binds?) (hash-empty? env))
           accum
           (body-find-mutable (remove-args env args) body #f accum))]
      [`(case-lambda [,argss . ,bodys] ...)
       (if (and (not more-binds?) (hash-empty? env))
           accum
           (for/fold ([accum accum]) ([args (in-list argss)]
                                      [body (in-list bodys)])
             (body-find-mutable (remove-args env args) body #f accum)))]
      [`(let . ,_) (find-mutable-in-let env v more-binds? accum)]
      [`(letrec . ,_) (find-mutable-in-let env v more-binds? accum)]
      [`(letrec* . ,_) (find-mutable-in-let env v more-binds? accum)]
      [`(begin . ,vs) (body-find-mutable env vs more-binds? accum)]
      [`(begin0 . ,vs) (body-find-mutable env vs more-binds? accum)]
      [`(if ,tst ,thn ,els)
       (find-mutable env tst more-binds?
                     (find-mutable env thn more-binds?
                                   (find-mutable env els more-binds? accum)))]
      [`(with-continuation-mark ,key ,val ,body)
       (find-mutable env key more-binds?
                     (find-mutable env val more-binds?
                                   (find-mutable env body more-binds? accum)))]
      [`(quote ,_) accum]
      [`(set! ,var ,rhs)
       (define id (unwrap var))
       (find-mutable env rhs more-binds? (if (hash-ref env id #f)
                                             (hash-set accum id #t)
                                             accum))]
      [`(,_ ...) (body-find-mutable env v more-binds? accum)]
      [`,_ accum]))

  (define (body-find-mutable env body more-binds? accum)
    (for/fold ([accum accum]) ([v (in-wrap-list body)])
      (find-mutable env v more-binds? accum)))

  (define (find-mutable-in-let env v more-binds? accum)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define local-env
         (if more-binds?
             (for/fold ([env env]) ([id (in-list ids)])
               (add-args env id))
             (for/fold ([env env]) ([id (in-list ids)])
               (remove-args env id))))
       (define rhs-env
         (case (unwrap let-form)
           [(letrec letrec* letrec*-values) local-env]
           [else env]))
       (body-find-mutable local-env
                          body
                          more-binds?
                          (for/fold ([accum accum]) ([id (in-list ids)]
                                                     [rhs (in-list rhss)])
                            (find-mutable rhs-env rhs more-binds? accum)))]))

  ;; ----------------------------------------
  
  (top))

;; ============================================================

(module+ main
  (require racket/pretty)
  (pretty-print
   (jitify-schemified-linklet (values ; datum->correlated
                               '(lambda (xv)
                                  (define top (letrec ([odd (lambda (x) (even x))]
                                                       [even (lambda (x) (odd x))]
                                                       [self (lambda (x) (vector (self x) self))])
                                                (odd 5)))
                                  (define top-self (lambda (x) (vector (top-self x) top-self)))
                                  (variable-set! top-self-var top-self 'const)
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    void)
                                  (define y (letrec ([f (lambda (x) (f (cons x x)))]
                                                     [g (lambda (q) (set! f g) (f q))])
                                              (list (lambda (f) (list x)))))
                                  (define x (lambda (j) j))
                                  (define whatever (begin (variable-set! xv x 'const) (void)))
                                  (define end (letrec ([w (lambda (x) (let ([proc (lambda (x) x)])
                                                                        (proc q)))]
                                                       [q q])
                                                (lambda (j) (set! q j))))))
                              (hasheq 'cons a-known-procedure)
                              vector
                              (lambda (v u) u)
                              values)))
