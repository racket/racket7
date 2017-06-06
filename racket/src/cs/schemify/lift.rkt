#lang racket/base
(require "match-annotation.rkt"
         "wrap-annotation.rkt")

;; Reduces closure allocation by lifting bindings that are only used
;; in calls that have the right number of arguments.

(provide lift-in-schemified-linklet
         lift-in-schemified-body)

;; An identifier registered in `lifts` is one of
;;
;;  * `liftable` - a function binding that is (so far) only referenced
;;                 in an application position with a correct number fo
;;                 arguments, so each call can supply the free
;;                 variables of the function and the closure
;;                 allocation (if any) can be lifted to the top level
;;
;; * `indirected` - a variable that is `set!`ed or referenced via
;;                `#%variable-reference`, which means that it can't be
;;                replaced by an argument if it appears as a free
;;                variable in a liftable function; instead, the
;;                argument must be an accessor, mutator, and/or
;;                variable reference
;;
;; * `mutator` - records that a synthesized identifier is a mutator,
;;                and maps back to the original identifier; these are
;;                removed after the first pass
;;
;; * `var-ref` - similar to `mutated`, but for a
;;               `#%variable-reference`d variable
;;
;; There's nothing analogous to `mutator` and `var-ref` for
;; synthesized accessors, because they're relevant only for the second
;; pass and recorded in an `indirected`.

(struct liftable (expr ; a `lambda` or `case-lambda` RHS of the binding
                  [frees #:mutable] ; set of variables free in `expr`, plus any lifted bindings
                  [binds #:mutable])) ; set of variables bound in `expr`

(struct indirected (accessor ; symbol for synthesized accessor binding
                    mutator  ; symbol for synthesized mutator binding
                    variable-reference) ; synbol for synthesized variable reference
  #:mutable)

(struct mutator (orig)) ; `orig` maps back to the original identifier
(struct var-ref (orig)) ; ditto

;; As we traverse expressions, we thread through free- and
;; bound-variable sets
(define empty-frees+binds (cons #hasheq() #hasheq()))

(define (lift-in-schemified-linklet v reannotate strip-annotations)
  ;; Match outer shape of a linklet produced by `schemuify-linklet`
  ;; and lift in the linklet body:
  (let loop ([v v])
    (match v
      [`(lambda ,args . ,body)
       (define new-body (lift-in-schemified-body body reannotate strip-annotations))
       (if (for/and ([old (in-list body)]
                     [new (in-list new-body)])
             (eq? old new))
           v
           `(lambda ,args . ,new-body))]
      [`(let* ,bindings ,body)
       (define new-body (loop body))
       (if (eq? body new-body)
           v
           `(let* ,bindings ,new-body))])))

(define (lift-in-schemified-body body reannotate strip-annotations)
  (for/list ([v (in-list body)])
    (lift-in-schemified v reannotate strip-annotations)))

(define (lift-in-schemified v reannotate strip-annotations)
  ;; Quick pre-check: do any lifts appear to be possible?
  (define (lift-in? v)
    (match v
      [`(define ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(define-values ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(begin . ,vs)
       (for/or ([v (in-wrap-list vs)])
         (lift-in? v))]
      [`,_ (lift-in-expr? v)]))
    
  (define (lift-in-expr? v)
    (match v
      [`(lambda ,_ . ,body)
       (lift?/seq body)]
      [`(case-lambda [,_ . ,bodys] ...)
       (for/or ([body (in-list bodys)])
         (lift?/seq body))]
      [`(let . ,_) (lift-in-let? v)]
      [`(letrec . ,_) (lift-in-let? v)]
      [`(letrec* . ,_) (lift-in-let? v)]
      [`(let-values . ,_) (lift-in-let? v)]
      [`(letrec-values . ,_) (lift-in-let? v)]
      [`(begin . ,vs)
       (for/or ([v (in-wrap-list vs)])
         (lift-in-expr? v))]
      [`(if ,tst ,thn ,els)
       (or (lift-in-expr? tst) (lift-in-expr? thn) (lift-in-expr? els))]
      [`(with-continuation-mark ,key ,val ,body)
       (or (lift-in-expr? key) (lift-in-expr? val) (lift-in-expr? body))]
      [`(quote ,_) #f]
      [`(#%variable-reference ,_) #f]
      [`(#%variable-reference) #f]
      [`(set! ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(,_ ...)
       (lift-in-seq? v)]
      [`,_ #f]))

  (define (lift-in-let? v)
    (match v
      [`(,_ ([,_ ,rhss] ...) . ,body)
       (or (for/or ([rhs (in-list rhss)])
             (lift-in-expr? rhs))
           (lift-in-seq? body))]))
  
  (define (lift-in-seq? vs)
    (for/or ([v (in-wrap-list vs)])
      (lift-in-expr? v)))

  ;; Under a `lambda`; any local bindings to functions?
  (define (lift? v)
    (match v
      [`(let . ,_) (lift?/let v)]
      [`(letrec . ,_) (lift?/let v)]
      [`(letrec* . ,_) (lift?/let v)]
      [`(let-values . ,_) (lift?/let v)]
      [`(letrec-values . ,_) (lift?/let v)]
      [`(lambda ,_ . ,body) (lift?/seq body)]
      [`(case-lambda [,_ . ,bodys] ...)
       (for/or ([body (in-list bodys)])
         (lift?/seq body))]
      [`(begin . ,vs) (lift?/seq vs)]
      [`(begin0 . ,vs) (lift?/seq vs)]
      [`(quote . ,_) #f]
      [`(if ,tst ,thn ,els)
       (or (lift? tst) (lift? thn) (lift? els))]
      [`(with-continuation-mark ,key ,val ,body)
       (or (lift? key) (lift? val) (lift? body))]
      [`(set! ,_ ,rhs) (lift? rhs)]
      [`(#%variable-reference) #f]
      [`(#%variable-reference ,id) #f]
      [`(,rator . ,rands)
       (or (lift? rator) (lift?/seq rands))]
      [`,_ #f]))
  
  (define (lift?/let v)
    (match v
      [`(,_ ([,_ ,rhss] ...) . ,body)
       (or (for/or ([rhs (in-list rhss)])
             (lambda? rhs))
           (lift?/seq body))]))

  (define (lift?/seq vs)
    (for/or ([v (in-wrap-list vs)])
      (lift? v)))

  ;; ----------------------------------------
  
  ;; Look for a `lambda` to lift out of:
  (define (lift-in v)
    (match v
      [`(define ,id ,rhs)
       (reannotate v `(define ,id ,(lift-in-expr rhs)))]
      [`(define-values ,ids ,rhs)
       (reannotate v `(define-values ,ids ,(lift-in-expr rhs)))]
      [`(begin ,vs ...)
       (reannotate v `(begin ,@(for/list ([v (in-wrap-list vs)])
                                 (lift-in v))))]
      [`,_ (lift-in-expr v)]))

  ;; Look for a `lambda` to lift out of:
  (define (lift-in-expr v)
    (match v
      [`(lambda ,args . ,body)
       (define lifts (make-hasheq))
       (define locals (add-args args #hasheq()))
       (define frees+binds/ignored (compute-seq-lifts! body empty-frees+binds lifts locals))
       (let ([lifts (if (zero? (hash-count lifts))
                        lifts
                        (close-and-convert-lifts lifts))])
         (cond
           [(zero? (hash-count lifts)) v]
           [else
            `(letrec ,(extract-lifted-bindings lifts)
               ,(reannotate v `(lambda ,args . ,(convert-lifted-calls-in-seq/add-mutators body args lifts))))]))]
      [`(case-lambda [,argss . ,bodys] ...)
       ;; Lift each clause separately, then splice results:
       (let ([lams (for/list ([args (in-list argss)]
                              [body (in-list bodys)])
                     (lift-in-expr `(lambda ,args . ,body)))])
         (reannotate
          v
          (let loop ([lams lams] [clauses null] [bindings null])
            (cond
              [(null? lams)
               (if (null? bindings)
                   `(case-lambda ,@(reverse clauses))
                   `(letrec ,bindings ,(loop null clauses null)))]
              [else
               (match (car lams)
                 [`(letrec ,new-bindings ,lam)
                  (loop (cons lam (cdr lams)) clauses (append (unwrap-list new-bindings) bindings))]
                 [`(lambda ,args . ,body)
                  (loop (cdr lams) (cons `[,args . ,body] clauses) bindings)])]))))]
      [`(let . ,_) (lift-in-let v)]
      [`(letrec . ,_) (lift-in-let v)]
      [`(letrec* . ,_) (lift-in-let v)]
      [`(let-values . ,_) (lift-in-let v)]
      [`(letrec-values . ,_) (lift-in-let v)]
      [`(begin . ,vs)
       (reannotate v `(begin ,@(for/list ([v (in-wrap-list vs)])
                                 (lift-in-expr v))))]
      [`(if ,tst ,thn ,els)
       (reannotate v `(if ,(lift-in-expr tst)
                          ,(lift-in-expr thn)
                          ,(lift-in-expr els)))]
      [`(with-continuation-mark ,key ,val ,body)
       (reannotate v `(with-continuation-mark ,(lift-in-expr key)
                                              ,(lift-in-expr val)
                                              ,(lift-in-expr body)))]
      [`(quote ,_) v]
      [`(#%variable-reference ,_) v]
      [`(#%variable-reference) v]
      [`(set! ,id ,rhs)
       (reannotate v `(set! ,id ,(lift-in-expr rhs)))]
      [`(,_ ...)
       (lift-in-seq v)]
      [`,_ v]))

  (define (lift-in-let v)
    (match v
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (reannotate v `(,let-id
                       ,(for/list ([id (in-list ids)]
                                   [rhs (in-list rhss)])
                          `[,id ,(lift-in-expr rhs)])
                       . ,(lift-in-seq body)))]))
  
  (define (lift-in-seq vs)
    (reannotate vs (for/list ([v (in-wrap-list vs)])
                     (lift-in-expr v))))

  ;; ----------------------------------------
  ;; Pass 1: figure out which bindings can be lifted, and also record
  ;; information about mutated and `#%variable-reference` variables.
  ;; We only care about local variables within a top-level `lambda` or
  ;; `case-lambda` form.

  ;; Returns a set of free variables and a set of bound variables
  ;; (paired together) while potentially mutating `lifts`
  (define (compute-lifts! v frees+binds lifts locals)
    (match v
      [`(let ([,ids ,rhss] ...) . ,body)
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (when (lambda? rhs)
           ;; RHS is a candidate for lifting
           (hash-set! lifts (unwrap id) (liftable rhs #f #f))))
       (let* ([frees+binds (compute-rhs-lifts! ids rhss frees+binds lifts locals)]
              [frees+binds (compute-seq-lifts! body frees+binds lifts (add-args ids locals))])
         (remove-frees/add-binds ids frees+binds lifts))]
      [`(letrec . ,_)
       (compute-letrec-lifts! v frees+binds lifts locals)]
      [`(letrec* . ,_)
       (compute-letrec-lifts! v frees+binds lifts locals)]
      [`(let-values ([,idss ,rhss] ...) . ,body)
       (let* ([frees+binds (compute-seq-lifts! rhss frees+binds lifts locals)]
              [locals (for/fold ([locals locals]) ([ids (in-list idss)])
                        (add-args ids locals))]
              [frees+binds (compute-seq-lifts! body frees+binds lifts locals)])
         (for/fold ([frees+binds frees+binds]) ([ids (in-list idss)])
           (remove-frees/add-binds ids frees+binds lifts)))]
      [`(letrec-values ([,idss ,rhss] ...) . ,body)
       (let* ([locals (for/fold ([locals locals]) ([ids (in-list idss)])
                        (add-args ids locals))]
              [frees+binds (compute-seq-lifts! rhss frees+binds lifts locals)]
              [frees+binds (compute-seq-lifts! body frees+binds lifts locals)])
         (for/fold ([frees+binds frees+binds]) ([ids (in-list idss)])
           (remove-frees/add-binds ids frees+binds lifts)))]
      [`((letrec ([,id ,rhs]) ,rator) ,rands ...)
       (compute-lifts! `(letrec ([,id ,rhs]) (,rator . ,rands)) frees+binds lifts locals)]
      [`((letrec* ([,id ,rhs]) ,rator) ,rands ...)
       (compute-lifts! `(letrec ([,id ,rhs]) (,rator . ,rands)) frees+binds lifts locals)]
      [`(lambda ,args . ,body)
       (let ([frees+binds (compute-seq-lifts! body frees+binds lifts (add-args args locals))])
         (remove-frees/add-binds args frees+binds lifts))]
      [`(case-lambda [,argss . ,bodys] ...)
       (for/fold ([frees+binds frees+binds]) ([args (in-list argss)]
                                  [body (in-list bodys)])
         (let ([frees+binds (compute-seq-lifts! body frees+binds lifts (add-args args locals))])
           (remove-frees/add-binds args frees+binds lifts)))]
      [`(begin . ,vs)
       (compute-seq-lifts! vs frees+binds lifts locals)]
      [`(begin0 . ,vs)
       (compute-seq-lifts! vs frees+binds lifts locals)]
      [`(quote . ,_) frees+binds]
      [`(if ,tst ,thn ,els)
       (let* ([frees+binds (compute-lifts! tst frees+binds lifts locals)]
              [frees+binds (compute-lifts! thn frees+binds lifts locals)]
              [frees+binds (compute-lifts! els frees+binds lifts locals)])
         frees+binds)]
      [`(with-continuation-mark ,key ,val ,body)
       (let* ([frees+binds (compute-lifts! key frees+binds lifts locals)]
              [frees+binds (compute-lifts! val frees+binds lifts locals)]
              [frees+binds (compute-lifts! body frees+binds lifts locals)])
         frees+binds)]
      [`(set! ,id ,rhs)
       (define var (unwrap id))
       (let ([frees+binds (cond
                            [(hash-ref locals var #f)
                             (define ind (lookup-indirected-variable lifts var))
                             (add-free frees+binds (indirected-mutator ind))]
                            [else frees+binds])])
         (compute-lifts! rhs frees+binds lifts locals))]
      [`(#%variable-reference)
       frees+binds]
      [`(#%variable-reference ,id)
       (define var (unwrap id))
       (cond
         [(hash-ref locals var #f)
          (define ind (lookup-indirected-variable lifts var))
          (add-free frees+binds (indirected-variable-reference ind))]
         [else frees+binds])]
      [`(,rator . ,rands)
       (define f (unwrap rator))
       (let ([frees+binds
              (cond
                [(symbol? f)
                 (let ([proc (hash-ref lifts f #f)])
                   (when (liftable? proc)
                     (unless (consistent-argument-count? (liftable-expr proc) (length (unwrap-list rands)))
                       (hash-remove! lifts f))))
                 ;; Don't recur on `rator`, because we don't want
                 ;; to mark `f` as unliftable
                 (if (hash-ref locals f #f)
                     (add-free frees+binds f)
                     frees+binds)]
                [else
                 (compute-lifts! rator frees+binds lifts locals)])])
         (compute-seq-lifts! rands frees+binds lifts locals))]
      [`,_
       (define x (unwrap v))
       (cond
         [(or (string? x) (bytes? x) (boolean? x) (number? x))
          frees+binds]
         [else
          (unless (symbol? x)
            (error 'lift-in-schemified
                   "unrecognized expression form: ~e"
                   (strip-annotations v)))
          ;; If this identifier is mapped to a liftable, then
          ;; the function is not liftable after all, since
          ;; the reference isn't in an application position
          (let ([proc (hash-ref lifts x #f)])
            (when (liftable? proc)
              (hash-remove! lifts x)))
          (if (hash-ref locals x #f)
              (add-free frees+binds x)
              frees+binds)])]))

  ;; Like `compute-lifts!`, but for a sequence of expressions
  (define (compute-seq-lifts! vs frees+binds lifts locals)
    (for/fold ([frees+binds frees+binds]) ([v (in-wrap-list vs)])
      (compute-lifts! v frees+binds lifts locals)))

  ;; Similar to `compute-seq-lifts!`, but installs free-variable
  ;; information in the `lifts` table for each identifier in `ids`:
  (define (compute-rhs-lifts! ids rhss frees+binds lifts locals)
    (for/fold ([frees+binds frees+binds]) ([id (in-list ids)]
                                           [rhs (in-list rhss)])
      (let ([rhs-frees+binds (compute-lifts! rhs empty-frees+binds lifts locals)]
            [f (unwrap id)])
        (let ([proc (hash-ref lifts f #f)])
          (when (liftable? proc)
            (set-liftable-frees! proc (car rhs-frees+binds))
            (set-liftable-binds! proc (cdr rhs-frees+binds))))
        (cons (union (car rhs-frees+binds) (car frees+binds))
              (union (cdr rhs-frees+binds) (cdr frees+binds))))))

  ;; Handle a letrec[*] form
  (define (compute-letrec-lifts! v frees+binds lifts locals)
    (match v
      [`(,_ ([,ids ,rhss] ...) . ,body)
       (let ([locals (add-args ids locals)])
         (when (for/and ([rhs (in-list rhss)])
                 (lambda? rhs))
           ;; Each RHS is a candidate for lifting
           (for ([id (in-list ids)]
                 [rhs (in-list rhss)])
             (hash-set! lifts (unwrap id) (liftable rhs #f #f))))
         (let* ([frees+binds (compute-rhs-lifts! ids rhss frees+binds lifts locals)]
                [frees+binds (compute-seq-lifts! body frees+binds lifts locals)])
           (remove-frees/add-binds ids frees+binds lifts)))]))

  ;; ----------------------------------------
  ;; Bridge between pass 1 and 2: transitive closure of free variables

  ;; Close a liftable's free variables over other variables needed by
  ;; other lifted functions that it calls. Also, clear `mutated` and
  ;; `var-ref` information from `lifts` in the returned table.
  (define (close-and-convert-lifts lifts)
    (define new-lifts (make-hash))
    ;; Copy over `liftable`s:
    (for ([(f info) (in-hash lifts)])
      (when (liftable? info)
        (hash-set! new-lifts f info)))
    ;; Compute the closure of free-variable sets, where a function
    ;; to be lifted calls another function to be lifted, and also
    ;; re-register mutators and variable references that are
    ;; used.
    (for ([proc (in-list (hash-values new-lifts))])
      (define frees (liftable-frees proc))
      (define binds (liftable-binds proc))
      (define closed-frees
        (let loop ([frees frees] [todo (hash-keys frees)])
          (cond
            [(null? todo) frees]
            [else
             (define v (car todo))
             (define info (hash-ref lifts v #f))
             (cond
               [(liftable? info)
                ;; A liftable function called by ths liftable function,
                ;; so we'll need to be able to supply all of its free
                ;; variables
                (define v-binds (liftable-binds info))
                (let v-loop ([v-frees (hash-keys (liftable-frees info))]
                             [frees frees]
                             [todo (cdr todo)])
                  (if (null? v-frees)
                      (loop frees todo)
                      (let ([g (car v-frees)])
                        (cond
                          [(or (hash-ref frees g #f)  ; avoid cycles
                               (hash-ref binds g #f) ; don't add if bound in this function
                               (hash-ref v-binds g #f)) ; don't add if local to `v`
                           (v-loop (cdr v-frees) frees todo)]
                          [else
                           (v-loop (cdr v-frees)
                                   (hash-set frees g #t)
                                   (cons g todo))]))))]
               [(mutator? info)
                ;; Re-register mutator
                (define ind (lookup-reregister-indirected-variable (mutator-orig info) new-lifts))
                (set-indirected-mutator! ind v)
                (loop frees (cdr todo))]
               [(var-ref? info)
                ;; Re-register variable reference
                (define ind (lookup-reregister-indirected-variable (var-ref-orig info) new-lifts))
                (set-indirected-variable-reference! ind v)
                (loop frees (cdr todo))]
               [(indirected? info)
                ;; Switch to accessor and register it
                (define ind (lookup-reregister-indirected-variable v new-lifts))
                (define new-v (indirected-accessor info))
                (set-indirected-accessor! ind new-v)
                (loop (hash-set (hash-remove frees v) new-v #t)
                      (cdr todo))]
               [else
                ;; Normal (non-mutated, non-lifted) variable:
                (loop frees (cdr todo))])])))
      (set-liftable-frees! proc closed-frees))
    ;; Remove references to lifted from free-variable sets, and also
    ;; convert free-variable sets to lists for consistent ordering:
    (for ([proc (in-hash-values new-lifts)]
          #:when (liftable? proc))
      (set-liftable-frees! proc (sort (for/list ([f (in-hash-keys (liftable-frees proc))]
                                                 #:unless (liftable? (hash-ref lifts f #f)))
                                        f)
                                      symbol<?)))
    ;; Return new lifts
    new-lifts)

  ;; ----------------------------------------
  ;; Pass 2: convert calls based on previously collected information
    
  (define (convert-lifted-calls-in-expr v lifts)
    (let convert ([v v])
      (match v
        [`(let . ,_)
         (convert-lifted-calls-in-let v lifts)]
        [`(letrec . ,_)
         (convert-lifted-calls-in-let v lifts)]
        [`(letrec* . ,_)
         (convert-lifted-calls-in-let v lifts)]
        [`(let-values . ,_)
         (convert-lifted-calls-in-let v lifts)]
        [`(letrec-values . ,_)
         (convert-lifted-calls-in-let v lifts)]
        [`((letrec ([,id ,rhs]) ,rator) ,rands ...)
         (convert (reannotate v `(letrec ([,id ,rhs]) (,rator . ,rands))))]
        [`((letrec* ([,id ,rhs]) ,rator) ,rands ...)
         (convert (reannotate v `(letrec* ([,id ,rhs]) (,rator . ,rands))))]
        [`(lambda ,args . ,body)
         (reannotate v `(lambda ,args . ,(convert-lifted-calls-in-seq/add-mutators body args lifts)))]
        [`(case-lambda [,argss . ,bodys] ...)
         (reannotate v `(case-lambda
                          ,@(for/list ([args (in-list argss)]
                                       [body (in-list bodys)])
                              `[,args . ,(convert-lifted-calls-in-seq/add-mutators body args lifts)])))]
        [`(begin . ,vs)
         (reannotate v `(begin . ,(convert-lifted-calls-in-seq vs lifts)))]
        [`(begin0 . ,vs)
         (reannotate v `(begin0 . ,(convert-lifted-calls-in-seq vs lifts)))]
        [`(quote . ,_) v]
        [`(if ,tst ,thn ,els)
         (reannotate v `(if ,(convert tst) ,(convert thn) ,(convert els)))]
        [`(with-continuation-mark ,key ,val ,body)
         (reannotate v `(with-continuation-mark ,(convert key) ,(convert val) ,(convert body)))]
        [`(set! ,id ,rhs)
         (define info (hash-ref lifts (unwrap id) #f))
         (cond
           [(and (indirected? info)
                 (indirected-mutator info))
            => (lambda (mutator-var)
                 (reannotate v `(,mutator-var ,(convert rhs))))]
           [else
            (reannotate v `(set! ,id ,(convert rhs)))])]
        [`(#%variable-reference) v]
        [`(#%variable-reference ,id)
         (define info (hash-ref lifts (unwrap id) #f))
         (cond
           [(and (indirected? info)
                 (indirected-variable-reference info))
            => (lambda (var-ref-var)
                 var-ref-var)]
           [else v])]
        [`(,rator . ,rands)
         (let ([rands (convert-lifted-calls-in-seq rands lifts)])
           (define f (unwrap rator))
           (cond
             [(and (symbol? f) (hash-ref lifts f #f))
              => (lambda (proc)
                   (reannotate v `(,rator ,@(liftable-frees proc) . ,rands)))]
             [else
              (reannotate v `(,(convert rator) . ,rands))]))]
        [`,_
         (define var (unwrap v))
         (define info (and (symbol? var)
                           (hash-ref lifts var #f)))
         (cond
           [(and (indirected? info)
                 (indirected-accessor info))
            => (lambda (accessor-var)
                 (reannotate v `(,accessor-var)))]
           [else v])])))
  
  (define (convert-lifted-calls-in-seq vs lifts)
    (reannotate vs (for/list ([v (in-wrap-list vs)])
                     (convert-lifted-calls-in-expr v lifts))))

  (define (convert-lifted-calls-in-let v lifts)
    (match v
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (reannotate
        v
        `(,let-id ,(for/list ([id (in-list ids)]
                              [rhs (in-list rhss)]
                              #:unless (liftable? (hash-ref lifts id #f)))
                     `[,id ,(convert-lifted-calls-in-expr rhs lifts)])
                  . ,(convert-lifted-calls-in-seq/add-mutators body ids lifts)))]))

  ;; For any `id` in `ids` that needs a mutator or variable-reference
  ;; binding to pass to a lifted function (as indicated by an
  ;; `indirected` mapping in `lifts`), add the binding. The `ids` can
  ;; be any tree of annotated symbols.
  (define (convert-lifted-calls-in-seq/add-mutators vs ids lifts)
    (let loop ([ids ids])
      (cond
        [(null? ids) (convert-lifted-calls-in-seq vs lifts)]
        [(wrap-pair? ids)
         (let ([a (wrap-car ids)])
           (cond
             [(wrap-pair? a)
              (loop (cons (wrap-car a) (cons (wrap-cdr a) (wrap-cdr ids))))]
             [(wrap-null? a)
              (loop (wrap-cdr ids))]
             [else
              (define v (unwrap a))
              (define ind (hash-ref lifts v #f))
              (cond
                [(indirected? ind)
                 ;; Add a binding for a mutator and/or variable reference
                 `((let ,(append
                          (if (indirected-accessor ind)
                              `([,(indirected-accessor ind)
                                 (lambda () ,v)])
                              '())
                          (if (indirected-mutator ind)
                              `([,(indirected-mutator ind)
                                 ,(let ([val-var (gensym 'v)])
                                    `(lambda (,val-var) (set! ,v ,val-var)))])
                              '())
                          (if (indirected-variable-reference ind)
                              `([,(indirected-variable-reference ind)
                                 (#%variable-reference ,v)])
                              '()))
                     . ,(loop (wrap-cdr ids))))]
                [else (loop (wrap-cdr ids))])]))]
        [else
         ;; `rest` arg:
         (loop (list ids))])))

  ;; Create bindings for lifted functions, adding new arguments
  ;; as the functions are lifted
  (define (extract-lifted-bindings lifts)
    (for/list ([(f proc) (in-hash lifts)]
               #:when (liftable? proc))
      (let ([new-args (liftable-frees proc)]
            [rhs (liftable-expr proc)])
        `[,f ,(match rhs
                [`(lambda ,args . ,body)
                 (let ([body (convert-lifted-calls-in-seq body lifts)])
                   (reannotate rhs `(lambda ,(append new-args args) . ,body)))]
                [`(case-lambda [,argss . ,bodys] ...)
                 (reannotate rhs `(case-lambda
                                    ,@(for/list ([args (in-list argss)]
                                                 [body (in-list bodys)])
                                        (let ([body (convert-lifted-calls-in-seq body lifts)])
                                          `[,(append new-args args) . ,body]))))])])))

  ;; ----------------------------------------
  ;; Helpers

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (consistent-argument-count? proc n)
    (define (consistent? args n)
      (let loop ([args args] [n n])
        (cond
          [(negative? n) #f]
          [(wrap-null? args) (zero? n)]
          [(wrap-pair? args)
           (loop (wrap-cdr args) (sub1 n))]
          [else #t])))
    (match proc
      [`(lambda ,args . ,_)
       (consistent? args n)]
      [`(case-lambda [,argss . ,_] ...)
       (for/or ([args (in-list argss)])
         (consistent? args n))]
      [`,_ #f]))

  ;; Find or create an `indirected` record for a variable
  (define (lookup-indirected-variable lifts var)
    (define ind (hash-ref lifts var #f))
    (or (and (indirected? ind)
             ind)
        (let ([ind (indirected (gensym var) ; accessor
                               (gensym var) ; mutator
                               (gensym var))]) ; variable reference
          (hash-set! lifts var ind)
          (hash-set! lifts (indirected-mutator ind) (mutator var))
          (hash-set! lifts (indirected-variable-reference ind) (var-ref var))
          ind)))

  ;; Find or create an `indirected` record for a variable in the
  ;; post-transitive-closure table of lift information
  (define (lookup-reregister-indirected-variable var lifts)
    (or (hash-ref lifts var #f)
        (let ([ind (indirected #f #f #f)])
          (hash-set! lifts var ind)
          ind)))

  ;; Add a group of arguments (a list or improper list) to a set
  (define (add-args args s)
    (let loop ([args args] [s s])
      (cond
        [(wrap-null? args) s]
        [(wrap-pair? args)
         (loop (wrap-cdr args)
               (hash-set s (unwrap (wrap-car args)) #t))]
        [else (hash-set s (unwrap args) #t)])))

  ;; Add a free variable
  (define (add-free frees+binds var)
    (cons (hash-set (car frees+binds) var #t)
          (cdr frees+binds)))

  ;; Remove a group of arguments (a list or improper list) from a set
  ;; as the variable go out of scope, including any associated mutator
  ;; and variable-reference variables, but keep variables for lifted
  ;; functions
  (define (remove-frees/add-binds args frees+binds lifts)
    (define (remove-free/add-bind frees+binds arg)
      (define info (hash-ref lifts arg #f))
      (cond
        [(liftable? info)
         ;; Since `arg` will be lifted to the top, it
         ;; stays in our local set of free variables,
         ;; but also add it to binds so that callers
         ;; will know that they don't need to chain
         (cons (car frees+binds)
               (hash-set (cdr frees+binds) arg #t))]
        [(indirected? info)
         ;; In addition to `arg`, move associated mutator and variable-reference
         (let* ([frees (car frees+binds)]
                [frees (hash-remove frees arg)]
                [frees (hash-remove frees (indirected-mutator info))]
                [frees (hash-remove frees (indirected-variable-reference info))]
                [binds (cdr frees+binds)]
                [binds (hash-set binds arg #t)]
                [binds (hash-set binds (indirected-accessor info) #t)]
                [binds (hash-set binds (indirected-mutator info) #t)]
                [binds (hash-set binds (indirected-variable-reference info) #t)])
           (cons frees binds))]
        [else (cons (hash-remove (car frees+binds) arg)
                    (hash-set (cdr frees+binds) arg #t))]))
    (let loop ([args args] [frees+binds frees+binds])
      (cond
        [(wrap-null? args) frees+binds]
        [(wrap-pair? args)
         (loop (wrap-cdr args)
               (remove-free/add-bind frees+binds (unwrap (wrap-car args))))]
        [else (remove-free/add-bind frees+binds (unwrap args))])))

  ;; Set union
  (define (union s1 s2)
    (cond
      [((hash-count s1) . > . (hash-count s2))
       (union s2 s1)]
      [else
       (for/fold ([s2 s2]) ([k (in-hash-keys s1)])
         (hash-set s2 k #t))]))

  ;; ----------------------------------------
  ;; Go
  
  (if (lift-in? v)
      (lift-in v)
      v))
