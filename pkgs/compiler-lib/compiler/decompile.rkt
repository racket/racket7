#lang racket/base
(require compiler/zo-parse
         syntax/modcollapse
         racket/port
         racket/match
         racket/list
         racket/set
         racket/path
         (only-in '#%linklet compiled-position->primitive))

(provide decompile)

;; ----------------------------------------

(define primitive-table
  (for/hash ([i (in-naturals)]
             #:break (not (compiled-position->primitive i)))
    (define v (compiled-position->primitive i))
    (values i (or (object-name v) v))))

(define (list-ref/protect l pos who)
  (list-ref l pos)
  #;
  (if (pos . < . (length l))
      (list-ref l pos)
      `(OUT-OF-BOUNDS ,who ,pos ,(length l) ,l)))

;; ----------------------------------------

(define-struct glob-desc (vars))

;; Main entry:
(define (decompile top)
  (cond
   [(hash? top)
    (for/hash ([(k v) (in-hash top)])
      (values k (decompile v)))]
   [(linkl? top)
    (decompile-linklet top)]
   [else `(quote ,top)]))

(define (decompile-linklet l)
  (match l
    [(struct linkl (name importss import-shapess exports internals lifts source-names body max-let-depth))
     (define closed (make-hasheq))
     (define globs (glob-desc
                    (append
                     (list 'root)
                     (apply append importss)
                     exports
                     internals
                     lifts)))
     `(linklet
       ,importss
       ,exports
       ,@(for/list ([form (in-list body)])
           (decompile-form form globs '(#%globals) closed)))]))

(define (decompile-form form globs stack closed)
  (match form
    [(struct def-values (ids rhs))
     `(define-values ,(map (lambda (tl)
                             (match tl
                               [(struct toplevel (depth pos const? set-const?))
                                (list-ref/protect (glob-desc-vars globs) pos 'def-vals)]))
                           ids)
        ,(if (inline-variant? rhs)
             `(begin
                ,(list 'quote '%%inline-variant%%)
                ,(decompile-expr (inline-variant-inline rhs) globs stack closed)
                ,(decompile-expr (inline-variant-direct rhs) globs stack closed))
             (decompile-expr rhs globs stack closed)))]
    [(struct seq (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack closed))
                    forms))]
    [else
     (decompile-expr form globs stack closed)]))

(define (extract-name name)
  (if (symbol? name)
      (gensym name)
      (if (vector? name)
          (gensym (vector-ref name 0))
          #f)))

(define (extract-id expr)
  (match expr
    [(struct lam (name flags num-params arg-types rest? closure-map closure-types tl-map max-let-depth body))
     (extract-name name)]
    [(struct case-lam (name lams))
     (extract-name name)]
    [(struct closure (lam gen-id))
     (extract-id lam)]
    [else #f]))

(define (extract-ids! body ids)
  (match body
    [(struct let-rec (procs body))
     (for ([proc (in-list procs)]
           [delta (in-naturals)])
       (when (< -1 delta (vector-length ids))
         (vector-set! ids delta (extract-id proc))))
     (extract-ids! body ids)]
    [(struct install-value (val-count pos boxes? rhs body))
     (extract-ids! body ids)]
    [(struct boxenv (pos body))
     (extract-ids! body ids)]
    [else #f]))

(define (decompile-tl expr globs stack closed no-check?)
  (match expr
    [(struct toplevel (depth pos const? ready?))
     (let ([id (list-ref/protect (glob-desc-vars globs) pos 'toplevel)])
       (cond
        [no-check? id]
        [(and (not const?) (not ready?))
         `(#%checked ,id)]
        #;[(and const? ready?) `(#%const ,id)]
        #;[const? `(#%iconst ,id)]
        [else id]))]))

(define (decompile-expr expr globs stack closed)
  (match expr
    [(struct toplevel (depth pos const? ready?))
     (decompile-tl expr globs stack closed #f)]
    [(struct varref (tl dummy))
     `(#%variable-reference ,(if (eq? tl #t)
                                 '<constant-local>
                                 (decompile-tl tl globs stack closed #t)))]
    [(struct primval (id))
     (hash-ref primitive-table id (lambda () (error "unknown primitive: " id)))]
    [(struct assign (id rhs undef-ok?))
     `(set! ,(decompile-expr id globs stack closed)
            ,(decompile-expr rhs globs stack closed))]
    [(struct localref (unbox? offset clear? other-clears? type))
     (let ([id (list-ref/protect stack offset 'localref)])
       (let ([e (if unbox?
                    `(#%unbox ,id)
                    id)])
         (if clear?
             `(#%sfs-clear ,e)
             e)))]
    [(? lam?)
     `(lambda . ,(decompile-lam expr globs stack closed))]
    [(struct case-lam (name lams))
     `(case-lambda
       ,@(map (lambda (lam)
                (decompile-lam lam globs stack closed))
              lams))]
    [(struct let-one (rhs body type unused?))
     (let ([id (or (extract-id rhs)
                   (gensym (or type (if unused? 'unused 'local))))])
       `(let ([,id ,(decompile-expr rhs globs (cons id stack) closed)])
          ,(decompile-expr body globs (cons id stack) closed)))]
    [(struct let-void (count boxes? body))
     (let ([ids (make-vector count #f)])
       (extract-ids! body ids)
       (let ([vars (for/list ([i (in-range count)]
                              [id (in-vector ids)])
                     (or id (gensym (if boxes? 'localvb 'localv))))])
         `(let ,(map (lambda (i) `[,i ,(if boxes? `(#%box ?) '?)])
                     vars)
            ,(decompile-expr body globs (append vars stack) closed))))]
    [(struct let-rec (procs body))
     `(begin
        (#%set!-rec-values ,(for/list ([p (in-list procs)]
                                       [i (in-naturals)])
                              (list-ref/protect stack i 'let-rec))
                           ,@(map (lambda (proc)
                                    (decompile-expr proc globs stack closed))
                                  procs))
        ,(decompile-expr body globs stack closed))]
    [(struct install-value (count pos boxes? rhs body))
     `(begin
        (,(if boxes? '#%set-boxes! 'set!-values)
         ,(for/list ([i (in-range count)])
            (list-ref/protect stack (+ i pos) 'install-value))
         ,(decompile-expr rhs globs stack closed))
        ,(decompile-expr body globs stack closed))]
    [(struct boxenv (pos body))
     (let ([id (list-ref/protect stack pos 'boxenv)])
       `(begin
          (set! ,id (#%box ,id))
          ,(decompile-expr body globs stack closed)))]
    [(struct branch (test then else))
     `(if ,(decompile-expr test globs stack closed)
          ,(decompile-expr then globs stack closed)
          ,(decompile-expr else globs stack closed))]
    [(struct application (rator rands))
     (let ([stack (append (for/list ([i (in-list rands)]) (gensym 'rand))
                          stack)])
       (annotate-unboxed
        rands
        (annotate-inline
         `(,(decompile-expr rator globs stack closed)
           ,@(map (lambda (rand)
                    (decompile-expr rand globs stack closed))
                  rands)))))]
    [(struct apply-values (proc args-expr))
     `(#%apply-values ,(decompile-expr proc globs stack closed) 
                      ,(decompile-expr args-expr globs stack closed))]
    [(struct with-immed-mark (key-expr val-expr body-expr))
     (let ([id (gensym 'cmval)])
       `(#%call-with-immediate-continuation-mark
         ,(decompile-expr key-expr globs stack closed)
         (lambda (,id) ,(decompile-expr body-expr globs (cons id stack) closed))
         ,(decompile-expr val-expr globs stack closed)))]
    [(struct seq (exprs))
     `(begin ,@(for/list ([expr (in-list exprs)])
                 (decompile-expr expr globs stack closed)))]
    [(struct beg0 (exprs))
     `(begin0
       ,@(for/list ([expr (in-list exprs)])
           (decompile-expr expr globs stack closed))
       ;; Make sure a single expression doesn't look like tail position:
       ,@(if (null? (cdr exprs)) (list #f) null))]
    [(struct with-cont-mark (key val body))
     `(with-continuation-mark
          ,(decompile-expr key globs stack closed)
          ,(decompile-expr val globs stack closed)
          ,(decompile-expr body globs stack closed))]
    [(struct closure (lam gen-id))
     (if (hash-ref closed gen-id #f)
         gen-id
         (begin
           (hash-set! closed gen-id #t)
           `(#%closed ,gen-id ,(decompile-expr lam globs stack closed))))]
    [else `(quote ,expr)]))

(define (decompile-lam expr globs stack closed)
  (match expr
    [(struct closure (lam gen-id)) (decompile-lam lam globs stack closed)]
    [(struct lam (name flags num-params arg-types rest? closure-map closure-types tl-map max-let-depth body))
     (let ([vars (for/list ([i (in-range num-params)]
                            [type (in-list arg-types)])
                   (gensym (format "~a~a-" 
                                   (case type 
                                     [(ref) "argbox"] 
                                     [(val) "arg"]
                                     [else (format "arg~a" type)])
                                   i)))]
           [rest-vars (if rest? (list (gensym 'rest)) null)]
           [captures (map (lambda (v)
                            (list-ref/protect stack v 'lam))
                          (vector->list closure-map))])
       `((,@vars . ,(if rest?
                        (car rest-vars)
                        null))
         ,@(if (and name (not (null? name)))
               `(',name)
               null)
         ,@(if (null? flags) null `('(flags: ,@flags)))
         ,@(if (null? captures)
               null
               `('(captures: ,@(map (lambda (c t)
                                      (if t
                                          `(,t ,c)
                                          c))
                                    captures
                                    closure-types)
                             ,@(if (not tl-map)
                                   '()
                                   (list
                                    (for/list ([pos (in-list (sort (set->list tl-map) <))])
                                      (list-ref/protect (glob-desc-vars globs)
                                                        pos
                                                        'lam)))))))
         ,(decompile-expr body globs
                          (append captures
                                  (append vars rest-vars))
                          closed)))]))

(define (annotate-inline a)
  a)

(define (annotate-unboxed args a)
  a)

;; ----------------------------------------

#;
(begin
  (require scheme/pretty)
  (define (try e)
    (pretty-print
     (decompile
      (zo-parse (let-values ([(in out) (make-pipe)])
                  (write (parameterize ([current-namespace (make-base-namespace)])
                           (compile e))
                         out)
                  (close-output-port out)
                  in)))))
  (pretty-print
   (decompile
    (zo-parse (open-input-file "/home/mflatt/proj/plt/collects/tests/mzscheme/benchmarks/common/sboyer_ss.zo"))))
  #;
  (try '(lambda (q . more)
          (letrec ([f (lambda (x) f)])
            (lambda (g) f)))))
