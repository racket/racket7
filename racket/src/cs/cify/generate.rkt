#lang racket/base
(require racket/list
         "match.rkt"
         "sort.rkt"
         "id.rkt"
         "vehicle.rkt"
         "primitive.rkt"
         "free-var.rkt"
         "function.rkt"
         "struct.rkt"
         "arg.rkt"
         "union-find.rkt"
         "indent.rkt"
         "state.rkt"
         "env.rkt"
         "simple.rkt"
         "inline.rkt"
         "return.rkt")

(provide generate-header
         generate-struct
         generate-prototypes
         generate-vehicles
         generate-tops)

(define (generate-header)
  (printf "#include \"expander-glue.inc\"\n\n"))

(define (generate-struct name names)
  (printf "struct ~a_t {\n" name)
  (for ([name (in-sorted-hash-keys names symbol<?)])
    (printf "  Scheme_Object *~a;\n" (cify name)))
  (printf "};\n")
  (printf "static struct ~a_t ~a;\n" name name))

(define (generate-prototypes vehicles)
  (printf "\n")
  (for ([vehicle (in-list vehicles)])
    (generate-prototype vehicle)
    (printf ";\n")))

(define (generate-prototype vehicle)
  (printf "static Scheme_Object *~a(int __argc, Scheme_Object **__~aargv~a)"
          (cify (vehicle-id vehicle))
          (if (vehicle-use-argv-space? vehicle) "in_" "")
          (if (vehicle-closure? vehicle) ", Scheme_Object *__self" "")))
  
(define (generate-vehicles vehicles lambdas knowns top-names state)
  (for ([vehicle (in-list vehicles)])
    (generate-vehicle vehicle knowns top-names state lambdas)))

(define (generate-vehicle vehicle knowns top-names state lambdas)
  (printf "\n") (generate-prototype vehicle) (printf " {\n")
  (define lams (vehicle-lams vehicle))
  (define multi? (pair? (cdr lams)))
  (when (vehicle-use-argv-space? vehicle)
    (printf "  GC_CAN_IGNORE Scheme_Object *__argv_space[~a];\n" (vehicle-max-jump-argc vehicle))
    (printf "  Scheme_Object **__argv = __in_argv;\n"))
  (when (vehicle-overflow-check? vehicle)
    (printf "  if (check_overflow()) return handle_overflow(~a, __argc, __~aargv);\n"
            (if (vehicle-closure? vehicle) "__self" (format "top.~a" (cify (vehicle-id vehicle))))
            (if (vehicle-use-argv-space? vehicle) "in_" "")))
  (when multi?
    (printf "  switch(SCHEME_INT_VAL(SCHEME_PRIM_CLOSURE_ELS(__self)[0])) {\n")
    (for ([lam (in-list lams)]
          [i (in-naturals)])
      (printf "  case ~a: goto __entry_~a;\n" i (cify (lam-id lam))))
    (printf "  }\n"))
  (for ([lam (in-list lams)])
    (when (or multi? (lam-need-entry? lam))
      (printf "__entry_~a:\n" (cify (lam-id lam))))
    (generate-lambda lam multi? (or multi? (vehicle-overflow-check? vehicle)) knowns top-names state lambdas))
  (printf "}\n"))

(define (generate-lambda lam multi? bracket? knowns top-names state lambdas)
  (define e (lam-e lam))
  (define id (lam-id lam))
  (define free-vars (lam-free-vars lam))
  (define closure-offset (if multi? 1 0))
  (define base-indent (tab ""))
  (define indent (if bracket? (tab base-indent) base-indent))
  (when bracket? (printf "~a{\n" base-indent))
  (for ([free-var (in-list free-vars)]
        [i (in-naturals)])
    (printf "~aScheme_Object *~a = SCHEME_PRIM_CLOSURE_ELS(__self)[~a];\n" indent (cify free-var) (+ closure-offset i)))
  (match e
    [`(lambda . ,_) (generate-lambda-case indent lam e knowns top-names state lambdas)]
    [`(case-lambda [,idss . ,bodys] ...)
     (for ([ids (in-list idss)]
           [body (in-list bodys)]
           [i (in-naturals)])
       (printf "~a~aif (__argc ~a ~a) {\n" indent (if (zero? i) "" "else ") (if (list? ids) "==" ">=") (args-length ids))
       (generate-lambda-case (tab indent) lam `(lambda ,ids . ,body) knowns top-names state lambdas)
       (printf "~a}\n" indent))
     (printf "~aelse return NULL;\n" indent)])
  (when bracket? (printf "~a}\n" base-indent)))

(define (generate-lambda-case indent lam e knowns top-names state lambdas)
  (define name (lam-id lam))
  (match e
    [`(lambda ,ids . ,body)
     (define n (args-length ids))
     (let loop ([ids ids] [i 0])
       (unless (null? ids)
         (cond
           [(symbol? ids)
            (when (referenced? (hash-ref state ids #f))
              (printf "~aScheme_Object *~a; ~a = __make_args_list(__argc, __argv, ~a);\n"
                      indent
                      (cify ids)
                      (cify ids)
                      i))]
           [else
            (when (referenced? (hash-ref state (car ids) #f))
              (printf "~aScheme_Object *~a = __argv[~a];\n" indent (cify (car ids)) i))
            (loop (cdr ids) (add1 i))])))
     (when (hash-ref (lam-loop-targets lam) n #f)
       (printf "__recur_~a_~a:\n" (cify name) n))
     (box-mutable-ids indent ids state top-names)
     (generate (tail-return name lam ids) `(begin . ,body) lam indent (add-args (lam-env lam) ids)
               knowns top-names state lambdas)]))

(define (box-mutable-ids indent ids state top-names)
  (let loop ([ids ids])
    (unless (null? ids)
      (cond
        [(symbol? ids) (loop (list ids))]
        [else
         (when (and (mutated? (hash-ref state (car ids) #f))
                    (not (hash-ref top-names (car ids) #f)))
           (define c-name (let ([c-name (cify (car ids))])
                            (if (hash-ref top-names (car ids) #f)
                                (format "top.~a" c-name)
                                c-name)))
           (printf "~a~a = scheme_box_variable(~a);\n" indent c-name c-name))
         (loop (cdr ids))]))))

;; ----------------------------------------

(define (generate ret e in-lam indent env knowns top-names state lambdas)
  (match e
    [`(quote ,v)
     (generate-quote indent ret v)]
    [`(lambda . ,_)
     (generate-closure ret e in-lam indent env knowns top-names state lambdas)]
    [`(case-lambda . ,_)
     (generate-closure ret e in-lam indent env knowns top-names state lambdas)]
    [`(begin ,e)
     (generate ret e in-lam indent env knowns top-names state lambdas)]
    [`(begin ,e . ,r)
     (generate (multiple-return "") e in-lam indent env knowns top-names state lambdas)
     (generate ret `(begin . ,r) in-lam indent env knowns top-names state lambdas)]
    [`(begin0 ,e . ,r)
     (define vals-id (genid '__vals))
     (printf "~a{\n" indent)
     (printf "~a  Scheme_Object *~a, **~a_values;\n" indent vals-id vals-id)
     (printf "~a  int ~a_count;\n" indent vals-id)
     (define (save-values indent)
       (printf "~aif (~a == SCHEME_MULTIPLE_VALUES) {\n" indent vals-id)
       (printf "~a  Scheme_Thread *p = scheme_current_thread;\n" indent)
       (printf "~a  ~a_values = p->ku.multiple.array;\n" indent vals-id)
       (printf "~a  ~a_count = p->ku.multiple.count;\n" indent vals-id)
       (printf "~a  if (SAME_OBJ(~a_values, p->values_buffer))\n" indent vals-id)
       (printf "~a    p->values_buffer = NULL;\n" indent)
       (printf "~a} else\n" indent)
       (printf "~a  ~a_count = 1;\n" indent vals-id))
     (generate (multiple-return/suffix (format "~a =" vals-id) save-values) e in-lam (tab indent) env knowns top-names state lambdas)
     (generate (multiple-return "") `(begin . ,r) in-lam (tab indent) env knowns top-names state lambdas)
     (printf "~a  if (~a_count != 1)\n" indent vals-id)
     (return (tab (tab indent)) ret #:can-omit? #t
             (format "scheme_values(~a_count, ~a_values)" vals-id vals-id))
     (printf "~a  else\n" indent)
     (return (tab (tab indent)) ret #:can-omit? #t
             vals-id)
     (printf "~a}\n" indent)]
    [`(if ,orig-tst ,thn ,els)
     (define-values (tsts wrapper) (extract-inline-predicate orig-tst knowns #:compose? #t))
     (define tst-ids (for/list ([tst (in-list tsts)])
                       (if (simple? tst state knowns)
                           #f
                           (genid '__if))))
     (define all-simple? (for/and ([tst-id (in-list tst-ids)])
                           (not tst-id)))
     (define sub-indent (if all-simple? indent (tab indent)))
     (unless all-simple?
       (printf "~a{\n" indent)
       (for ([tst-id (in-list tst-ids)]
             #:when tst-id)
         (printf "~a  GC_CAN_IGNORE Scheme_Object *~a;\n" indent tst-id))
       (for ([tst-id (in-list tst-ids)]
             [tst (in-list tsts)]
             #:when tst-id)
         (generate (format "~a =" tst-id) tst in-lam (tab indent) env knowns top-names state lambdas)))
     (printf "~aif (~a) {\n"
             sub-indent
             (wrapper (apply string-append
                             (add-between
                              (for/list ([tst-id (in-list tst-ids)]
                                         [tst (in-list tsts)])
                                (format "~a"
                                        (or tst-id
                                            (generate-simple tst env top-names knowns))))
                              ", "))))
     (generate ret thn in-lam (tab sub-indent) env knowns top-names state lambdas)
     (printf "~a} else {\n" sub-indent)
     (generate ret els in-lam (tab sub-indent) env knowns top-names state lambdas)
     (printf "~a}\n" sub-indent)
     (unless all-simple?
       (printf "~a}\n" indent))]
    [`(with-continuation-mark ,key ,val ,body)
     (define wcm-id (genid '__wcm))
     (printf "~a{\n" indent)
     (printf "~a  Scheme_Object *~a_key, *~a_val;\n" indent wcm-id wcm-id)
     (unless (tail-return? ret)
       (printf "~a  Scheme_Cont_Frame_Data ~a_frame;\n" indent wcm-id)
       (printf "~a  scheme_push_continuation_frame(&~a_frame);\n" indent wcm-id))
     (generate (format "~a_key =" wcm-id) key in-lam (tab indent) env knowns top-names state lambdas)
     (generate (format "~a_val =" wcm-id) val in-lam (tab indent) env knowns top-names state lambdas)
     (printf "~a  scheme_set_cont_mark(~a_key, ~a_val);\n" indent wcm-id wcm-id)
     (generate ret body in-lam (tab indent) env knowns top-names state lambdas)
     (unless (tail-return? ret)
       (printf "~a  scheme_pop_continuation_frame(&~a_frame);\n" indent wcm-id))
     (printf "~a}\n" indent)]
    [`(let . ,_) (generate-let ret e in-lam indent env knowns top-names state lambdas)]
    [`(letrec . ,_) (generate-let ret e in-lam indent env knowns top-names state lambdas)]
    [`(letrec* . ,_) (generate-let ret e in-lam indent env knowns top-names state lambdas)]
    [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
     (define values-ret (if (for/or ([id (in-list ids)])
                              (or (referenced? (hash-ref state id #f))
                                  (hash-ref top-names id #f)))
                            "/*needed*/"
                            ""))
     (generate (multiple-return values-ret) `(begin . ,body1) in-lam indent env knowns top-names state lambdas)
     (printf "~a{\n" indent)
     (generate-multiple-value-binds (format "~aScheme_Object *" (tab indent)) ids state top-names)
     (generate ret `(begin . ,body2) in-lam (tab indent) (add-args env ids) knowns top-names state lambdas)
     (printf "~a}\n" indent)]
    [`(set! ,id ,rhs)
     (define top? (hash-ref top-names id #f))
     (define target
       (cond
         [top? (format "top.~a" (cify id))]
         [else (genid '__set)]))
     (unless top?
       (printf "~a{\n" indent)
       (printf "~a  GC_CAN_IGNORE Scheme_Object *~a;\n" indent target))
     (generate (format "~a =" target) rhs in-lam (if top? indent (tab indent)) env knowns top-names state lambdas)
     (unless top?
       (printf "~a  SCHEME_UNBOX_VARIABLE_LHS(~a) = ~a;\n" indent (cify id) target)
       (printf "~a}\n" indent))
     (generate ret '(void) in-lam indent env knowns top-names state lambdas)]
    [`(void) (return indent ret #:can-omit? #t "scheme_void")]
    [`(void . ,r)
     (generate ret `(begin ,@r (void)) in-lam indent env knowns top-names state lambdas)]
    [`(values ,r)
     (generate ret r in-lam indent env knowns top-names state lambdas)]
    [`null (return indent ret #:can-omit? #t "scheme_null")]
    [`(#%app . ,r)
     (generate ret r in-lam indent env knowns top-names state lambdas)]
    [`(,rator ,rands ...)
     (define n (length rands))
     (cond
       [(and (symbol? rator)
             (inline-function rator n knowns))
        (define tmp-ids (for/list ([rand (in-list rands)])
                          (and (not (simple? rand state knowns #:can-gc? #f)) (genid '__arg))))
        (define all-simple? (for/and ([tmp-id (in-list tmp-ids)])
                              (not tmp-id)))
        (unless all-simple?
          (printf "~a{\n" indent)
          (for ([tmp-id (in-list tmp-ids)])
            (when tmp-id
              (printf "~a  Scheme_Object *~a;\n" indent tmp-id)))
          (for ([tmp-id (in-list tmp-ids)]
                [rand (in-list rands)])
            (when tmp-id
              (generate (format "~a =" tmp-id) rand in-lam (tab indent) env knowns top-names state lambdas))))
        (define s (generate-simple (cons rator (for/list ([tmp-id (in-list tmp-ids)]
                                                          [rand (in-list rands)])
                                                 (if tmp-id
                                                     (format "~a" tmp-id)
                                                     rand)))
                                   env
                                   top-names
                                   knowns))
        (return (if all-simple? indent (tab indent)) ret s)
        (unless all-simple?
          (printf "~a}\n" indent))]
       [else
        (define knowns-target-lam (let ([f (hash-ref knowns rator #f)])
                                   (and (function? f) (hash-ref lambdas (function-e f)))))
        (when (and (tail-return? ret)
                   knowns-target-lam
                   (state-first-pass? state))
          (union! state (tail-return-lam ret) knowns-target-lam))
        (define direct? (and knowns-target-lam
                             (or (not (tail-return? ret))
                                 (eq? (find! state knowns-target-lam)
                                      (find! state (tail-return-lam ret))))
                             (compatible-args? n (lam-e knowns-target-lam))))
        (define loop-all-simple?
          (and direct?
               (tail-return? ret)
               (for/and ([rand (in-list rands)])
                 (simple? rand state knowns #:can-gc? #f))))
        (define rator-id (cond
                           [direct? #f]
                           [(simple? rator state knowns) #f]
                           [else (genid '__rator)]))
        (define args-id (if (zero? n) 'NULL (genid '__args)))
        (printf "~a{\n" indent)
        (when rator-id
          (printf "~a  Scheme_Object *~a;\n" indent rator-id))
        (unless (zero? n)
          (printf "~a  ~aScheme_Object *~a[~a];\n" indent (if loop-all-simple? "GC_CAN_IGNORE " "") args-id n))
        (let ([indent (tab indent)])
          (when rator-id
            (generate (format "~a =" rator-id) rator in-lam indent env knowns top-names state lambdas))
          (for ([rand (in-list rands)]
                [i (in-naturals)])
            (generate (format "~a[~a] =" args-id i) rand in-lam indent env knowns top-names state lambdas))
          (cond
            [(eq? rator 'values)
             (return indent ret #:can-omit? #t (format "scheme_values(~a, ~a)" n args-id))]
            [(not direct?)
             (define rator-s (or rator-id
                                 (generate-simple rator env top-names knowns)))
             (define use-tail-apply? (and (tail-return? ret)
                                          (or (not (symbol? rator))
                                              (hash-ref env rator #f)
                                              (hash-ref top-names rator #f)
                                              (not (direct-call-primitive? rator e)))))
             (define template (cond
                                [use-tail-apply? "_scheme_tail_apply(~a, ~a, ~a)"]
                                [(or (multiple-return? ret) (tail-return? ret)) "_scheme_apply_multi(~a, ~a, ~a)"]
                                [else "_scheme_apply(~a, ~a, ~a)"]))
             (when use-tail-apply?
               (set-vehicle-can-tail-apply?! (lam-vehicle in-lam) #t))
             (return indent ret (format template rator-s n args-id))]
            [(tail-return? ret)
             (cond
               [(and (eq? knowns-target-lam (tail-return-lam ret))
                     (= n (args-length (tail-return-self-args ret))))
                (hash-set! (lam-loop-targets (tail-return-lam ret)) n #t)
                (for ([id (in-list (tail-return-self-args ret))]
                      [i n])
                  (printf "~a~a = ~a[~a];\n" indent (cify id) args-id i))
                (printf "~agoto __recur_~a_~a;\n" indent (cify (lam-id knowns-target-lam)) n)]
               [else
                (set-lam-need-entry?! knowns-target-lam #t)
                (define use-space? (vehicle-use-argv-space? (lam-vehicle knowns-target-lam)))
                (set-lam-max-jump-argc! knowns-target-lam (max n (lam-max-jump-argc knowns-target-lam)))
                (for ([i n])
                  (printf "~a__argv~a[~a] = ~a[~a];\n" indent
                          (if use-space? "_space" "") i
                          args-id i))
                (when use-space?
                  (printf "~a__argv = __argv_space;\n" indent))
                (printf "~a__argc = ~a;\n" indent n)
                (unless (null? (lam-free-vars knowns-target-lam))
                  (printf "~a__self = top.~a;\n" (cify (lam-id knowns-target-lam))))
                (printf "~agoto __entry_~a;\n" indent (cify (lam-id knowns-target-lam)))])]
            [else
             (return indent ret (let ([s (format "~a(~a, ~a~a)"
                                                 (cify (vehicle-id (lam-vehicle knowns-target-lam))) n args-id
                                                 (if (vehicle-closure? (lam-vehicle knowns-target-lam))
                                                     (format ", top.~a" (cify rator))
                                                     ""))])
                                  (if (vehicle-can-tail-apply? (lam-vehicle knowns-target-lam))
                                      (format "scheme_force_~avalue(~a)" (if (multiple-return? ret) "" "one_") s)
                                      s)))]))
        (printf "~a}\n" indent)])]
    [`,_
     (cond
       [(symbol? e)
        (return indent ret #:can-omit? #t
                (let loop ([e e])
                  (cond
                    [(hash-ref env e #f)
                     => (lambda (r)
                          (cond
                            [(propagate? r) (loop r)]
                            [(mutated? (hash-ref state e #f))
                             (format "SCHEME_UNBOX_VARIABLE(~a)" (cify e))]
                            [else
                             (when (and (return-can-omit? ret)
                                        (state-first-pass? state))
                               (adjust-state! state e -1 'a))
                             (cify e)]))]
                    [(hash-ref top-names e #f) (format "top.~a" (cify e))]
                    [else (format "prims.~a" (cify e))])))]
       [else (generate-quote indent ret e)])]))

(define (generate-let ret e in-lam indent env knowns top-names state lambdas)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define body-env (for/fold ([env env]) ([id (in-list ids)]
                                             [rhs (in-list rhss)]
                                             ;; Leave out of the environment if flattened
                                             ;; into the top sequence:
                                             #:unless (hash-ref top-names id #f))
                        (when (eq? let-id 'letrec)
                          (unless (function? (hash-ref knowns id #f))
                            (log-error "`letrec` binding should have been treated as closed: ~e" id)))
                        (hash-set env id  (get-propagate id rhs env state))))
     (cond
       [(for/and ([id (in-list ids)])
          (propagate? (hash-ref body-env id #f)))
        ;; All propagated: simplify output by avoiding a layer of braces
        (generate ret `(begin . ,body) in-lam indent body-env knowns top-names state lambdas)]
       [else
        (define rhs-env (if (eq? let-id 'let) env body-env))
        (printf "~a{\n" indent)
        (for ([id (in-list ids)]
              #:unless (or (not (referenced? (hash-ref state id #f)))
                           ;; flattened into top?
                           (hash-ref top-names id #f)))
          (printf "~a  Scheme_Object *~a = NULL;\n" indent (cify id)))
        (when (eq? let-id 'letrec*)
          (box-mutable-ids (tab indent) ids state top-names))
        (for ([id (in-list ids)]
              [rhs (in-list rhss)]
              #:unless (propagate? (hash-ref body-env id #f)))
          (cond
            [(eq? let-id 'letrec*)
             (generate "" `(set! ,id ,rhs) in-lam (tab indent) rhs-env knowns top-names state lambdas)]
            [(not (referenced? (hash-ref state id #f)))
             (generate "" rhs in-lam (tab indent) rhs-env knowns top-names state lambdas)]
            [else
             (define c-name (if (hash-ref top-names id #f)
                                (format "top.~a" (cify id))
                                (cify id)))
             (generate (format "~a =" c-name) rhs in-lam (tab indent) rhs-env knowns top-names state lambdas)]))
        (when (eq? let-id 'let)
          (box-mutable-ids (tab indent) ids state top-names))
        (generate ret `(begin . ,body) in-lam (tab indent) body-env knowns top-names state lambdas)
        (printf "~a}\n" indent)])
     (when (state-first-pass? state)
       ;; For each variable that is propagated, remove
       ;; the use of the right-hand side:
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (define r (hash-ref body-env id #f))
         (when (propagate? r)
           (adjust-state! state r (hash-ref state id 0) id)
           (adjust-state! state rhs -1 id)
           (hash-remove! state id)))
       ;; For any variable that has become unused, mark a
       ;; right-hand side function as unused
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (unless (referenced? (hash-ref state id #f))
           (define lam-e (match rhs
                           [`(lambda . ,_) rhs]
                           [`(case-lambda . ,_) rhs]
                           [`,_ #f]))
           (when lam-e
             (define lam (hash-ref lambdas lam-e #f))
             (set-lam-unused?! lam #t)))))]))

(define (generate-closure ret e in-lam indent env knowns top-names state lambdas)
  (define lam (hash-ref lambdas e))
  (cond
    [(and (lam-moved-to-top? lam)
          (not (state-tops-pass? state)))
     ;; Lifted out after discovering that it has no free variables
     (return indent ret (format "top.~a" (cify (lam-id lam))))]
    [else
     (when in-lam (set-lam-under-lambda?! lam #t))
     (define name (format "~a" (lam-id lam)))
     (define free-vars (get-free-vars e env lambdas knowns top-names state))
     (define index-in-closure? (pair? (cdr (vehicle-lams (lam-vehicle lam)))))
     (define-values (min-a max-a) (lambda-arity e #:precise-cases? #t))
     (cond
       [(and (null? free-vars)
             (not index-in-closure?))
        (return indent ret #:can-omit? #t
                (format "scheme_make_prim_w_~aarity(~a, ~s, ~a, ~a)"
                        (if (string? max-a) "case_" "")
                        (cify (lam-id lam)) name min-a max-a))]
       [else
        (define len (+ (length free-vars) (if index-in-closure? 1 0)))
        (define closure-id (genid '__closure))
        (printf "~a{\n" indent)
        (printf "~a  Scheme_Object *~a[~a];\n" indent closure-id len)
        (when index-in-closure?
          (printf "~a  ~a[0] = scheme_make_integer(~a);\n" indent closure-id (lam-index lam)))
        (for ([free-var (in-list free-vars)]
              [i (in-naturals)])
          (printf "~a  ~a[~a] = ~a;\n" indent closure-id (if index-in-closure? (add1 i) i) (cify free-var)))
        (return (tab indent) ret #:can-omit? #t
                (format "scheme_make_prim_closure_w_~aarity(~a, ~a, ~a, ~s, ~a, ~a)"
                        (if (string? max-a) "case_" "")
                        (cify (vehicle-id (lam-vehicle lam)))
                        len closure-id name min-a max-a))
        (printf "~a}\n" indent)])]))

(define (generate-quote indent ret e)
  (cond
    [(return-can-omit? ret) (void)]
    [(simple-quote? e)
     (return indent ret (generate-simple-quote e))]
    [(pair? e)
     (define pair-id (genid '__pair))
     (printf "~a{\n" indent)
     (printf "~a  Scheme_Object *~a_car, *~a_cdr;\n" indent pair-id pair-id)
     (generate-quote (tab indent) (format "~a_car =" pair-id) (car e))
     (generate-quote (tab indent) (format "~a_cdr =" pair-id) (cdr e))
     (return indent ret (format "scheme_make_pair(~a_car, ~a_cdr)" pair-id pair-id))
     (printf "~a}\n" indent)]
    [(vector? e)
     (define vec-id (genid '__vec))
     (printf "~a{\n" indent)
     (printf "~a  Scheme_Object *~a;\n" indent vec-id)
     (unless (zero? (vector-length e))
       (printf "~a  Scheme_Object *~a_elem;\n" indent vec-id))
     (printf "~a  ~a = scheme_make_vector(~a, NULL);\n" indent vec-id (vector-length e))
     (for ([e (in-vector e)]
           [i (in-naturals)])
       (generate-quote (tab indent) (format "~a_elem =" vec-id) e)
       (printf "~a  SCHEME_VEC_ELS(~a)[~a] = ~a_elem;\n" indent vec-id i vec-id))
     (return indent ret (format "~a" vec-id))
     (printf "~a}\n" indent)]
    [else
     (error 'generate-quote "not handled: ~e" e)]))

;; ----------------------------------------

(define (generate-tops e exports knowns top-names state lambdas prim-names)
  (printf "\nvoid scheme_init_startup_instance(Scheme_Instance *__instance) {\n")
  (printf "  REGISTER_SO(prims);\n")
  (printf "  REGISTER_SO(top);\n")
  (for ([id (in-sorted-hash-keys prim-names symbol<?)])
    (printf "  prims.~a = scheme_builtin_value(~s);\n" (cify id) (format "~a" id)))
  (generate-moved-to-top lambdas knowns top-names state)
  (generate-top e knowns top-names state lambdas)
  ;; Expects `(export (rename [<int-id> <ext-id>] ...))` for `exports`
  (for ([ex (in-list (cdr (cadr exports)))])
    (printf "  scheme_instance_add(__instance, ~s, top.~a);\n"
            (format "~a" (cadr ex))
            (cify (car ex))))
  (printf "}\n"))

(define (generate-moved-to-top lambdas knowns top-names state)
  (for ([lam (in-sorted-hash-values lambdas (compare symbol<? lam-id))])
    (when (lam-moved-to-top? lam)
      (generate-top `(define ,(lam-id lam) ,(lam-e lam)) knowns top-names state lambdas))))

(define (generate-top e knowns top-names state lambdas)
  (match e
    [`(begin . ,es)
     (for ([e (in-list es)])
       (generate-top e knowns top-names state lambdas))]
    [`(define ,id (let . ,_))
     (generate-top-let e knowns top-names state lambdas)]
    [`(define ,id (letrec . ,_))
     (generate-top-let e knowns top-names state lambdas)]
    [`(define ,id (letrec* . ,_))
     (generate-top-let e knowns top-names state lambdas)]
    [`(define ,id ,rhs)
     (generate (format "top.~a =" (cify id)) rhs #f "  " #hasheq() knowns top-names state lambdas)]
    [`(define-values (,ids ...) ,rhs)
     (generate (multiple-return "/*needed*/") rhs #f "  " #hasheq() knowns top-names state lambdas)
     (generate-multiple-value-binds "  top." ids state top-names)]
    [`,_
     (generate "" e #f "  " #hasheq() knowns top-names state lambdas)]))

(define (generate-top-let e knowns top-names state lambdas)
  (match e
    [`(define ,id (,let-id ([,ids ,rhss] ...) ,rhs))
     (cond
       [(hash-ref knowns id #f)
        (define new-e `(begin
                         ,@(for/list ([id (in-list ids)]
                                      [rhs (in-list rhss)])
                             `(define ,id ,rhs))
                         (define ,id ,rhs)))
        (generate-top new-e knowns top-names state lambdas)]
       [else
        (match e
          [`(define ,id ,rhs)
           ;; Hide immediate `let` with a `begin`:
           (define new-e `(define ,id (begin ,rhs)))
           (generate-top new-e knowns top-names state lambdas)])])]))

(define (generate-multiple-value-binds prefix ids state top-names)
  (for ([id (in-list ids)]
        [i (in-naturals)]
        #:when (or (hash-ref top-names id #f)
                   (referenced? (hash-ref state id #f))))
    (printf "~a~a = scheme_current_thread->ku.multiple.array[~a];\n" prefix (cify id) i)))
