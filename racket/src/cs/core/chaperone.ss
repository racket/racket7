
(define-record impersonator (val next props))
(define-record chaperone impersonator ())

(define (strip-impersonator v)
  (if (impersonator? v)
      (impersonator-val v)
      v))

(define (impersonate-ref acc rtd pos orig)
  (impersonate-struct-or-property-ref acc rtd (cons rtd pos) orig))

(define (impersonate-struct-or-property-ref acc rtd key orig)
  (cond
   [(and (impersonator? orig)
         (or (not rtd)
             (record? (impersonator-val orig) rtd)))
    (let loop ([v orig])
      (cond
       [(struct-impersonator? v)
        (let ([wrapper (hash-ref (struct-impersonator-procs v) key #f)])
          (cond
           [wrapper
            (let* ([r (cond
                       [(pair? wrapper)
                        (|#%app| (car wrapper) (impersonator-next v))]
                       [else
                        (loop (impersonator-next v))])]
                   [new-r (cond
                           [(pair? wrapper)
                            (|#%app| (cdr wrapper) orig r)]
                           [else (wrapper orig r)])])
              (when (struct-chaperone? v)
                (unless (chaperone-of? new-r r)
                  (raise-arguments-error 'struct-ref
                                         (string-append
                                          "non-chaperone result;\n"
                                          " received a value that is not a chaperone of the original value")
                                         "original" r
                                         "received" new-r)))
              new-r)]
           [else
            (loop (impersonator-next v))]))]
       [(impersonator? v)
        (loop (impersonator-next v))]
       [else (|#%app| acc v)]))]
   [else
    ;; Let accessor report the error:
    (|#%app| acc orig)]))

(define (impersonate-set! set rtd pos orig a)
  (cond
   [(and (impersonator? orig)
         (record? (impersonator-val orig) rtd))
    (let ([key (vector rtd pos)])
      (let loop ([v orig] [a a])
        (cond
         [(struct-impersonator? v)
          (let ([wrapper (hash-ref (struct-impersonator-procs v) key #f)])
            (cond
             [wrapper
              (let ([new-a (cond
                            [(pair? wrapper)
                             (|#%app| (cdr wrapper) orig a)]
                            [else (wrapper orig a)])])
                (when (struct-chaperone? v)
                  (unless (chaperone-of? new-a a)
                    (raise-arguments-error 'struct-set!
                                           (string-append
                                            "non-chaperone result;\n"
                                            " received a value that is not a chaperone of the original value")
                                           "original" a
                                           "received" new-a)))
                (cond
                 [(pair? wrapper)
                  (|#%app| (car wrapper) (impersonator-next v) new-a)]
                 [else
                  (loop (impersonator-next v) new-a)]))]
             [else
              (loop (impersonator-next v) a)]))]
         [(impersonator? v)
          (loop (impersonator-next v) a)]
         [else (set v a)])))]
   [else
    ;; Let mutator report the error:
    (set orig a)]))

;; ----------------------------------------

(define-record-type (impersonator-property create-impersonator-property impersonator-property?)
  (fields name))

(define-record-type (impersonator-property-accessor-procedure
                     make-impersonator-property-accessor-procedure
                     raw:impersonator-property-accessor-procedure?)
  (fields proc name))

(define (make-impersonator-property name)
  (unless (symbol? name)
    (raise-argument-error 'make-impersonator-property "symbol?" name))
  (let ([p (create-impersonator-property name)]
        [predicate-name (string->symbol (format "~a?" name))]
        [accessor-name (string->symbol (format "~a-accessor" name))])
    (values p
            (make-named-procedure
             (lambda (v)
               (and (impersonator? v)
                    (not (eq? none (hash-ref (impersonator-props v) p none)))))
             predicate-name)
            (make-impersonator-property-accessor-procedure
             (lambda (v)
               (and (impersonator? v)
                    (let ([pv (hash-ref (impersonator-props v) p none)])
                      (if (eq? none pv)
                          (raise-argument-error accessor-name
                                                (format "~a?" name)
                                                v)
                          pv))))
             accessor-name))))

(define (impersonator-property-accessor-procedure? v)
  (or (raw:impersonator-property-accessor-procedure? v)
      (and (impersonator? v) (raw:impersonator-property-accessor-procedure? (impersonator-val v)))))

;; ----------------------------------------

(define-record props-impersonator impersonator ())
(define-record props-chaperone chaperone ())

;; Applicable variants:
(define-record props-procedure-impersonator props-impersonator ())
(define-record props-procedure-chaperone props-chaperone ())

(define (add-impersonator-properties who props base-props)
  (let loop ([props props] [base-props base-props])
    (cond
     [(null? props)
      base-props]
     [(impersonator-property? (car props))
      (when (null? (cdr props))
        (raise-arguments-error who "missing value argument after an imperonsonator-property argument"
                               "impersonator property" (car props)))
      (loop (cddr props) (hash-set base-props (car props) (cadr props)))]
     [else
      (raise-argument-error who "impersonator-property?" (car props))])))

;; ----------------------------------------

(define-record struct-impersonator impersonator (procs)) ; hash of proc -> (cons orig-orig wrapper-proc)
(define-record struct-chaperone struct-impersonator ())

(define-record procedure-struct-impersonator struct-impersonator ())
(define-record procedure-struct-chaperone struct-chaperone ())

(define (impersonate-struct v . args)
  (do-impersonate-struct 'impersonate-struct #f v args make-struct-impersonator make-procedure-struct-impersonator))

(define (chaperone-struct v . args)
  (do-impersonate-struct 'chaperone-struct #t v args make-struct-chaperone make-procedure-struct-chaperone))

(define (do-impersonate-struct who as-chaperone? v args make-struct-impersonator make-procedure-struct-impersonator)
  (cond
   [(null? args) v]
   [else
    (let* ([st (if (struct-type? (car args))
                   (car args)
                   #f)]
           [orig-args (if st (cdr args) args)]
           [val (strip-impersonator v)]
           [orig-iprops (if (impersonator? v) (impersonator-props v) empty-hasheq)])
      (unless (or (not st) (record? val st))
        (raise-arguments-error who "given value is not an instance of the given structure type"
                               "struct type" st
                               "value" v))
      (let loop ([first? (not st)]
                 [args orig-args]
                 [props empty-hash]
                 [saw-props empty-hash]
                 [witnessed? (and st #t)]
                 [iprops orig-iprops])
        (let ([get-proc
               (lambda (what args arity proc->key key-applies?)
                 (let* ([key-proc (strip-impersonator (car args))]
                        [key (proc->key key-proc)])
                   (when (hash-ref saw-props key #f)
                     (raise-arguments-error who
                                            "given operation accesses the same value as a previous operation argument"
                                            "operation kind" what
                                            "operation procedure" (car args)))
                   (when key-applies?
                     (unless (key-applies? key val)
                       (raise-arguments-error who
                                              "operation does not apply to given value"
                                              "operation kind" what
                                              "operation procedure" (car args)
                                              "value" v)))
                   (when (null? (cdr args))
                     (raise-arguments-error who
                                            "missing redirection procedure after operation"
                                            "operation kind" what
                                            "operation procedure" (car args)))
                   (let ([proc (cadr args)])
                     (when proc
                       (unless (procedure-arity-includes? proc arity)
                         (raise-arguments-error who
                                                "operation's redirection procedure does not match the expected arity"
                                                "given" proc
                                                "expected" (string-append
                                                            "(or/c #f (procedure-arity-includes/c " (number->string arity) "))")
                                                "operation kind" what
                                                "operation procedure" (car args))))
                     (loop #f
                           (cddr args)
                           (if proc
                               (hash-set props key
                                         (if (impersonator? (car args))
                                             (cons (car args) ; save original accessor, in case it's impersonated
                                                   proc)      ; the interposition proc
                                             proc))
                               props)
                           (hash-set saw-props key #t)
                           (or witnessed? key-applies?)
                           iprops))))])
          (cond
           [(null? args)
            (unless as-chaperone?
              (check-accessors-paired-with-mutators who orig-args v))
            (unless witnessed?
              (raise-arguments-error who
                                     (string-append "cannot "
                                                    (if as-chaperone? "chaperone" "impersonate")
                                                    " value as a structure without a witness")
                                     "explanation" (string-append
                                                    "a structure type, accessor, or mutator acts as a witness\n"
                                                    "   that the given value's representation can be chaperoned or impersonated")
                                     "given value" v))
            (when (authentic? v)
              (raise-arguments-error who
                                     (string-append "cannot "
                                                    (if as-chaperone? "chaperone" "impersonate")
                                                    " instance of an authentic structure type")
                                     "given value" v))
            (if (and (zero? (hash-count props))
                     (eq? iprops orig-iprops))
                v
                (let ([mk (if (procedure? v) make-procedure-struct-impersonator make-struct-impersonator)])
                  (mk val v iprops props)))]
           [(impersonator-property? (car args))
            (loop #f
                  '()
                  props
                  saw-props
                  witnessed?
                  (add-impersonator-properties who args iprops))]
           [(struct-accessor-procedure? (car args))
            (get-proc "accessor" args 2
                      struct-accessor-procedure-rtd+pos
                      (lambda (rtd+pos v)
                        (and (record? v (car rtd+pos))
                             (begin
                               (unless (or as-chaperone?
                                           (struct-type-field-mutable? (car rtd+pos) (cdr rtd+pos)))
                                 (raise-arguments-error who
                                                        "cannot replace operation for an immutable field"
                                                        "operation kind" "property accessor"
                                                        "operation procedure" (car args)))
                               #t))))]
           [(struct-mutator-procedure? (car args))
            (get-proc "mutator" args 2
                      (lambda (proc)
                        (let ([rtd+pos (struct-mutator-procedure-rtd+pos proc)])
                          (vector (car rtd+pos) (cdr rtd+pos))))
                      (lambda (rtd++pos v)
                        (record? v (vector-ref rtd++pos 0))))]
           [(struct-type-property-accessor-procedure? (car args))
            (get-proc "property accessor" args 2
                      (lambda (proc) proc)
                      (lambda (proc v)
                        (unless (or as-chaperone?
                                    (struct-type-property-accessor-procedure-can-impersonate? proc))
                          (raise-arguments-error who
                                                 "operation cannot be impersonated"
                                                 "operation kind" "property accessor"
                                                 "operation procedure" (car args)))
                        ((struct-type-property-accessor-procedure-pred proc) v)))]
           [(and as-chaperone?
                 (equal? struct-info (car args)))
            (get-proc "struct-info procedure" args 2 (lambda (proc) proc) #f)]
           [else
            (raise-argument-error who
                                  (string-append
                                   "(or/c "
                                   (if first? "struct-type?\n      " "")
                                   "struct-accessor-procedure?"
                                   "\n      struct-mutator-procedure?"
                                   "\n      struct-type-property-accessor-procedure?"
                                   (if as-chaperone? "\n      struct-info" "")
                                   ")")
                                  (car args))]))))]))

(define (check-accessors-paired-with-mutators who args v)
  (define mutator-reps
    (let loop ([args args])
      (cond
       [(null? args) empty-hash]
       [(struct-mutator-procedure? (car args))
        (hash-set (loop (cddr args))
                  (struct-mutator-procedure-rtd+pos (strip-impersonator (car args)))
                  #t)]
       [else
        (loop (cddr args))])))
  (let loop ([args args])
    (cond
       [(null? args) empty-hash]
       [(struct-accessor-procedure? (car args))
        (let ([rtd+pos (struct-accessor-procedure-rtd+pos (strip-impersonator (car args)))])
          (unless (or (struct-type-transparent? (car rtd+pos))
                      (hash-ref mutator-reps rtd+pos #f))
            (raise-arguments-error who
                                   "accessor redirection for a non-transparent field requires a mutator redirection"
                                   "explanation" "a mutator redirection acts as a witness that access is allowed"
                                   "accessor" (car args)
                                   "value to impersonate" v)))
        (loop (cddr args))]
       [else
        (loop (cddr args))])))

;; ----------------------------------------

(define (set-impersonator-applicables!)
  (struct-property-set! prop:procedure
                        (record-type-descriptor props-procedure-impersonator)
                        impersonate-apply)  
  (struct-property-set! prop:procedure
                        (record-type-descriptor props-procedure-chaperone)
                        impersonate-apply)

  (struct-property-set! prop:procedure
                        (record-type-descriptor impersonator-property-accessor-procedure)
                        0))

(define (set-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor props-impersonator)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor props-chaperone)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor props-procedure-impersonator)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor props-procedure-chaperone)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c)))))
