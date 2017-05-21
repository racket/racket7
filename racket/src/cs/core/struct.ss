(define-record struct-type-prop (name guard supers))

;; Record the properties that are implemented by each rtd:
(define rtd-props (make-weak-eq-hashtable))

;; Maps a property-accessor function to `(cons predicate-proc can-impersonate)`:
(define property-accessors (make-weak-eq-hashtable))

(define (struct-type-property? v)
  (struct-type-prop? v))

(define make-struct-type-property
  (case-lambda
    [(name) (make-struct-type-property name #f '() #f)]
    [(name guard) (make-struct-type-property name guard '() #f)]
    [(name guard supers) (make-struct-type-property name guard supers #f)]
    [(name guard supers can-impersonate?)
     (unless (symbol? name)
       (raise-argument-error 'make-struct-type-property "symbol?" name))
     (unless (or (not guard)
                 (eq? guard 'can-impersonate)
                 (and (#%procedure? guard) ; avoid `procedure?` until it's defined
                      (bitwise-bit-set? (#%procedure-arity-mask guard) 2))
                 (and (procedure? guard)
                      (procedure-arity-includes? guard 2)))
       (raise-argument-error 'make-struct-type-property "(or/c (procedure-arity-includes/c 2) #f 'can-impersonate)" guard))
     (unless (and (or (null? supers) ; avoid `list?` until it's defined
                      (list? supers))
                  (andmap (lambda (p)
                            (and (pair? p)
                                 (struct-type-property? (car p))
                                 (procedure? (cdr p))
                                 (procedure-arity-includes? (cdr p) 1)))
                          supers))
       (raise-argument-error 'make-struct-type-property "(listof (cons/c struct-type-property? (procedure-arity-includes/c 1)))" supers))
     (let* ([can-impersonate? (and (or can-impersonate? (eq? guard 'can-impersonate)) #t)]
            [st (make-struct-type-prop name (and (not (eq? guard 'can-impersonate)) guard) supers)]
            [pred (lambda (v)
                    (let* ([v (strip-impersonator v)]
                           [rtd (if (record-type-descriptor? v)
                                    v
                                    (and (record? v)
                                         (record-rtd v)))])
                      (and rtd
                           (not (eq? none (struct-property-ref st rtd none))))))]
            [fail (lambda (v)
                    (raise-argument-error (string->symbol (string-append
                                                           (symbol->string name)
                                                           "-ref"))
                                          (string-append
                                           (symbol->string name)
                                           "?")
                                          v))])
       (letrec ([acc (lambda (v)
                       (cond
                        [(and (impersonator? v)
                              (pred v))
                         (impersonate-struct-or-property-ref acc #f acc v)]
                        [else
                         (let* ([rtd (if (record-type-descriptor? v)
                                         v
                                         (and (record? v)
                                              (record-rtd v)))])
                           (if rtd
                               (let ([v (struct-property-ref st rtd none)])
                                 (if (eq? v none)
                                     (fail v)
                                     v))
                               (fail v)))]))])
         (hashtable-set! property-accessors
                         acc
                         (cons pred can-impersonate?))
         (values st
                 pred
                 acc)))]))

(define (struct-type-property-accessor-procedure? v)
  (and (procedure? v)
       (hashtable-ref property-accessors v #f)
       #t))

(define (struct-type-property-accessor-procedure-pred v)
  (car (hashtable-ref property-accessors v #f)))

(define (struct-type-property-accessor-procedure-can-impersonate? v)
  (cdr (hashtable-ref property-accessors v #f)))

(define (struct-property-ref prop rtd default)
  (getprop (record-type-uid rtd) prop default))

(define (struct-property-set! prop rtd val)
  (putprop (record-type-uid rtd) prop val))

;; ----------------------------------------

(define-record-type (inspector new-inspector inspector?) (fields parent))

(define root-inspector (new-inspector #f))

(define make-inspector
  (case-lambda
    [() (new-inspector (|#%app| current-inspector))]
    [(i)
     (unless (inspector? i)
       (raise-argument-error 'current-inspector
                             "inspector?"
                             i))
     (new-inspector i)]))

(define (inspector-superior? sup-insp sub-insp)
  (unless (inspector? sup-insp)
    (raise-argument-error 'inspector-superior? "inspector?" sup-insp))
  (unless (inspector? sub-insp)
    (raise-argument-error 'inspector-superior? "inspector?" sub-insp))
  (if (eq? sub-insp root-inspector)
      #f
      (let ([parent (inspector-parent sub-insp)])
        (or (eq? parent sup-insp)
            (inspector-superior? sup-insp parent)))))

(define (inspector-ref rtd)
  (getprop (record-type-uid rtd) 'inspector none))

(define (inspector-set! rtd insp)
  (putprop (record-type-uid rtd) 'inspector insp))

;; ----------------------------------------

(define (check-make-struct-type-arguments who name parent-rtd fields-count auto-fields
                                          props insp proc-spec immutables guard constructor-name)
  (unless (symbol? name)
    (raise-argument-error who "symbol?" name))
  (unless (or (not parent-rtd)
              (struct-type? parent-rtd))
    (raise-argument-error who "(or/c #f struct-type?)" parent-rtd))
  (unless (exact-nonnegative-integer? fields-count)
    (raise-argument-error who "exact-nonnegative-integer?" fields-count))
  (unless (exact-nonnegative-integer? auto-fields)
    (raise-argument-error who "exact-nonnegative-integer?" auto-fields))
  (unless (or (not proc-spec)
              (procedure? proc-spec)
              (exact-nonnegative-integer? proc-spec))
    (raise-argument-error who "(or/c procedure? exact-nonnegative-integer? #f)" proc-spec))
  (unless (and (list props)
               (andmap (lambda (i) (and (pair? i) (struct-type-property? (car i))))
                       props))
    (raise-argument-error who "(listof (cons/c struct-type-property? any/c))" props))
  (unless (or (not insp)
              (inspector? insp)
              (eq? insp 'prefab))
    (raise-argument-error who "(or/c inspector? #f 'prefab)" insp))
  (unless (and (list? immutables)
               (andmap exact-nonnegative-integer? immutables))
    (raise-argument-error who "(listof exact-nonnegative-integer?)" immutables))
  (unless (or (not guard)
              (procedure? guard))
    (raise-argument-error who "(or/c #f procedure?)" guard))
  (unless (or (not constructor-name)
              (symbol? constructor-name))
    (raise-argument-error who "(or/c #f symbol?)" constructor-name))
  
  (let ([props-ht
         ;; Check for duplicates and record property values for
         ;; other checks
         (let loop ([props props] [ht empty-hasheq])
           (cond
            [(null? props)
             (if proc-spec
                 (let ([v (hash-ref ht prop:procedure none)])
                   (unless (or (eq? v none)
                               (eq? v proc-spec))
                     (raise-arguments-error who
                                            "duplicate property binding"
                                            "property" prop:procedure))
                   (hash-set ht prop:procedure proc-spec))
                 ht)]
            [else
             (let ([v (hash-ref ht (caar props) none)])
               (unless (or (eq? v none)
                           (eq? v (cdar props)))
                 (raise-arguments-error who
                                        "duplicate property binding"
                                        "property" (caar props)))
               (loop (cdr props) (hash-set ht (caar props) (cdar props))))]))])

    (when (eq? insp 'prefab)
      (let ([bad
             (or (and (impersonator? parent-rtd)
                      "chaperoned supertype disallowed for non-generative structure type")
                 (and parent-rtd
                      (not (eq? (inspector-ref parent-rtd) 'prefab))
                      "generative supertype disallowed for non-generative structure type")
                 (and (pair? props)
                      "properties disallowed for non-generative structure type")
                 (and proc-spec
                      "procedure specification disallowed for non-generative structure type")
                 (and guard
                      "guard disallowed for non-generative structure type"))])
      (when bad
        (raise-arguments-error who bad
                               "structure type name" name))))

    (let loop ([ht empty-hasheqv] [imms immutables])
      (cond
       [(null? imms) (void)]
       [else
        (let ([i (car imms)])
          (when (hash-ref ht i #f)
            (raise-arguments-error who
                                   "redundant immutable field index"
                                   "index" i
                                   "in list" immutables))
          (unless (< i fields-count)
            (raise-arguments-error who
                                   "index for immutable field >= initialized-field count"
                                   "index" i
                                   "initialized-field count" fields-count
                                   "in list" immutables))
          (loop (hash-set ht i #t) (cdr imms)))]))

    (let ([v (hash-ref props-ht prop:procedure #f)])
      (when v
        (cond
         [(exact-nonnegative-integer? v)
          (unless (< v fields-count)
            (raise-arguments-error who
                                   "index for procedure >= initialized-field count"
                                   "index" v
                                   "field count" fields-count))
          (unless (or (eq? v proc-spec) (chez:memv v immutables))
            (raise-arguments-error who
                                   "field is not specified as immutable for a prop:procedure index"
                                   "index" v))]
         [(procedure? v)
          (void)]
         [else
          (raise-arguments-error who
                                 "given value did not satisfy the contract for prop:procedure"
                                 "expected" "(or/c procedure? exact-nonnegative-integer?)"
                                 "given" v)])))

    (when parent-rtd
      (let ([authentic? (not (eq? (hash-ref props-ht prop:authentic none) none))]
            [authentic-parent? (struct-property-ref prop:authentic parent-rtd #f)])
        (when (not (eq? authentic? authentic-parent?))
          (if authentic?
              (raise-arguments-error who
                                     "cannot make an authentic subtype of a non-authentic type"
                                     "type name" name
                                     "non-authentic type" parent-rtd)
              (raise-arguments-error who
                                     "cannot make a non-authentic subtype of an authentic type"
                                     "type name" name
                                     "authentic type" parent-rtd)))))

    (when guard
      (let ([expected-count (+ 1 fields-count
                               (if parent-rtd
                                   (- (struct-type-field-count parent-rtd)
                                      (struct-type-auto-field-count parent-rtd))
                                   0))])
        (unless (procedure-arity-includes? guard expected-count)
          (raise-arguments-error who
                                 (string-append
                                  "guard procedure does not accept correct number of arguments;\n"
                                  " should accept one more than the number of constructor arguments")
                                 "guard procedure" guard
                                 "expected arity" expected-count))))))

;; ----------------------------------------

;; Records which fields of an rtd are mutable, where an rtd that is
;; not in the table has no mutable fields:
(define rtd-mutables (make-weak-eq-hashtable))

;; Accessors and mutators that need a position are wrapped in these records:
(define-record position-based-accessor (rtd offset field-count))
(define-record position-based-mutator (rtd offset field-count))

;; Register other procedures in hash tables; avoid wrapping to
;; avoid making the procedures slower
(define struct-constructors (make-weak-eq-hashtable))
(define struct-predicates (make-weak-eq-hashtable))
(define struct-field-accessors (make-weak-eq-hashtable))
(define struct-field-mutators (make-weak-eq-hashtable))

(define (register-struct-constructor! p)
  (hashtable-set! struct-constructors p #t))

(define (register-struct-predicate! p)
  (hashtable-set! struct-predicates p #t))

(define (register-struct-field-accessor! p rtd pos)
  (hashtable-set! struct-field-accessors p (cons rtd pos)))

(define (register-struct-field-mutator! p rtd pos)
  (hashtable-set! struct-field-mutators p (cons rtd pos)))

(define (struct-constructor-procedure? v)
  (and (procedure? v)
       (hashtable-ref struct-constructors (strip-impersonator v) #f)))

(define (struct-predicate-procedure? v)
  (and (procedure? v)
       (hashtable-ref struct-predicates (strip-impersonator v) #f)))

(define (struct-accessor-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (or (position-based-accessor? v)
             (hashtable-ref struct-field-accessors v #f)))
       #t))

(define (struct-mutator-procedure? v)
  (and (procedure? v)
       (let ([v (strip-impersonator v)])
         (or (position-based-mutator? v)
             (hashtable-ref struct-field-mutators v #f)))
       #t))

(define (struct-accessor-procedure-rtd+pos v)
  (hashtable-ref struct-field-accessors v #f))

(define (struct-mutator-procedure-rtd+pos v)
  (hashtable-ref struct-field-mutators v #f))

;; ----------------------------------------

;; General structure-type creation, but not called when a `schemify`
;; transformation keeps the record type exposed to the compiler
(define make-struct-type
  (case-lambda 
    [(name parent-rtd fields auto-fields)
     (make-struct-type name parent-rtd fields auto-fields #f '() (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd fields auto-fields auto-val)
     (make-struct-type name parent-rtd fields auto-fields auto-val '() (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd fields auto-fields auto-val props)
     (make-struct-type name parent-rtd fields auto-fields auto-val props (|#%app| current-inspector) #f '() #f name)]
    [(name parent-rtd fields auto-fields auto-val props insp)
     (make-struct-type name parent-rtd fields auto-fields auto-val props insp #f '() #f name)]
    [(name parent-rtd fields auto-fields auto-val props insp proc-spec)
     (make-struct-type name parent-rtd fields auto-fields auto-val props insp proc-spec '() #f name)]
    [(name parent-rtd fields auto-fields auto-val props insp proc-spec immutables)
     (make-struct-type name parent-rtd fields auto-fields auto-val props insp proc-spec immutables #f name)]
    [(name parent-rtd fields auto-fields auto-val props insp proc-spec immutables guard)
     (make-struct-type name parent-rtd fields auto-fields auto-val props insp proc-spec immutables guard name)]
    [(name parent-rtd fields-count auto-fields auto-val props insp proc-spec immutables guard constructor-name)
     (check-make-struct-type-arguments 'make-struct-type name parent-rtd fields-count auto-fields
                                       props insp proc-spec immutables guard constructor-name)
     (let* ([prefab-uid (and (eq? insp 'prefab)
                             (structure-type-lookup-prefab-uid name parent-rtd fields-count auto-fields auto-val immutables))]
            [total-field-count (+ (if parent-rtd
                                      (struct-type-field-count parent-rtd)
                                      0)
                                  fields-count
                                  auto-fields)]
            [rtd (make-record-type-descriptor name
                                              parent-rtd
                                              prefab-uid #f #f
                                              (make-fields (+ fields-count auto-fields)))]
            [parent-count (if parent-rtd
                              (struct-type-field-count parent-rtd)
                              0)]
            [parent-auto-field-count (if parent-rtd
                                         (struct-type-auto-field-count parent-rtd)
                                         0)]
            [total-auto-field-count (+ auto-fields parent-auto-field-count)]
            [auto-field-adder (and (positive? total-auto-field-count)
                                   (let ([pfa (and parent-rtd
                                                   (struct-type-auto-field-adder parent-rtd))])
                                     (lambda (args)
                                       (args-insert args fields-count auto-fields auto-val pfa))))])
       (struct-type-install-properties! rtd name fields-count auto-fields parent-rtd
                                        props insp proc-spec immutables guard constructor-name
                                        #t)
       (when auto-field-adder
         (putprop (record-type-uid rtd) 'auto-field (cons total-auto-field-count auto-field-adder)))
       (let ([ctr (let ([c (record-constructor rtd)])
                    (if (zero? total-auto-field-count)
                        c
                        (procedure-reduce-arity
                         (lambda args
                           (apply c (auto-field-adder args)))
                         (- total-field-count total-auto-field-count))))]
             [pred (lambda (v)
                     (or (record? v rtd)
                         (and (impersonator? v)
                              (record? (impersonator-val v) rtd))))])
         (register-struct-constructor! ctr)
         (register-struct-constructor! pred)
         (values rtd
                 ctr
                 pred
                 (make-position-based-accessor rtd parent-count (+ fields-count auto-fields))
                 (make-position-based-mutator rtd parent-count (+ fields-count auto-fields)))))]))

;; Called both by `make-struct-type` and by a `schemify` transformation:
(define struct-type-install-properties!
  (case-lambda
   [(rtd name fields auto-fields parent-rtd)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd '() (|#%app| current-inspector) #f '() #f name #f)]
   [(rtd name fields auto-fields parent-rtd props)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props (|#%app| current-inspector) #f '() #f name #f)]
   [(rtd name fields auto-fields parent-rtd props insp)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp #f '() #f name #f)]
   [(rtd name fields auto-fields parent-rtd props insp proc-spec)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec '() #f name #f)]
   [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec immutables #f name #f)]
   [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard name #f)]
   [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard constructor-name)
    (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard constructor-name #f)]
   [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard constructor-name skip-checks?)
    (unless skip-checks?
      (check-make-struct-type-arguments 'make-struct-type name parent-rtd fields auto-fields
                                        props insp proc-spec immutables guard constructor-name))
    (unless (eq? insp 'prefab) ; everything for prefab must be covered in `prefab-key+count->rtd`
      (let* ([parent-props
              (if parent-rtd
                  (hashtable-ref rtd-props parent-rtd '())
                  '())]
             [all-immutables (if (integer? proc-spec)
                                 (cons proc-spec immutables)
                                 immutables)]
             [mutables (immutables->mutables all-immutables fields)])
        (when (not parent-rtd)
          (record-type-equal-procedure rtd default-struct-equal?)
          (record-type-hash-procedure rtd default-struct-hash))
        ;; Record properties implemented by this type:
        (hashtable-set! rtd-props rtd (let ([props (append (map car props) parent-props)])
                                        (if proc-spec
                                            (cons prop:procedure props)
                                            props)))
        (unless (equal? '#() mutables)
          (hashtable-set! rtd-mutables rtd mutables))
        ;; Copy parent properties for this type:
        (for-each (lambda (prop)
                    (struct-property-set! prop rtd (struct-property-ref prop parent-rtd #f)))
                  parent-props)
        ;; Install new property values
        (for-each (lambda (prop+val)
                    (let loop ([prop (car prop+val)]
                               [val (cdr prop+val)])
                      (let ([guarded-val
                             (let ([guard (struct-type-prop-guard prop)])
                               (if guard
                                   (let ([parent-count (if parent-rtd
                                                           (struct-type-field-count parent-rtd)
                                                           0)])
                                     (guard val
                                            (list name
                                                  fields
                                                  auto-fields
                                                  (make-position-based-accessor rtd parent-count (+ fields auto-fields))
                                                  (make-position-based-mutator rtd parent-count (+ fields auto-fields))
                                                  all-immutables
                                                  parent-rtd
                                                  #f)))
                                   val))])
                        (when (eq? prop prop:equal+hash)
                          (record-type-equal-procedure rtd (car val))
                          (record-type-hash-procedure rtd (cadr val)))
                        (struct-property-set! prop rtd guarded-val)
                        (for-each (lambda (super)
                                    (loop (car super)
                                          ((cdr super) guarded-val)))
                                  (struct-type-prop-supers prop)))))
                  (if proc-spec
                      (cons (cons prop:procedure proc-spec) props)
                      props))
        ;; Record inspector
        (inspector-set! rtd insp)))]))

;; Used by a `schemify` transformation:
(define (structure-type-lookup-prefab-uid name parent-rtd fields-count auto-fields auto-val immutables)
  ;; Return a UID for a prefab structure type. We can assume that
  ;; `immutables` is well-formed, and checking an error reporting will
  ;; happen latter if necessary.
  (let ([prefab-key (derive-prefab-key name
                                       (and parent-rtd
                                            (getprop (record-type-uid parent-rtd) 'prefab-key+count))
                                       fields-count
                                       immutables auto-fields auto-val)]
        [total-field-count (+ (if parent-rtd
                                  (struct-type-field-count parent-rtd)
                                  0)
                              fields-count
                              auto-fields)])
    (record-type-uid
     (prefab-key+count->rtd (cons prefab-key total-field-count)))))

(define (prefab-key+count->rtd prefab-key+count)
  (cond
   [(and prefabs
         (hash-ref prefabs prefab-key+count #f))
    => (lambda (rtd) rtd)]
   [else
    (let* ([prefab-key (car prefab-key+count)]
           [name (if (symbol? prefab-key)
                     prefab-key
                     (car prefab-key))]
           [parent-prefab-key+count
            (prefab-key->parent-prefab-key+count (car prefab-key+count))]
           [parent-rtd (and parent-prefab-key+count
                            (prefab-key+count->rtd parent-prefab-key+count))]
           [num-fields (- (cdr prefab-key+count)
                          (if parent-prefab-key+count
                              (cdr parent-prefab-key+count)
                              0))]
           [uid (gensym name)]
           [rtd (make-record-type-descriptor name
                                             parent-rtd
                                             uid #f #f
                                             (make-fields num-fields))]
           [mutables (prefab-key-mutables prefab-key)])
      (with-interrupts-disabled
       (cond
        [(and prefabs
              (hash-ref prefabs prefab-key+count #f))
         ;; rtd was created concurrently
         => (lambda (rtd) rtd)]
        [else
         (putprop uid 'prefab-key+count prefab-key+count)
         (unless prefabs (set! prefabs (make-weak-hash)))
         (hash-set! prefabs prefab-key+count rtd)
         (unless parent-rtd
           (record-type-equal-procedure rtd default-struct-equal?)
           (record-type-hash-procedure rtd default-struct-hash))
         (unless (equal? mutables '#())
           (hashtable-set! rtd-mutables rtd mutables))
         (inspector-set! rtd 'prefab)
         rtd])))]))

(define make-struct-field-accessor
  (case-lambda
    [(pba pos)
     (let* ([rtd (position-based-accessor-rtd pba)]
            [p (record-field-accessor rtd
                                      (+ pos (position-based-accessor-offset pba)))]
           [wrap-p
            (lambda (v)
              (if (impersonator? v)
                  (impersonate-ref p rtd pos v)
                  (p v)))])
       (register-struct-field-accessor! wrap-p rtd pos)
       wrap-p)]
    [(pba pos name)
     (make-struct-field-accessor pba pos)]))

(define make-struct-field-mutator
  (case-lambda
   [(pbm pos)
    (let* ([rtd (position-based-mutator-rtd pbm)]
           [p (record-field-mutator rtd
                                    (+ pos (position-based-mutator-offset pbm)))]
           [wrap-p
            (lambda (v a)
              (if (impersonator? v)
                  (impersonate-set! p rtd pos v a)
                  (p v a)))])
      (register-struct-field-mutator! wrap-p rtd pos)
      wrap-p)]
   [(pbm pos name)
    (make-struct-field-mutator pbm pos)]))

(define (args-insert args fields-count auto-fields auto-val pfa)
  (let loop ([fields-count fields-count] [args args])
    (if (zero? fields-count)
        (let loop ([auto-fields auto-fields])
          (if (zero? auto-fields)
              (if pfa
                  (pfa args)
                  args)
              (cons auto-val (loop (fx1- auto-fields)))))
        (cons (car args) (loop (fx1- fields-count) (cdr args))))))                          

;; ----------------------------------------

(define (struct-type? v) (record-type-descriptor? v))

(define (procedure-struct-type? v)
  (unless (struct-type? v)
    (raise-argument-error 'procedure-struct-type? "struct-type?" v))
  (procedure-struct? v))

(define (struct? v)
  (and (record? v)
       (struct-type-any-transparent? (record-rtd v))))

(define (struct-info v)
  (cond
   [(not (record? v)) (values #f #t)]
   [else (next-visible-struct-type (record-rtd v))]))

(define (next-visible-struct-type rtd)
  (let loop ([rtd rtd] [skipped? #f])
    (cond
     [(struct-type-immediate-transparent? rtd)
      (values rtd skipped?)]
     [else
      (let ([parent-rtd (record-type-parent rtd)])
        (if parent-rtd
            (loop parent-rtd #t)
            (values #f #t)))])))

(define (struct-type-info rtd)
  (unless (struct-type? rtd)
    (raise-argument-error 'struct-type-info "struct-type?" rtd))
  (check-inspector-access 'struct-type-info rtd)
  (let* ([auto-fields (struct-type-auto-field-count rtd)]
         [fields-count (- (#%vector-length (record-type-field-names rtd))
                          auto-fields)]
         [parent-rtd (record-type-parent rtd)]
         [parent-count (if parent-rtd
                           (struct-type-field-count parent-rtd)
                           0)])
    (let-values ([(next-rtd skipped?)
                  (if parent-rtd
                      (next-visible-struct-type parent-rtd)
                      (values #f #f))])
      (values (record-type-name rtd)
              fields-count
              auto-fields
              (make-position-based-accessor rtd parent-count (+ fields-count auto-fields))
              (make-position-based-mutator rtd parent-count (+ fields-count auto-fields))
              (mutables->immutables (hashtable-ref rtd-mutables rtd '#()) fields-count)
              next-rtd
              skipped?))))

(define (check-inspector-access who rtd)
  (unless (struct-type-immediate-transparent? rtd)
    (raise-arguments-error who
                           "current inspector cannot extract info for structure type"
                           "structure type" rtd)))

(define (struct-type-make-constructor rtd)
  (unless (struct-type? rtd)
    (raise-argument-error 'struct-type-make-constructor "struct-type?" rtd))
  (check-inspector-access 'struct-type-make-constructor rtd)
  (let ([ctr (let ([c (record-constructor rtd)]
                   [auto-field-adder (struct-type-auto-field-adder rtd)])
               (if auto-field-adder
                   (procedure-reduce-arity
                    (lambda args
                      (apply c (auto-field-adder args)))
                    (- (#%vector-length (record-type-field-names rtd))
                       (struct-type-auto-field-count rtd)))
                   c))])
    (register-struct-constructor! ctr)
    ctr))

(define (struct-type-make-predicate rtd)
  (unless (struct-type? rtd)
    (raise-argument-error 'struct-type-make-predicate "struct-type?" rtd))
  (check-inspector-access 'struct-type-make-predicate rtd)
  (let ([pred (lambda (v)
                (or (record? v rtd)
                    (and (impersonator? v)
                         (record? (impersonator-val v) rtd))))])
    (register-struct-constructor! pred)
    pred))

(define (struct-type-auto-field-count rtd)
  (car (getprop (record-type-uid rtd) 'auto-field '(0 . #f))))

(define (struct-type-auto-field-adder rtd)
  (cdr (getprop (record-type-uid rtd) 'auto-field '(0 . #f))))

(define (struct-type-field-count rtd)
  (+ (let ([p-rtd (record-type-parent rtd)])
       (if p-rtd
           (struct-type-field-count p-rtd)
           0))
     (vector-length (record-type-field-names rtd))))

(define (struct-type-parent-field-count rtd)
  (let ([p-rtd (record-type-parent rtd)])
    (if p-rtd
        (struct-type-field-count p-rtd)
        0)))

(define (struct-type-field-mutable? rtd pos)
  (let ([mutables (hashtable-ref rtd-mutables rtd '#())])
    (let loop ([j (#%vector-length mutables)])
      (cond
       [(fx= j 0) #f]
       [else
        (let ([j (fx1- j)])
          (or (eqv? pos (#%vector-ref mutables j))
              (loop j)))]))))

(define (unsafe-struct-ref s i)
  (#3%vector-ref s i))
(define (unsafe-struct-set! s i v)
  (#3%vector-set! s i v))

(define-values (prop:equal+hash equal+hash? equal+hash-ref)
  (make-struct-type-property 'equal+hash
                             (lambda (val info)
                               (cons (gensym) val))))

(define-values (prop:authentic authentic? authentic-ref)
  (make-struct-type-property 'authentic (lambda (val info) #t)))

(define (struct-type-immediate-transparent? rtd)
  (let ([insp (inspector-ref rtd)])
    (and (not (eq? insp none))
         (or (not insp)
             (eq? insp 'prefab)
             (inspector-superior? (|#%app| current-inspector) insp)))))

;; Check whether a structure type is fully transparent
(define (struct-type-transparent? rtd)
  (and (struct-type-immediate-transparent? rtd)
       (let ([p-rtd (record-type-parent rtd)])
         (or (not p-rtd)
             (struct-type-transparent? p-rtd)))))

;; Checks whether a structure type is at least partially trasparent
(define (struct-type-any-transparent? rtd)
  (or (struct-type-immediate-transparent? rtd)
      (let ([p-rtd (record-type-parent rtd)])
        (and p-rtd
             (struct-type-any-transparent? p-rtd)))))

(define (struct-transparent-type r)
  (let ([t (record-rtd r)])
    (and (struct-type-transparent? t)
         t)))

(define (default-struct-equal? s1 s2 eql?)
  (let ([t1 (record-rtd s1)]
        [t2 (record-rtd s2)])
    (and (eq? t1 t2)
         (struct-type-transparent? t1)
         (let ([n (struct-type-field-count t1)])
           (let loop ([j 0])
             (if (fx= j n)
                 #t
                 (and (eql? (unsafe-struct-ref s1 j)
                            (unsafe-struct-ref s2 j))
                      (loop (fx+ j 1)))))))))
         
(define (default-struct-hash s hash-code)
  (let ([t (record-rtd s)])
    (if (struct-type-transparent? t)
        (let ([n (struct-type-field-count t)])
          (let loop ([j 0] [hc 0])
            (if (fx= j n)
                hc
                (loop (fx+ j 1)
                      (hash-code-combine hc (hash-code (unsafe-struct-ref s j)))))))
        (eq-hash-code s))))

(define (struct->vector s)
  (if (record? s)
      (let ([rtd (record-rtd s)])
        ;; Create that vector that has '... for opaque ranges and each field
        ;; value otherwise
        (let-values ([(vec-len rec-len)
                      ;; First, get the vector and record sizes
                      (let loop ([vec-len 1] [rec-len 0] [rtd rtd] [dots-already? #f])
                        (cond
                         [(not rtd) (values vec-len rec-len)]
                         [else
                          (let ([len (vector-length (record-type-field-names rtd))])
                            (cond
                             [(struct-type-immediate-transparent? rtd)
                              ;; A transparent region
                              (loop (+ vec-len len) (+ rec-len len) (record-type-parent rtd) #f)]
                             [dots-already?
                              ;; An opaque region that follows an opaque region
                              (loop vec-len (+ rec-len len) (record-type-parent rtd) #t)]
                             [else
                              ;; The start of opaque regions
                              (loop (add1 vec-len) (+ rec-len len) (record-type-parent rtd) #t)]))]))])
          ;; Walk though the record's types again, this time filling in the vector
          (let ([vec (make-vector vec-len '...)])
            (vector-set! vec 0 (string->symbol (format "struct:~a" (record-type-name rtd))))
            (let loop ([vec-pos vec-len] [rec-pos rec-len] [rtd rtd] [dots-already? #f])
              (when rtd
                (let* ([len (vector-length (record-type-field-names rtd))]
                       [rec-pos (- rec-pos len)])
                  (cond
                   [(struct-type-immediate-transparent? rtd)
                    ;; Copy over a transparent region
                    (let ([vec-pos (- vec-pos len)])
                      (let floop ([n 0])
                        (cond
                         [(= n len) (loop vec-pos rec-pos (record-type-parent rtd) #f)]
                         [else
                          (vector-set! vec (+ vec-pos n) (unsafe-struct-ref s (+ rec-pos n)))
                          (floop (add1 n))])))]
                   [dots-already?
                    ;; Skip another opaque region
                    (loop vec-pos rec-pos (record-type-parent rtd) #t)]
                   [else
                    ;; The vector already has '...
                    (loop (sub1 vec-pos) rec-pos (record-type-parent rtd) #t)]))))
            vec)))
      ;; Any value that is not implemented as a record is treated as
      ;; a fully opaque struct
      (vector (string->symbol (format "struct:~a" ((inspect/object s) 'type))) '...)))

;; ----------------------------------------

(define (make-fields field-count)
  (list->vector
   (let loop ([i 0])
     (if (= i field-count)
         '()
         (cons `(mutable ,(string->symbol (format "f~a" i)))
               (loop (fx1+ i)))))))

;; ----------------------------------------
;; Convenience for core:

(define-syntax struct
  (lambda (stx)
    (syntax-case stx  ()
      [(_ name (field ...))
       #'(struct name #f (field ...))]
      [(_ name parent (field ...))
       (let ([make-id (lambda (id fmt . args)
                        (datum->syntax id
                                       (string->symbol (chez:apply format fmt args))))])
         (with-syntax ([struct:name (make-id #'name "struct:~a" (syntax->datum #'name))]
                       [name? (make-id #'name "~a?" (syntax->datum #'name))]
                       [(name-field ...) (map (lambda (field)
                                                (make-id field "~a-~a" (syntax->datum #'name) (syntax->datum field)))
                                              #'(field ...))]
                       [(field-index ...) (let loop ([fields #'(field ...)] [accum '()] [pos 0])
                                            (cond
                                             [(null? fields) (reverse accum)]
                                             [else (loop (cdr fields) (cons pos accum) (add1 pos))]))]
                       [struct:parent (if (syntax->datum #'parent)
                                          (make-id #'parent "struct:~a" (syntax->datum #'parent))
                                          #f)])
           #'(begin
               (define struct:name (make-record-type-descriptor 'name struct:parent #f #f #f '#((immutable field) ...)))
               (define name? (record-predicate struct:name))
               (define name (record-constructor (make-record-constructor-descriptor struct:name #f #f)))
               (define name-field (record-accessor struct:name field-index))
               ...
               (define dummy
                 (begin
                   (record-type-equal-procedure struct:name default-struct-equal?)
                   (record-type-hash-procedure struct:name default-struct-hash)
                   (inspector-set! struct:name #f))))))])))

(define-syntax define-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ name . rest)
       (with-syntax ([make-name
                      (datum->syntax #'name
                                     (string->symbol (format "make-~a" (syntax->datum #'name))))])
         #'(begin
             (struct name . rest)
             (define make-name name)))])))

;; ----------------------------------------

(define-struct srcloc (source line column position span))
