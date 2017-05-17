(define-record struct-type-prop (name guard supers))

(define rtd-props (make-weak-eq-hashtable))
(define prefabs #f)

(define make-struct-type-property
  (case-lambda
    [(name) (make-struct-type-property name #f '() #f)]
    [(name guard) (make-struct-type-property name guard '() #f)]
    [(name guard supers) (make-struct-type-property name guard supers #f)]
    [(name guard supers can-inpersonate?)
     (let ([st (make-struct-type-prop name guard supers)])
       (values st
               (lambda (v)
                 (and (record? v)
                      (not (eq? none (struct-property-ref st (record-rtd v) none)))))
               (lambda (v)
                 (struct-property-ref st (record-rtd v) #f))))]))

(define (struct-property-ref prop rtd default)
  (getprop (record-type-uid rtd) prop default))

(define (struct-property-set! prop rtd val)
  (putprop (record-type-uid rtd) prop val))

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

(define-record position-based-accessor (rtd offset field-count))
(define-record position-based-mutator (rtd offset field-count))

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
     (unless (symbol? name)
       (raise-argument-error 'make-struct-type "symbol?" name))
     (unless (or (not parent-rtd)
                 (struct-type? parent-rtd))
       (raise-argument-error 'make-struct-type "(or/c #f struct-type?)" parent-rtd))
     (unless (exact-nonnegative-integer? fields-count)
       (raise-argument-error 'make-struct-type "exact-nonnegative-integer?" fields-count))
     (unless (exact-nonnegative-integer? auto-fields)
       (raise-argument-error 'make-struct-type "exact-nonnegative-integer?" auto-fields))
     (unless (zero? auto-fields)
       (error 'make-struct-type "auto fields not supported"))
     (when (eq? insp 'prefab)
       (unless (or (not parent-rtd)
                   (eq? (inspector-ref parent-rtd) 'prefab))
         (raise-arguments-error 'make-struct-type
                                "generative supertype disallowed for non-generative structure type"
                                "structure type name" name)))
     (let* ([fields (let loop ([fields fields-count])
                      (if (zero? fields)
                          '()
                          (cons (string->symbol (format "a~a" fields))
                                (loop (sub1 fields)))))]
            [prefab-name (and (eq? insp 'prefab)
                              (let* ([l (if parent-rtd
                                            (getprop (record-type-uid parent-rtd) 'prefab-name)
                                            null)]
                                     [l (if (= (length immutables) fields-count)
                                            l
                                            (cons (list->vector
                                                   (let loop ([i 0])
                                                     (cond
                                                      [(= i fields-count) null]
                                                      [(chez:member i immutables) (loop (add1 i))]
                                                      [else (cons i (loop (add1 i)))])))
                                                  l))]
                                     [l (if (zero? auto-fields)
                                            l
                                            (cons (list auto-fields l)
                                                  l))]
                                     [l (cons fields-count l)]
                                     [l (cons name l)])
                                l))]
            [rtd (cond
                  [(and prefab-name
                        (begin
                          (unless prefabs (set! prefabs (make-weak-hash)))
                          #t)
                        (hash-ref prefabs prefab-name #f))
                   => (lambda (rtd) rtd)]
                  [parent-rtd
                   (make-record-type parent-rtd (symbol->string name) fields)]
                  [else
                   (make-record-type (symbol->string name) fields)])]
            [parent-count (if parent-rtd
                              (struct-type-field-count parent-rtd)
                              0)])
       (struct-type-install-properties! rtd name fields-count auto-fields parent-rtd
                                        props insp proc-spec immutables guard constructor-name)
       (when prefab-name
         (putprop (record-type-uid rtd) 'prefab-name prefab-name)
         (hash-set! prefabs prefab-name rtd))
       (values rtd
               (record-constructor rtd)
               (lambda (v) (record? v rtd))
               (make-position-based-accessor rtd parent-count (+ fields-count auto-fields))
               (make-position-based-mutator rtd parent-count (+ fields-count auto-fields))))]))

(define struct-type-install-properties!
  (case-lambda
    [(rtd name fields auto-fields parent-rtd)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd '() (|#%app| current-inspector) #f '() #f name)]
    [(rtd name fields auto-fields parent-rtd props)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd props '(|#%app| current-inspector) #f '() #f name)]
    [(rtd name fields auto-fields parent-rtd props insp)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp #f name)]
    [(rtd name fields auto-fields parent-rtd props insp proc-spec)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec '() #f name)]
    [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec immutables #f name)]
    [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard)
     (struct-type-install-properties! rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard name)]
    [(rtd name fields auto-fields parent-rtd props insp proc-spec immutables guard constructor-name)
     (let ([parent-props
            (if parent-rtd
                (hashtable-ref rtd-props parent-rtd '())
                '())])
       (when (not parent-rtd)
         (record-type-equal-procedure rtd default-struct-equal?)
         (record-type-hash-procedure rtd default-struct-hash))
       ;; Record properties implemented by this type:
       (hashtable-set! rtd-props rtd (let ([props (append (map car props) parent-props)])
                                       (if proc-spec
                                           (cons prop:procedure props)
                                           props)))
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
                                                 (if (integer? proc-spec)
                                                     (cons proc-spec immutables)
                                                     immutables)
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
       (inspector-set! rtd insp))]))

(define make-struct-field-accessor
  (case-lambda
    [(pba pos)
     (record-field-accessor (position-based-accessor-rtd pba)
                            (+ pos (position-based-accessor-offset pba)))]
    [(pba pos name)
     (make-struct-field-accessor pba pos)]))
(define make-struct-field-mutator
  (case-lambda
    [(pbm pos)
     (record-field-mutator (position-based-mutator-rtd pbm)
                           (+ pos (position-based-mutator-offset pbm)))]
    [(pbm pos name)
     (make-struct-field-mutator pbm pos)]))

(define struct-field-accessors (make-weak-eq-hashtable))
(define struct-field-mutators (make-weak-eq-hashtable))

(define (register-struct-field-accessor! p)
  (hashtable-set! struct-field-accessors p #t))

(define (register-struct-field-mutator! p)
  (hashtable-set! struct-field-mutators p #t))

(define (struct-accessor-procedure? v)
  (and (procedure? v)
       (hashtable-ref struct-field-accessors v #f)))

(define (struct-mutator-procedure? v)
  (and (procedure? v)
       (hashtable-ref struct-field-mutators v #f)))

(define struct? record?)
(define struct-type? record-type-descriptor?)
(define struct-type record-rtd)

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

(define (unsafe-struct-ref s i)
  (#3%vector-ref s i))
(define (unsafe-struct-set! s i v)
  (#3%vector-set! s i v))

(define-values (prop:equal+hash equal+hash? equal+hash-ref)
  (make-struct-type-property 'equal+hash
                             (lambda (val info)
                               (cons (gensym) val))))

(define-values (prop:authentic authentic? authentic-ref)
  (make-struct-type-property 'authentic))

(define (struct-type-transparent? rtd)
  (let ([insp (inspector-ref rtd)])
    (and (not (eq? insp none))
         (or (not insp)
             (eq? insp 'prefab)
             (inspector-superior? (|#%app| current-inspector) insp))
         (let ([p-rtd (record-type-parent rtd)])
           (or (not p-rtd)
               (struct-type-transparent? p-rtd))))))

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
                          (let ([insp (inspector-ref rtd)]
                                [len (vector-length (record-type-field-names rtd))])
                            (cond
                             [(or (not insp)
                                  (eq? insp 'prefab)
                                  (and (not (eq? insp none))
                                       (inspector-superior? (|#%app| current-inspector) insp)))
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
                (let* ([insp (inspector-ref rtd)]
                       [len (vector-length (record-type-field-names rtd))]
                       [rec-pos (- rec-pos len)])
                  (cond
                   [(or (not insp)
                        (eq? insp 'prefab)
                        (and (not (eq? insp none))
                             (inspector-superior? (|#%app| current-inspector) insp)))
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

(define (prefab-key? k)
  (or (symbol? k)
      (and (pair? k)
           (symbol? (car k))
           (let* ([k (cdr k)] ; skip name
                  [prev-k k]
                  ;; The initial field count can be omitted:
                  [k (if (and (pair? k)
                              (exact-nonnegative-integer? (car k)))
                         (cdr k)
                         k)]
                  [field-count (if (eq? prev-k k)
                                   #f
                                   (car prev-k))])
             (let loop ([field-count field-count] [k k]) ; `k` is after name and field count
               (or (null? k)
                   (and (pair? k)
                        (let* ([prev-k k]
                               [k (if (and (pair? (car k))
                                           (pair? (cdar k))
                                           (null? (cddar k))
                                           (exact-nonnegative-integer? (caar k)))
                                      ;; Has a (list <auto-count> <auto-val>) element
                                      (cdr k)
                                      ;; Doesn't have auto-value element:
                                      k)]
                               [auto-count (if (eq? prev-k k)
                                               0
                                               (caar prev-k))])
                          (or (null? k)
                              (and (pair? k)
                                   (let* ([k (if (and (pair? k)
                                                      (vector? (car k)))
                                                 ;; Make sure it's a vector of indices
                                                 ;; that are in range and distinct:
                                                 (let* ([vec (car k)]
                                                        [len (vector-length vec)])
                                                   (let loop ([i 0] [set 0])
                                                     (cond
                                                      [(= i len) (cdr k)]
                                                      [else
                                                       (let ([pos (vector-ref vec i)])
                                                         (and (exact-nonnegative-integer? pos)
                                                              (or (not field-count)
                                                                  (< pos (+ field-count auto-count)))
                                                              (not (bitwise-bit-set? set pos))
                                                              (loop (add1 i) (bitwise-ior set (bitwise-arithmetic-shift-left 1 pos)))))])))
                                                 k)])
                                     (or (null? k)
                                         (and (pair? k)
                                              ;; Supertype: make sure it's a pair with a
                                              ;; symbol and a field count, and loop:
                                              (symbol? (car k))
                                              (pair? (cdr k))
                                              (exact-nonnegative-integer? (cadr k))
                                              (loop (cadr k) (cddr k)))))))))))))))

(define (prefab-struct-key v) #f)
(define (prefab-key->struct-type key field-count) #f)
(define (make-prefab-struct key args) (error 'make-prefab-struct "not ready"))

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

(define-struct arity-at-least (value))
