
(define (vector-immutable . args)
  (let ([vec (apply vector args)])
    (#%$vector-set-immutable! vec)
    vec))

;; ----------------------------------------

(define (vector? v)
  (or (#%vector? v)
      (and (impersonator? v)
           (#%vector? (impersonator-val v)))))

(define (mutable-vector? v)
  (or (#%mutable-vector? v)
      (and (impersonator? v)
           (#%mutable-vector? (impersonator-val v)))))

;; ----------------------------------------

(define-record vector-chaperone chaperone (ref set))
(define-record vector-impersonator impersonator (ref set))

(define (chaperone-vector vec ref set . props)
  (unless (vector? vec)
    (raise-argument-error 'chaperone-vector "vector?" vec))
  (do-impersonate-vector 'chaperone-vector make-vector-chaperone vec ref set
                         make-props-chaperone props))

(define (impersonate-vector vec ref set . props)
  (unless (mutable-vector? vec)
    (raise-argument-error 'impersonate-vector "(and/c vector? (not/c immutable?))" vec))
  (do-impersonate-vector 'impersonate-vector make-vector-impersonator vec ref set
                         make-props-impersonator props))

(define (do-impersonate-vector who make-vector-impersonator vec ref set
                               make-props-impersonator props)
  (unless (or (not ref)
              (and (procedure? ref)
                   (procedure-arity-includes? ref 3)))
    (raise-argument-error who "(or/c (procedure-arity-includes/c 3) #f)" ref))
  (unless (or (not set)
              (and (procedure? set)
                   (procedure-arity-includes? set 3)))
    (raise-argument-error who "(or/c (procedure-arity-includes/c 3) #f)" set))
  (let ([val (if (impersonator? vec)
                 (impersonator-val vec)
                 vec)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? vec)
                                                (impersonator-props vec)
                                                empty-hasheq))])
    (if (or ref set)
        (make-vector-impersonator val vec props ref set)
        (make-props-impersonator val vec props))))

(define (set-vector-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor vector-chaperone)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor vector-impersonator)
                              (lambda (i hash-code)
                                (hash-code (vector-copy i)))))

;; ----------------------------------------

(define-record vector*-chaperone vector-chaperone ())
(define-record vector*-impersonator vector-impersonator ())

(define (chaperone-vector* vec ref set . props)
  (unless (vector? vec)
    (raise-argument-error 'chaperone-vector* "vector?" vec))
  (do-impersonate-vector* 'chaperone-vector* make-vector*-chaperone vec ref set
                          make-props-chaperone props))

(define (impersonate-vector* vec ref set . props)
  (unless (mutable-vector? vec)
    (raise-argument-error 'impersonate-vector* "(and/c vector? (not/c immutable?))" vec))
  (do-impersonate-vector 'impersonate-vector* make-vector*-impersonator vec ref set
                         make-props-impersonator props))

(define (do-impersonate-vector* who make-vector*-impersonator vec ref set
                                make-props-impersonator props)
  (unless (or (not ref)
              (and (procedure? ref)
                   (procedure-arity-includes? ref 4)))
    (raise-argument-error who "(or/c (procedure-arity-includes/c 4) #f)" ref))
  (unless (or (not set)
              (and (procedure? set)
                   (procedure-arity-includes? set 4)))
    (raise-argument-error who "(or/c (procedure-arity-includes/c 4) #f)" set))
  (let ([val (if (impersonator? vec)
                 (impersonator-val vec)
                 vec)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? vec)
                                                (impersonator-props vec)
                                                empty-hasheq))])
    (if (or ref set)
        (make-vector*-impersonator val vec props ref set)
        (make-props-impersonator val vec props))))

;; ----------------------------------------

(define (vector-length vec)
  (if (#%vector? vec)
      (#3%vector-length vec)
      (pariah (impersonate-vector-length vec))))

(define (unsafe-vector-length vec)
  (vector-length vec))

(define (impersonate-vector-length vec)
  (if (and (impersonator? vec)
           (#%vector? (impersonator-val vec)))
      (#%vector-length (impersonator-val vec))
      ;; Let primitive report the error:
      (#2%vector-length vec)))

;; ----------------------------------------

(define (vector-ref vec idx)
  (if (#%vector? vec)
      (#%vector-ref vec idx)
      (pariah (impersonate-vector-ref vec idx))))

(define (unsafe-vector-ref vec idx)
  (if (#%vector? vec)
      (#3%vector-ref vec idx)
      (pariah (impersonate-vector-ref vec idx))))

(define (impersonate-vector-ref orig idx)
  (if (and (impersonator? orig)
           (#%vector? (impersonator-val orig)))
      (let loop ([o orig])
        (cond
         [(#%vector? o) (#2%vector-ref o idx)]
         [(vector-chaperone? o)
          (let* ([val (loop (impersonator-next o))]
                 [new-val (if (vector*-chaperone? o)
                              ((vector-chaperone-ref o) orig o idx val)
                              ((vector-chaperone-ref o) o idx val))])
            (unless (chaperone-of? new-val val)
              (raise-arguments-error 'vector-ref
                                     "chaperone produced a result that is not a chaperone of the original result"
                                     "chaperone result" new-val
                                     "original result" val))
            new-val)]
         [(vector-impersonator? o)
          (let ([val  (loop (impersonator-next o))])
            (if (vector*-impersonator? o)
                ((vector-impersonator-ref o) orig o idx val)
                ((vector-impersonator-ref o) o idx val)))]
         [else (loop (impersonator-next o))]))
      ;; Let primitive report the error:
      (#2%vector-ref orig idx)))

;; ----------------------------------------

(define (vector-set! vec idx val)
  (if (#%vector? vec)
      (#%vector-set! vec idx val)
      (pariah (impersonate-vector-set! vec idx val))))

(define (unsafe-vector-set! vec idx val)
  (if (#%vector? vec)
      (#3%vector-set! vec idx val)
      (pariah (impersonate-vector-set! vec idx val))))

(define (impersonate-vector-set! orig idx val)
  (cond
   [(not (and (impersonator? orig)
              (mutable-vector? (impersonator-val orig))))
    ;; Let primitive report the error:
    (#2%vector-set! orig idx val)]
   [(or (not (exact-nonnegative-integer? idx))
        (>= idx (vector-length (impersonator-val orig))))
    ;; Let primitive report the index error:
    (#2%vector-set! (impersonator-val orig) idx val)]
   [else
    (let loop ([o orig] [val val])
      (cond
       [(#%vector? o) (#2%vector-set! o idx val)]
       [else
        (let ([next (impersonator-next o)])
          (cond
           [(vector-chaperone? o)
            (let ([new-val (if (vector*-chaperone? o)
                               ((vector-chaperone-set o) orig o idx val)
                               ((vector-chaperone-set o) o idx val))])
              (unless (chaperone-of? new-val val)
                (raise-arguments-error 'vector-set!
                                       "chaperone produced a result that is not a chaperone of the original result"
                                       "chaperone result" new-val
                                       "original result" val))
              (loop next val))]
           [(vector-impersonator? o)
            (loop next
                  (if (vector*-impersonator? o)
                      ((vector-impersonator-set o) orig o idx val)
                      ((vector-impersonator-set o) o idx val)))]
           [else (loop next val)]))]))]))

;; ----------------------------------------

(define (vector-copy vec)
  (cond
   [(#%vector? vec)
    (#3%vector-copy vec)]
   [(vector? vec)
    (let* ([len (vector-length vec)]
           [vec2 (make-vector len)])
      (vector-copy! vec2 0 vec)
      vec2)]
   [else
    ;; Let primitive complain:
    (#2%vector-copy vec)]))

(define vector-copy!
  (case-lambda
   [(dest dest-start src)
    (vector-copy! dest dest-start src 0
                  (if (vector? src) (vector-length src) 0))]
   [(src src-start dest dest-start)
    (vector-copy! dest dest-start src src-start
                  (if (vector? src) (vector-length src) 0))]
   [(dest dest-start src src-start src-end)
    (unless (mutable-vector? dest)
      (raise-argument-error 'vector-copy! "(and/c vector? (not/c immutable?))" dest))
    (let loop ([i (- src-end src-start)])
      (unless (zero? i)
        (let ([i (sub1 i)])
          (vector-set! dest (+ dest-start i) (vector-ref src (+ src-start i)))
          (loop i))))]))

(define vector->values
  (case-lambda
    [(vec)
     (let ([len (vector-length vec)])
       (cond
        [(fx= len 0) (values)]
        [(fx= len 1) (vector-ref vec 0)]
        [(fx= len 2) (values (vector-ref vec 0) (vector-ref vec 1))]
        [(fx= len 3) (values (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2))]
        [else (chez:apply values (vector->list vec))]))]
    [(vec start)
     (vector->values vec start (vector-length vec))]
    [(vec start end)
     ;; FIXME: check range
     (chez:apply values
                 (let loop ([start start])
                   (cond
                    [(fx= start end) null]
                    [else (cons (vector-ref vec start)
                                (loop (fx1+ start)))])))]))

(define (vector-fill! vec v)
  (cond
   [(#%vector? vec)
    (#3%vector-fill! vec v)]
   [(vector? vec)
    (unless (mutable-vector? v)
      (raise-argument-error 'vector-fill! "(and/c vector? (not immutable?))" v))
    (let ([len (vector-length vec)])
      (let loop ([i 0])
        (unless (= i len)
          (vector-set! vec i v)
          (loop (fx1+ i)))))]
   [else
    ;; Let primitive complain:
    (#2%vector-fill! vec v)]))

(define (vector->immutable-vector v)
  (cond
   [(#%vector? v)
    (#3%vector->immutable-vector v)]
   [(vector? v)
    (if (mutable-vector? v)
        (#3%vector->immutable-vector
         (vector-copy v))
        v)]
   [else
    (#2%vector->immutable-vector v)]))
