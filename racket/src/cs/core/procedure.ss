(define-values (prop:method-arity-error method-arity-error? method-arity-error-ref)
  (make-struct-type-property 'method-arity-error))

(define-values (prop:procedure procedure-struct? procedure-struct-ref)
  (make-struct-type-property 'procedure (lambda (v info)
                                          (if (integer? v)
                                              (+ v (let ([p (list-ref info 6)])
                                                     (if p
                                                         (struct-type-field-count p)
                                                         0)))
                                              v))))

(define (procedure? v)
  (or (chez:procedure? v)
      (and (record? v)
           (struct-property-ref prop:procedure (record-rtd v) #f))))

(define apply
  (case-lambda
    [(proc args)
     (if (chez:procedure? proc)
         (chez:apply proc args)
         (chez:apply (extract-procedure proc) args))]
    [(proc . argss)
     (if (chez:procedure? proc)
         (chez:apply chez:apply proc argss)
         (chez:apply chez:apply (extract-procedure proc) argss))]))

(define-syntax |#%app|
  (syntax-rules ()
    [(_ rator rand ...)
     ((extract-procedure rator) rand ...)]))

(define (extract-procedure f)
  (cond
   [(chez:procedure? f) f]
   [else (or (try-extract-procedure f)
             (not-a-procedure f))]))

(define (try-extract-procedure f)
  (cond
   [(record? f)
    (let* ([v (struct-property-ref prop:procedure (record-rtd f) #f)])
      (cond
       [(procedure? v) (case-lambda
                         [(a) (v f a)]
                         [(a b) (v f a b)]
                         [(a b c) (v f a b c)]
                         [args (apply v f args)])]
       [(fixnum? v)
        (let ([v (unsafe-struct-ref f v)])
          (if (chez:procedure? v)
              v
              (try-extract-procedure v)))]
       [else #f]))]
   [else #f]))

(define (procedure-arity-includes? orig-f orig-n)
  (cond
   [(chez:procedure? orig-f)
    (unless (exact-nonnegative-integer? orig-n)
      (raise-argument-error 'procedure-arity-includes? "exact-nonnegative-integer?" orig-n))
    (bitwise-bit-set? (procedure-arity-mask orig-f) orig-n)]
   [else
    (let arity-includes? ([f orig-f] [n orig-n])
      (cond
       [(chez:procedure? f)
        (cond
         [(exact-nonnegative-integer? n)
          (bitwise-bit-set? (procedure-arity-mask f) n)]
         [(exact-nonnegative-integer? orig-n)
          #f]
         [else
          (raise-argument-error 'procedure-arity-includes? "exact-nonnegative-integer?" orig-n)])]
       [(record? f)
        (let* ([v (struct-property-ref prop:procedure (record-rtd f) #f)])
          (cond
           [(fixnum? v)
            (arity-includes? (unsafe-struct-ref f v) n)]
           [else
            (arity-includes? v (if (exact-integer? n)
                                   (add1 n)
                                   n))]))]
       [else
        (raise-argument-error 'procedure-arity-includes? "procedure?" orig-f)]))]))

(define (procedure-arity orig-f)
  (cond
   [(chez:procedure? orig-f)
    (mask->arity (procedure-arity-mask orig-f))]
   [else
    (let proc-arity ([f orig-f] [shift 0])
      (cond
       [(chez:procedure? f)
        (mask->arity (bitwise-arithmetic-shift-right (procedure-arity-mask f) shift))]
       [(record? f)
        (let* ([rtd (record-rtd f)]
               [v (struct-property-ref prop:procedure rtd #f)])
          (cond
           [(fixnum? v)
            (proc-arity (unsafe-struct-ref f v) shift)]
           [else
            (proc-arity v (add1 shift))]))]
       [else
        (raise-argument-error 'procedure-arity "procedure?" orig-f)]))]))

(define (mask->arity mask)
  (let loop ([mask mask] [pos 0])
    (cond
     [(= mask 0) null]
     [(= mask -1) (arity-at-least pos)]
     [(bitwise-bit-set? mask 0)
      (let ([rest (loop (bitwise-arithmetic-shift-right mask 1) (add1 pos))])
        (cond
         [(null? rest) pos]
         [(pair? rest) (cons pos rest)]
         [else (list pos rest)]))]
     [else
      (loop (bitwise-arithmetic-shift-right mask 1) (add1 pos))])))

;; Public, limited variant:
(define (procedure-extract-target f)
  (cond
   [(record? f)
    (let* ([rtd (record-rtd f)]
           [v (struct-property-ref prop:procedure rtd #f)])
      (cond
       [(fixnum? v)
        (let ([v (unsafe-struct-ref f v)])
          (and (chez:procedure? v) v))]
       [else #f]))]
   [else #f]))

(define (not-a-procedure f)
  (error 'apply (format "not a procedure: ~s" f)))

;; ----------------------------------------

(define (set-primitive-applicables!)
  (struct-property-set! prop:procedure
                        (record-type-descriptor position-based-accessor)
                        (lambda (pba s p)
                          (if (and (record? s (position-based-accessor-rtd pba))
                                   (< p (position-based-accessor-field-count pba)))
                              (unsafe-struct-ref s (+ p (position-based-accessor-offset pba)))
                              (error 'struct-ref "bad access"))))

  (struct-property-set! prop:procedure
                        (record-type-descriptor position-based-mutator)
                        (lambda (pbm s p v)
                          (if (and (record? s (position-based-mutator-rtd pbm))
                                   (< p (position-based-mutator-field-count pbm)))
                              (unsafe-struct-set! s (+ p (position-based-mutator-offset pbm)) v)
                              (error 'struct-set! "bad assignment")))))
