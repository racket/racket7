
(define-record impersonator (val next props))
(define-record chaperone impersonator ())

(define (impersonate-ref s? acc i)
  (if (impersonator? i)
      (acc (impersonator-val i))
      (acc i)))

(define (impersonate-set! s? mut i v)
  (if (impersonator? i)
      (let ([s (impersonator-val i)])
        (if (s? s)
            (mut s v)
            (mut s v)))
      (mut i v)))

;; ----------------------------------------

(define-record-type (impersonator-property create-impersonator-property impersonator-property?)
  (fields name))

(define-record impersonator-property-accessor-procedure (proc name))

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
