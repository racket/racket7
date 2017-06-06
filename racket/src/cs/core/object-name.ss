
(define-values (prop:object-name object-name? object-name-ref)
  (make-struct-type-property 'object-name
                             (lambda (v info)
                               (cond
                                [(exact-nonnegative-integer? v)
                                 (unless (< v (list-ref info 1))
                                   (raise-arguments-error 'guard-for-prop:object-name
                                                          "field index >= initialized-field count for structure type"
                                                          "field index" v
                                                          "initialized-field count" (list-ref info 1)))
                                 (+ v (let ([p (list-ref info 6)])
                                        (if p
                                            (struct-type-field-count p)
                                            0)))]
                                [(and (procedure? v)
                                      (procedure-arity-includes? v 1))
                                 v]
                                [else
                                 (raise-argument-error 'guard-for-prop:object-name
                                                       "(or/c exact-nonnegative-integer? (procedure-arity-includes/c 1))"
                                                       v)]))))

(define (object-name v)
  (cond
   [(object-name? v)
    (let ([n (object-name-ref v)])
      (cond
       [(exact-integer? n)
        (unsafe-struct-ref v n)]
       [else
        (n v)]))]
   [(#%procedure? v)
    (let ([name (((inspect/object v) 'code) 'name)])
      (and name
           (string->symbol name)))]
   [(impersonator? v)
    (object-name (impersonator-val v))]
   [(procedure? v)
    (object-name (extract-procedure v))]
   [else #f]))
