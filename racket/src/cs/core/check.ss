
(define-syntax (who stx)
  (syntax-error stx "not bound"))

(define-syntax (define/who stx)
  (syntax-case stx ()
    [(define/who (id . args) body ...)
     #'(define id
         (fluid-let-syntax ([who (lambda (stx)
                                   #''id)])
           (lambda args body ...)))]
    [(define/who id rhs)
     #'(define id
         (fluid-let-syntax ([who (lambda (stx)
                                   #''id)])
           rhs))]))

(define-syntax (check stx)
  (syntax-case stx (:test :contract :or-false)
    [(_ who pred :contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who :test test-expr :contract ctc v)
     #`(unless test-expr
         (raise-argument-error who ctc v))]
    [(_ who :or-false pred v)
     #`(unless (or (not v) (pred v))
         (raise-argument-error who #,(format "(or/c #f ~a)" (syntax->datum #'pred)) v))]
    [(_ who pred :or-false v)
     #`(unless (or (not v) (pred v))
         (raise-argument-error who #,(format "(or/c ~a #f)" (syntax->datum #'pred)) v))]
    [(_ who pred v)
     #`(check who pred :contract #,(format "~a" (syntax->datum #'pred)) v)]))

(define-syntax (procedure-arity-includes/c stx)
  (syntax-case stx ()
    [(_ n)
     (let ([n (syntax->datum #'n)])
       (and (integer? n)
            (exact? n)
            (not (negative? n))))
     #'(lambda (p)
         (and (procedure? p)
              (procedure-arity-includes? p n)))]))
