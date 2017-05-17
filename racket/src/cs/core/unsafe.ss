(define unsafe-car #3%car)
(define unsafe-cdr #3%cdr)
(define unsafe-list-tail #3%list-tail)
(define unsafe-list-ref #3%list-ref)

(define unsafe-fx+ #3%fx+)
(define unsafe-fx- #3%fx-)
(define unsafe-fx* #3%fx*)
(define unsafe-fxquotient #3%fxquotient)
(define unsafe-fxremainder #3%fxremainder)
(define unsafe-fxmodulo #3%fxmodulo)
(define unsafe-fxabs #3%fxabs)
(define unsafe-fxand #3%fxand)
(define unsafe-fxior #3%fxior)
(define unsafe-fxxor #3%fxxor)
(define unsafe-fxnot #3%fxnot)
(define unsafe-fxrshift #3%fxarithmetic-shift-right)
(define unsafe-fxlshift #3%fxarithmetic-shift-left)

(define unsafe-fx= #3%fx=)
(define unsafe-fx< #3%fx<)
(define unsafe-fx> #3%fx>)
(define unsafe-fx>= #3%fx>=)
(define unsafe-fx<= #3%fx<=)
(define unsafe-fxmin #3%fxmin)
(define unsafe-fxmax #3%fxmax)

(define unsafe-fl+ #3%fl+)
(define unsafe-fl- #3%fl-)
(define unsafe-fl* #3%fl*)
(define unsafe-fl/ #3%fl/)
(define unsafe-flabs #3%flabs)

(define unsafe-fl= #3%fl=)
(define unsafe-fl< #3%fl<)
(define unsafe-fl> #3%fl>)
(define unsafe-fl>= #3%fl>=)
(define unsafe-fl<= #3%fl<=)
(define unsafe-flmin #3%flmin)
(define unsafe-flmax #3%flmax)

(define unsafe-flround #3%flround)
(define unsafe-flfloor #3%flfloor)
(define unsafe-flceiling #3%flceiling)
(define unsafe-fltruncate #3%fltruncate)

(define unsafe-flsin #3%flsin)
(define unsafe-flcos #3%flcos)
(define unsafe-fltan #3%fltan)
(define unsafe-flasin #3%flasin)
(define unsafe-flacos #3%flacos)
(define unsafe-flatan #3%flatan)
(define unsafe-fllog #3%fllog)
(define unsafe-flexp #3%flexp)
(define unsafe-flsqrt #3%flsqrt)
(define unsafe-flexpt #3%flexpt)

(define unsafe-vector*-length #3%vector-length)
(define unsafe-vector*-ref #3%vector-ref)
(define unsafe-vector*-set! #3%vector-set!)

(define unsafe-unbox* #3%unbox)
(define unsafe-set-box*! #3%set-box!)

(define unsafe-bytes-length #3%bytevector-length)
(define unsafe-bytes-ref #3%bytevector-u8-ref)
(define unsafe-bytes-set! #3%bytevector-u8-set!)

(define unsafe-string-length #3%string-length)
(define unsafe-string-ref #3%string-ref)
(define unsafe-string-set! #3%string-set!)

(define unsafe-fxvector-length #3%fxvector-length)
(define unsafe-fxvector-ref #3%fxvector-ref)
(define unsafe-fxvector-set! #3%fxvector-set!)

(define unsafe-undefined (let ([p (make-record-type "undefined" '())])
                           ((record-constructor p))))

(define (check-not-unsafe-undefined v sym)
  (when (eq? v unsafe-undefined)
    (raise-arguments-error sym "undefined;\n cannot use before initialization")))

(define (check-not-unsafe-undefined/assign v sym)
  (when (eq? v unsafe-undefined)
    (raise-arguments-error sym "assignment disallowed;\n cannot assign before initialization")))

(define-syntax (define-extfl-ids stx)
  (syntax-case stx ()
    [(_ ids ...)
     (with-syntax ([(extflids ...) (map (lambda (i)
                                          (datum->syntax i (string->symbol (format "extfl~s" (syntax->datum i)))))
                                        (syntax->list #'(ids ...)))])
       #'(begin
           (define (extflids v)
             (error 'extflids "extflonums are unsupported"))
           ...))]))

(define-extfl-ids
  sin cos tan
  asin acos atan
  truncate round floor ceiling
  exp log expt)
