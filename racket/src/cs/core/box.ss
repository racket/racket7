(define (box-cas! b v1 v2)
  (unless (mutable-box? b)
    (raise-argument-error 'box-cas! "(and/c box? (not/c immutable?))" b))
  (unsafe-box-cas! b v1 v2))

(define (unsafe-box-cas! b v1 v2)
  (and (eq? v1 (#3%unbox b))
       (#3%set-box! b v2)
       #t))

(define (make-weak-box v) (weak-cons v #t))

(define (weak-box? v) (weak-pair? v))

(define (weak-box-value v)
  (unless (weak-pair? v)
    (raise-argument-error 'weak-box-value "weak-box?" v))
  (let ([c (car v)])
    (if (eq? c #!bwp)
        #f
        c)))
