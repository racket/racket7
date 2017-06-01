(define-record mpair (car cdr))

(define (mcons a b)
  (make-mpair a b))

(define/who (mcar m)
  (check who mpair? m)
  (mpair-car m))

(define/who (mcdr m)
  (check who mpair? m)
  (mpair-cdr m))

(define/who (set-mcar! m v)
  (check who mpair? m)
  (set-mpair-car! m v))

(define/who (set-mcdr! m v)
  (check who mpair? m)
  (set-mpair-cdr! m v))
