
(define (extflonum-available?) #f)
(define (extflonum? v) #f)
(define (extflvector? v) #f)

(define-syntax (define-extfl-ids stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(begin
         (define (id v)
           (error 'id "extflonums are unsupported"))
         ...)]))

(define-extfl-ids
  extfl*
  extfl+
  extfl-
  ->extfl
  extfl->exact
  extfl->exact-integer
  extfl->floating-point-bytes
  extfl->fx
  extfl->inexact
  extfl/
  extfl<
  extfl<=
  extfl=
  extfl>
  extfl>=
  extflabs
  extflacos
  extflasin
  extflatan
  extflceiling
  extflcos
  extflexp
  extflexpt
  floating-point-bytes->extfl
  extflfloor
  fx->extfl
  extfllog
  make-shared-extflvector
  make-extflvector
  extflmax
  extflmin
  real->extfl
  extflround
  shared-extflvector
  extflsin
  extflsqrt
  extfltan
  extfltruncate
  extflvector
  extflvector-length
  extflvector-ref
  extflvector-set!)
