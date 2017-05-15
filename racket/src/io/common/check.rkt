#lang racket/base
(require (for-syntax racket/base))

(provide check
         check-range
         check-immutable-field)

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ who pred #:contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who pred v)
     #`(check who pred #:contract #,(format "~a" (syntax->datum #'pred)) v)]))

(define (check-range who start-pos end-pos max-end in-value)
  (when (start-pos . > . max-end)
    (raise-range-error who
                       "byte string"
                       "starting "
                       start-pos
                       in-value
                       0
                       max-end
                       #f))
  (when (or (end-pos . < . start-pos)
            (end-pos . > . max-end))
    (raise-range-error who
                       "byte string"
                       "starting "
                       end-pos
                       in-value
                       0
                       max-end
                       start-pos)))

(define (check-immutable-field who v sti)
  (when (exact-integer? v)
    (unless (memv v (list-ref sti 5))
      (raise-arguments-error who "field index not declared immutable"
                             "field index" v))))
