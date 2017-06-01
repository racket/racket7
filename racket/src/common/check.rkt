#lang racket/base
(require racket/stxparam
         (for-syntax racket/base))

(provide check

         procedure-arity-includes/c
         
         define/who
         who)

;; ----------------------------------------

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ who pred #:contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who pred #:or-false v)
     #`(check who (lambda (x) (or (not x) (pred x))) #:contract #,(format "(or/c ~a #f)" (syntax->datum #'pred)) v)]
    [(_ who #:or-false pred v)
     #`(check who (lambda (x) (or (not x) (pred x))) #:contract #,(format "(or/c #f ~a)" (syntax->datum #'pred)) v)]
    [(_ who #:test expr #:contract ctc v)
     #`(unless expr
         (raise-argument-error who ctc v))]
    [(_ who pred v)
     #`(check who pred #:contract #,(format "~a" (syntax->datum #'pred)) v)]))

;; ----------------------------------------

(define-syntax (procedure-arity-includes/c stx)
  (syntax-case stx ()
    [(_ n)
     (exact-nonnegative-integer? (syntax-e #'n))
     #'(lambda (p)
         (and (procedure? p)
              (procedure-arity-includes? p n)))]))

;; ----------------------------------------

(define-syntax-parameter who
  (lambda (stx)
    (raise-syntax-error #f "not defined" stx)))

(define-for-syntax (make-who id)
  (lambda (stx)
    (syntax-case stx ()
      [(who . _)
       (raise-syntax-error #f "cannot apply" #'who stx)]
      [else #`'#,id])))

(define-syntax (define/who stx)
  (syntax-case stx ()
    [(_ (id . args) . body)
     (syntax/loc stx
       (define id
         (lambda args
           (syntax-parameterize ([who (make-who 'id)])
             . body))))]
    [(_ id rhs)
     (syntax/loc stx
       (define id
         (syntax-parameterize ([who (make-who 'id)])
           rhs)))]))
