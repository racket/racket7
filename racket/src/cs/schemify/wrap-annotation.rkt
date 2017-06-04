#lang racket/base
(require '#%linklet
         (only-in "wrap.rkt" define-wrap))

;; Like "wrap.rkt", but uses the host Scheme's annotations
;; instead of correlated objects

(provide unwrap unwrap-list
         wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
         wrap-eq? wrap-equal?
         in-wrap-list)

(define annotation-table (primitive-table '#%annotation))
(define kernel-table (primitive-table '#%kernel))

(define annotation?
  (or (and annotation-table
           (hash-ref annotation-table 'annotation?))
      (and kernel-table
           (hash-ref kernel-table 'syntax?))
      (lambda (x) #f)))

(define annotation-expression
  (or (and annotation-table
           (hash-ref annotation-table 'annotation-expression))
      (and kernel-table
           (hash-ref kernel-table 'syntax-e))
      (lambda (x) x)))


(define-wrap (unwrap unwrap-list
                     wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
                     wrap-eq? wrap-equal?
                     in-wrap-list)
  annotation? annotation-expression)
