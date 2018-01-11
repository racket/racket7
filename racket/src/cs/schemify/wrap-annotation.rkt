#lang racket/base
(require racket/private/primitive-table
         (only-in "wrap.rkt" define-wrap))

;; Like "wrap.rkt", but uses the host Scheme's annotations
;; instead of correlated objects

(provide unwrap unwrap-list
         wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
         wrap-eq? wrap-equal?
         in-wrap-list)

(import-from-primitive-table
 (#%annotation #%kernel)
 [syntax? annotation?]
 [syntax-e annotation-expression])

(define-wrap (unwrap unwrap-list
                     wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
                     wrap-eq? wrap-equal?
                     in-wrap-list)
  annotation? annotation-expression)
