#lang racket/base
(require "wrap-annotation.rkt"
         (only-in "match.rkt" define-match))

;; Like "match.rkt", but works with the host Scheme's annotations
;; instead of correlated objects

(provide match)

(define-match match
  ;; These are the annotation-based unwraps:
  unwrap unwrap-list
  wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
  wrap-eq? wrap-equal?
  in-wrap-list)
