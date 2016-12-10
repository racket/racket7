#lang racket/base
(require "correlate-syntax.rkt"
         "../syntax/datum-map.rkt"
         "../common/make-match.rkt")

;; A "correlated" is the host's notion of syntax objects for
;; `compile-linklet`, which is an S-expression with source locations
;; and properties (but no scopes).

(provide correlate
         correlated?
         datum->correlated
         correlated-e
         correlated-cadr
         correlated-length
         correlated->list
         correlated->datum
         correlated-property
         correlated-property-symbol-keys
         define-correlated-match

         correlated-source
         correlated-line
         correlated-column
         correlated-position
         correlated-span)

(define (correlate src-e s-exp)
  (define e (datum->correlated s-exp src-e))
  (define maybe-n (syntax-property src-e 'inferred-name))
  (if maybe-n
      (syntax-property e 'inferred-name maybe-n)
      e))

(define (correlated? e)
  (syntax? e))

(define (datum->correlated d [srcloc #f])
  (datum->syntax #f d srcloc))

(define (correlated-e e)
  (if (syntax? e)
      (syntax-e e)
      e))

(define (correlated-cadr e)
  (car (correlated-e (cdr (correlated-e e)))))

(define (correlated-length e)
  (define l (correlated-e e))
  (and (list? l)
       (length l)))

(define (correlated->list e)
  (let loop ([e e])
    (cond
     [(list? e) e]
     [(pair? e) (cons (car e) (loop (cdr e)))]
     [(null? e) null]
     [(syntax? e) (loop (syntax-e e))]
     [else (error 'correlate->list "not a list")])))

(define (correlated->datum e)
  (datum-map e (lambda (tail? d)
                 (if (syntax? d)
                     (syntax->datum d)
                     d))))

(define (correlated-property-symbol-keys e)
  (syntax-property-symbol-keys e))

(define correlated-property
  (case-lambda
    [(e k) (syntax-property e k)]
    [(e k v) (syntax-property e k v)]))

(define-define-match define-correlated-match
  syntax? syntax-e (lambda (false str e) (error str)))

(define (correlated-source s) (syntax-source s))
(define (correlated-line s) (syntax-line s))
(define (correlated-column s) (syntax-column s))
(define (correlated-position s) (syntax-position s))
(define (correlated-span s) (syntax-span s))
