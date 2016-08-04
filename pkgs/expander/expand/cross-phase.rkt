#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "../boot/runtime-primitive.rkt")

;; Check whether a module fits the restricted grammar of a cross-phase
;; persistent module

(provide check-cross-phase-persistent-form)

(define (check-cross-phase-persistent-form bodys)
  (check-body bodys))

(define (check-body bodys)
  (for ([body (in-list bodys)])
    (case (core-form-sym body 0)
      [(begin)
       (define-match m body '(begin e ...))
       (check-body (m 'e))]
      [(#%declare #%provide #%require module module*)
       (void)]
      [(define-values)
       (define-match m body '(define-values (id ...) rhs))
       (check-expr (m 'rhs) (length (m 'id)) body)]
      [else
       (disallow body)])))

(define (check-expr e num-results enclosing)
  (case (core-form-sym e 0)
    [(lambda case-lambda)
     (check-count 1 num-results enclosing)]
    [(quote)
     (define-match m e '(quote datum))
     (check-datum (m 'datum))
     (check-count 1 num-results enclosing)]
    [(#%app)
     (define-match m e '(#%app rator rand ...))
     (define rands (m 'rand))
     (for ([rand (in-list rands)])
       (check-expr rand 1 e))
     (case (cross-phase-primitive-name (m 'rator))
       [(cons list)
        (check-count 1 num-results enclosing)]
       [(make-struct-type)
        (check-count 5 num-results enclosing)]
       [(make-struct-type-property)
        (check-count 3 num-results enclosing)]
       [(gensym)
        (unless (or (= 0 (length rands))
                    (and (= 1 (length rands))
                         (quoted-string? (car rands))))
          (disallow e))]
       [(string->uninterned-symbol)
        (unless (and (= 1 (length rands))
                     (quoted-string? (car rands)))
          (disallow e))]
       [else (disallow e)])]))

(define (check-count is-num expected-num enclosing)
  (unless (= is-num expected-num)
    (disallow enclosing)))

(define (check-datum datum)
  (define d (syntax-e datum))
  (cond
   [(or (number? d) (boolean? d) (symbol? d) (string? d) (bytes? d))
    (void)]
   [else (disallow datum)]))

(define (quoted-string? e)
  (and (eq? 'quote (core-form-sym e 0))
       (let ()
         (define-match m e '(quote datum))
         (string? (syntax-e (m 'datum))))))

(define (cross-phase-primitive-name id)
  (define b (resolve+shift id 0))
  (and (module-binding? b)
       (eq? runtime-module-name (module-path-index-resolve (module-binding-module b)))
       (module-binding-sym b)))

(define (disallow body)
  (error "not allowed in a cross-phase persistent module:" body))
