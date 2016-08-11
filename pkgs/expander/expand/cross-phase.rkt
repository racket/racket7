#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "../boot/runtime-primitive.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt")

;; Check whether a module fits the restricted grammar of a cross-phase
;; persistent module

(provide check-cross-phase-persistent-form)

(define (check-cross-phase-persistent-form bodys)
  (check-body bodys))

(define (check-body bodys)
  (for ([body (in-list bodys)])
    (define p (if (expanded+parsed? body)
                  (expanded+parsed-parsed body)
                  body))
    (cond
     [(semi-parsed-begin-for-syntax? p)
      (disallow (semi-parsed-begin-for-syntax-s p))]
     [(parsed-define-values? p)
      (check-expr (parsed-define-values-rhs p) (length (parsed-define-values-syms p)) p)]
     [(or (parsed-#%declare? p)
          (parsed-module? p)
          (syntax? p)) ;; remaining unparsed forms, such as `#%require` and `#%provide`, are ok
      (void)]
     [else
      (disallow p)])))

(define (check-expr e num-results enclosing)
  (cond
    [(or (parsed-lambda? e)
         (parsed-case-lambda? e))
     (check-count 1 num-results enclosing)]
    [(parsed-quote? e)
     (check-datum (parsed-quote-datum e))
     (check-count 1 num-results enclosing)]
    [(parsed-app? e)
     (define rands (cdr (parsed-app-rator+rands e)))
     (for ([rand (in-list rands)])
       (check-expr rand 1 e))
     (case (cross-phase-primitive-name (car (parsed-app-rator+rands e)))
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
  (and (parsed-quote? e)
       (string? (syntax-e (parsed-quote-datum e)))))

(define (cross-phase-primitive-name id)
  (cond
   [(parsed-id? id)
    (define b (parsed-id-binding id))
    (and (module-binding? b)
         (eq? runtime-module-name (module-path-index-resolve (module-binding-module b)))
         (module-binding-sym b))]
   [else #f]))

(define (disallow body)
  (raise-syntax-error 'module
                      "not allowed in a cross-phase persistent module"
                      (if (parsed? body)
                          (parsed-s body)
                          body)))
