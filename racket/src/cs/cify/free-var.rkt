#lang racket/base
(require "match.rkt"
         "vehicle.rkt"
         "function.rkt"
         "sort.rkt"
         "arg.rkt"
         "env.rkt")

(provide get-free-vars)

(define (get-free-vars e env lambdas knowns top-names state)
  (define lam (hash-ref lambdas e))
  (or (lam-free-vars lam)
      (let ([vars (extract-lambda-free-vars #hasheq() e env lambdas knowns top-names state)])
        (define free-vars (for/list ([var (in-sorted-hash-keys vars symbol<?)]
                                     #:when (hash-ref env var #f))
                                  var))
        (set-lam-free-vars! lam free-vars)
        (set-lam-env! lam env)
        free-vars)))

(define (extract-lambda-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(lambda ,ids . ,body)
     (extract-free-vars vars `(begin . ,body) (add-args env ids) lambdas knowns top-names state)]
    [`(case-lambda [,idss . ,bodys] ...)
     (for/fold ([vars vars]) ([ids (in-list idss)]
                              [body (in-list bodys)])
       (extract-free-vars vars `(begin . ,body) (add-args env ids) lambdas knowns top-names state))]))

(define (extract-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(begin ,es ...)
     (for/fold ([vars vars]) ([e (in-list es)])
       (extract-free-vars vars e env lambdas knowns top-names state))]
    [`(begin0 ,es ...)
     (extract-free-vars vars `(begin . ,es) env lambdas knowns top-names state)]
    [`(lambda . ,_)
     (add-args vars (get-free-vars e env lambdas knowns top-names state))]
    [`(case-lambda . ,_)
     (add-args vars (get-free-vars e env lambdas knowns top-names state))]
    [`(quote ,_) vars]
    [`(if ,tst ,thn ,els)
     (define vars1 (extract-free-vars vars tst env lambdas knowns top-names state))
     (define vars2 (extract-free-vars vars1 thn env lambdas knowns top-names state))
     (extract-free-vars vars2 els env lambdas knowns top-names state)]
    [`(with-continuation-mark ,key ,val ,body)
     (define vars1 (extract-free-vars vars key env lambdas knowns top-names state))
     (define vars2 (extract-free-vars vars1 val env lambdas knowns top-names state))
     (extract-free-vars vars2 body env lambdas knowns top-names state)]
    [`(let . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(letrec . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(letrec* . ,_)
     (extract-let-free-vars vars e env lambdas knowns top-names state)]
    [`(set! ,id ,rhs)
     (extract-free-vars (hash-set vars id #t) rhs env lambdas knowns top-names state)]
    [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
     (define vars1 (extract-free-vars vars `(begin . ,body1) env lambdas knowns top-names state))
     (extract-free-vars vars1 `(begin . ,body2) (add-args env ids) lambdas knowns top-names state)]
    [`(#%app . ,r)
     (extract-free-vars vars r env lambdas knowns top-names state)]
    [`(,rator ,rands ...)
     (cond
       [(function? (hash-ref knowns rator #f))
        (extract-free-vars vars `(begin . ,rands) env lambdas knowns top-names state)]
       [else
        (extract-free-vars vars `(begin ,rator . ,rands) env lambdas knowns top-names state)])]
    [`,_
     (cond
       [(symbol? e)
        (let loop ([e e])
          (define r (hash-ref env e #f))
          (cond
            [(propagate? r) (loop r)]
            [r (hash-set vars e #t)]
            [else vars]))]
       [else vars])]))

(define (extract-let-free-vars vars e env lambdas knowns top-names state)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define body-env (for/fold ([env env]) ([id (in-list ids)]
                                             [rhs (in-list rhss)]
                                             #:unless (hash-ref top-names id #f))
                        (hash-set env id (get-propagate id rhs env state))))
     (define rhs-env (if (eq? let-id 'let) env body-env))
     (define new-vars
       (for/fold ([vars vars]) ([rhs (in-list rhss)])
         (extract-free-vars vars rhs rhs-env lambdas knowns top-names state)))
     (extract-free-vars new-vars `(begin . ,body) body-env lambdas knowns top-names state)]))

