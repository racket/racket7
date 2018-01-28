#lang racket/base
(require "match.rkt"
         "arg.rkt")

;; Extract all primitives that are referenced

(provide extract-prim-names)

(define (extract-prim-names names e env top-names)
  (match e
    [`(define ,_ ,rhs)
     (extract-prim-names names rhs env top-names)]
    [`(define-values ,_ ,rhs)
     (extract-prim-names names rhs env top-names)]
    [`(begin ,es ...)
     (for/fold ([names names]) ([e (in-list es)])
       (extract-prim-names names e env top-names))]
    [`(begin0 ,es ...)
     (extract-prim-names names `(begin . ,es) env top-names)]
    [`(lambda ,ids . ,body)
     (extract-prim-names names `(begin . ,body) (add-args env ids) top-names)]
    [`(case-lambda [,idss . ,bodys] ...)
     (for/fold ([names names]) ([ids (in-list idss)]
                                [body (in-list bodys)])
       (extract-prim-names names `(begin . ,body) (add-args env ids) top-names))]
    [`(quote ,_) names]
    [`(if ,tst ,thn ,els)
     (define names1 (extract-prim-names names tst env top-names))
     (define names2 (extract-prim-names names1 thn env top-names))
     (extract-prim-names names2 els env top-names)]
    [`(with-continuation-mark ,key ,val ,body)
     (define names1 (extract-prim-names names key env top-names))
     (define names2 (extract-prim-names names1 val env top-names))
     (extract-prim-names names2 body env top-names)]
    [`(let . ,_)
     (extract-let-prim-names names e env top-names)]
    [`(letrec . ,_)
     (extract-let-prim-names names e env top-names)]
    [`(letrec* . ,_)
     (extract-let-prim-names names e env top-names)]
    [`(set! ,id ,rhs)
     (extract-prim-names names rhs env top-names)]
    [`(#%app . ,r)
     (extract-prim-names names r env top-names)]
    [`(,rator ,rands ...)
     (extract-prim-names names `(begin ,rator . ,rands) env top-names)]
    [`,_
     (cond
       [(symbol? e)
        (if (or (hash-ref env e #f)
                (hash-ref top-names e #f))
            names
            (hash-set names e #t))]
       [else names])]))

(define (extract-let-prim-names names e env top-names)
  (match e
    [`(,let-id ([,ids ,rhss] ...) . ,body)
     (define body-env (for/fold ([env env]) ([id (in-list ids)])
                        (hash-set env id #t)))
     (define rhs-env (if (eq? let-id 'let) env body-env))
     (define new-names
       (for/fold ([names names]) ([rhs (in-list rhss)])
         (extract-prim-names names rhs rhs-env top-names)))
     (extract-prim-names new-names `(begin . ,body) body-env top-names)]))
