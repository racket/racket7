#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "simple.rkt"
         "literal.rkt"
         "inline.rkt")

(provide infer-known
         lambda?)

(define (infer-known rhs defn? rec? id knowns prim-knowns imports mutated)
  (cond
    [(lambda? rhs)
     (if (can-inline? rhs)
         (known-procedure/can-inline rhs)
         a-known-procedure)]
    [(and (literal? rhs)
          (not (hash-ref mutated (unwrap id) #f)))
     (known-literal (unwrap-literal rhs))]
    [(and (symbol? (unwrap rhs))
          (not (hash-ref mutated (unwrap id) #f)))
     (define u-rhs (unwrap rhs))
     (cond
       [(or (hash-ref prim-knowns u-rhs #f)
            (and (not (hash-ref mutated u-rhs #f))
                 (hash-ref-either knowns imports u-rhs)))
        => (lambda (known) known)]
       [else (and defn? a-known-unknown)])]
    [(and defn?
          (simple? rhs prim-knowns knowns imports mutated))
     a-known-unknown]
    [else #f]))
  
;; ----------------------------------------

;; Recognize forms that produce plain procedures
(define (lambda? v)
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                            (lambda? body))]
    [`(letrec-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                               (lambda? body))]
    [`(let-values ,_ ,body) (lambda? body)]
    [`(letrec-values ,_ ,body) (lambda? body)]
    [`,_ #f]))
