#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "literal.rkt")

(provide optimize)

;; Perform shallow optimizations. The `schemify` pass calls `optimize`
;; on each schemified form, which means that subexpressions of the
;; immediate expression have already been optimized.

(define (optimize v prim-knowns knowns imports mutated)
  (match v
    [`(if ,t ,e1 ,e2)
     (if (literal? t)
         (if (unwrap t) e1 e2)
         v)]
    [`,_
     (define u (unwrap v))
     (cond
       [(symbol? u)
        (define k (hash-ref-either knowns imports u))
        (cond
          [(known-literal? k) (known-literal-expr k)]
          [else v])]
       [else v])]))
