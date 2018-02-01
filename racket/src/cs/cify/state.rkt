#lang racket/base
(require "match.rkt")

(provide make-state
         extract-state!
         mutated?
         referenced?
         state-first-pass?
         state-tops-pass?
         adjust-state!)

;; The state table maps
;;
;;  * symbols (for variables names) to
;;     - 'mutated
;;     - an integer for the use count
;;
;;  * `lam` records for union-find of functions
;;    that tail-call each other
;;
;;  * '#:runstack to information recorded and used
;;     by "runstack.rkt"
;;
(define (make-state) (make-hasheq))

(define (mutated? v) (eq? v 'mutated))
(define (referenced? v) v)

(define (state-first-pass? state)
  (not (hash-ref state '#:done? #f)))

(define (state-tops-pass? state)
  (hash-ref state '#:tops? #f))

(define (adjust-state! state id delta)
  (define new-n (+ (hash-ref state id 0) delta))
  (if (zero? new-n)
      (hash-remove! state id)
      (hash-set! state id new-n)))

;; ----------------------------------------

(define (extract-state! state e)
  (match e
    [`(define ,_ ,rhs)
     (extract-state! state rhs)]
    [`(define-values ,_ ,rhs)
     (extract-state! state rhs)]
    [`(begin ,es ...)
     (for ([e (in-list es)])
       (extract-state! state e))]
    [`(begin0 ,es ...)
     (extract-state! state `(begin . ,es))]
    [`(lambda ,ids . ,body)
     (extract-state! state `(begin . ,body))]
    [`(case-lambda [,idss . ,bodys] ...)
     (for ([body (in-list bodys)])
       (extract-state! state `(begin . ,body)))]
    [`(quote ,_) state]
    [`(if ,tst ,thn ,els)
     (extract-state! state tst)
     (extract-state! state thn)
     (extract-state! state els)]
    [`(with-continuation-mark ,key ,val ,body)
     (extract-state! state key)
     (extract-state! state val)
     (extract-state! state body)]
    [`(let . ,_)
     (extract-let-state! state e)]
    [`(letrec . ,_)
     (extract-let-state! state e)]
    [`(letrec* ([,ids . ,_] ...) . ,_)
     (for ([id (in-list ids)])
       (hash-set! state id 'mutated))
     (extract-let-state! state e)]
    [`(set! ,id ,rhs)
     (hash-set! state id 'mutated)
     (extract-state! state rhs)]
    [`(#%app . ,r)
     (extract-state! state r)]
    [`(,rator ,rands ...)
     (extract-state! state `(begin ,rator . ,rands))]
    [`,_
     (when (symbol? e)
       (unless (mutated? (hash-ref state e #f))
         (hash-set! state e (add1 (hash-ref state e 0)))))]))

(define (extract-let-state! state e)
  (match e
    [`(,_ ([,_ ,rhss] ...) . ,body)
     (for ([rhs (in-list rhss)])
       (extract-state! state rhs))
     (extract-state! state `(begin . ,body))]))
