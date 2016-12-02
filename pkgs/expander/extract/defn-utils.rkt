#lang racket/base
(require racket/match
         "../run/status.rkt"
         "../compile/side-effect.rkt")
(provide (all-defined-out))

(define (defn? e)
  (match e [`(define-values ,_ ,_) #t] [_ #f]))
(define defn-syms cadr)
(define defn-rhs caddr)

(define (lambda-arity e)
  (match e
    [`(lambda (,args ...) ,_) (length args)]
    [_ #f]))

(define (struct-arity e defns)
  (match e
    [`(make-struct-type ,_ #f ,i 0 #f . ,_)
     i]
    [`(make-struct-type ,_ ,s ,i 0 #f . ,_)
     (define h (hash-ref defns s #f))
     (and (known-struct-op? h)
          (eq? (known-struct-op-type h) 'struct-type)
          (+ i (known-struct-op-field-count h)))]
    [`(let-values (((,ty ,mk ,pred ,ref ,mut)
                    (let-values ()
                      (let-values ()
                        ,mst))))
       (values ,ty ,mk ,pred
               (make-struct-field-accessor ,refs ,is ,_) ...))
     (define f (struct-arity mst defns))
     (and f
          (for/and ([r (in-list refs)] [i (in-list is)])
            (and (< i f) (eq? r ref)))
          f)]
    [_ #f]))

(define (add-defn-types! seen-defns syms rhs)
  (for ([s (in-list syms)])
    (unless (hash-ref seen-defns s #f)
      (hash-set! seen-defns s (known-defined))))
  (cond 
   ;; known-arity lambda, not known to be pure
   [(and (= 1 (length syms)) (lambda-arity rhs))
    =>
    (lambda (a) (hash-set! seen-defns (car syms) (known-function a #f)))]
   [(and (= 5 (length syms)) (struct-arity rhs seen-defns))
    =>
    (lambda (a)
      (for ([sym (in-list syms)]
            [type (in-list '(struct-type
                             constructor
                             predicate
                             accessor
                             mutator))])
        (hash-set! seen-defns sym (known-struct-op type a))))]
   [(and (>= 3 (length syms)) (struct-arity rhs seen-defns))
    =>
    (lambda (a) 
      (for ([sym (in-list syms)]
            [type (in-list '(struct-type
                             constructor
                             predicate))])
        (hash-set! seen-defns sym (known-struct-op type a))))]
   [(and (= 3 (length syms)) (simple-property? rhs))
    (hash-set! seen-defns (list-ref syms 0) (known-property))
    (hash-set! seen-defns (list-ref syms 1) (known-function 1 #t))
    (hash-set! seen-defns (list-ref syms 2) (known-function 1 #t))]))

;; checks for properties without guards
(define (simple-property? e)
  (match e
    [`(make-struct-type-property ,_) #t]
    [_ #f]))

