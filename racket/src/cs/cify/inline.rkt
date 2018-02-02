#lang racket/base
(require "match.rkt"
         "id.rkt"
         "struct.rkt")

(provide inline-function
         extract-inline-predicate)

(define (inline-function rator n rands knowns #:can-gc? [can-gc? #t])
  (case rator
    [(car unsafe-car) (and (= n 1) '__pair_car)]
    [(cdr unsafe-cdr) (and (= n 1) '__pair_cdr)]
    [(cadr) (and (= n 1) '__pair_cadr)]
    [(cdar) (and (= n 1) '__pair_cdar)]
    [(cddr) (and (= n 1) '__pair_cddr)]
    [(caar) (and (= n 1) '__pair_caar)]
    [(cons) (and (= n 2) can-gc? 'scheme_make_pair)]
    [(unbox unsafe-unbox unsafe-unbox*) (and (= n 1) '__box_ref)]
    [(set-box! unsafe-set-box! unsafe-set-box*!) (and (= n 2) '__box_set)]
    [(vector-ref unsafe-vector-ref) (and (= n 2) '__vector_ref)]
    [(vector*-set! unsafe-vector*-set! unsafe-vector*-set!) (and (= n 3) '__vector_set)]
    [(unsafe-vector*-ref) (and (= n 2) '__authentic_vector_ref)]
    [(vector-length unsafe-vector-length unsafe-vector*-length) (and (= n 1) '__vector_length)]
    [(fx+ unsafe-fx+) (and (= n 2) '__int_add)]
    [(hash-ref) (cond
                  [(= n 3) (and can-gc? (known-non-procedure? (caddr rands) knowns) '__hash_ref)]
                  [(= n 2) (and can-gc? '__hash_ref2)]
                  [else #f])]
    [(unsafe-immutable-hash-iterate-next) (and (= n 2) can-gc? '__unsafe_immutable_hash_iterate_next)]
    [else
     (define-values (pred-exprs pred-gc? pred-inliner)
       (extract-inline-predicate (cons rator (for/list ([i (in-range n)]) '__unknown)) knowns))
     (cond
       [(and pred-inliner
             (or (not pred-gc?) can-gc?))
        (lambda (s) (format "(~a ? scheme_true : scheme_false)" (pred-inliner s)))]
       [else
        (define k (hash-ref knowns rator #f))
        (cond
          [(and (struct-accessor? k) (= n 1))
           (lambda (s)
             (if (struct-info-authentic? (struct-accessor-si k))
                 (format "__authentic_struct_ref(~a, ~a)" s (struct-accessor-pos k))
                 (format "__struct_ref(~a, ~a)" s (struct-accessor-pos k))))]
          [(and (struct-mutator? k) (= n 2))
           (lambda (s)
             (if (struct-info-authentic? (struct-mutator-si k))
                 (format "__authentic_struct_set(~a, ~a)" s (struct-mutator-pos k))
                 (format "__struct_set(~a, ~a)" s (struct-mutator-pos k))))]
          [else #f])])]))

(define (extract-inline-predicate e knowns #:compose? [compose? #f])
  (define (compose e gc? wrapper)
    (define-values (new-es new-gc? new-wrapper) (extract-inline-predicate e knowns #:compose? compose?))
    (values new-es (or gc? new-gc?) (lambda (s) (wrapper (new-wrapper s)))))
  (define (generic e)
    (if compose?
        (values (list e) #f (lambda (s) (format "__scheme_truep(~a)" s)))
        (values #f #f #f)))
  ;; simple => no GC
  (define (simple template . args)
    (values args #f (lambda (s) (format template s))))
  (match e
    [`(not ,e)
     (if compose?
         (compose e #f (lambda (s) (format "!~a" s)))
         (values (list e) #f (lambda (s) (format "__scheme_falsep(~a)" s))))]
    [`(null? ,e) (simple "__scheme_nullp(~a)" e)]
    [`(boolean? ,e) (simple "__scheme_boolp(~a)" e)]
    [`(number? ,e) (simple "__scheme_numberp(~a)" e)]
    [`(pair? ,e) (simple "__scheme_pairp(~a)" e)]
    [`(vector? ,e) (simple "__scheme_chaperone_vectorp(~a)" e)]
    [`(box? ,e) (simple "__scheme_chaperone_boxp(~a)" e)]
    [`(symbol? ,e) (simple "__scheme_symbolp(~a)" e)]
    [`(keyword? ,e) (simple "__scheme_keywordp(~a)" e)]
    [`(string? ,e) (simple "__scheme_char_stringp(~a)" e)]
    [`(bytes? ,e) (simple "__scheme_byte_stringp(~a)" e)]
    [`(eq? ,e1 ,e2) (simple "__same_obj(~a)" e1 e2)]
    [`(eqv? ,e1 ,e2) (simple "scheme_eqv(~a)" e1 e2)]
    [`(equal? ,e1 ,e2) (values (list e1 e2) #t (lambda (s) (format "scheme_equal(~a)" s)))]
    [`(unsafe-fx< ,e1 ,e2) (simple "__int_lt(~a)" e1 e2)]
    [`(unsafe-fx> ,e1 ,e2) (simple "__int_gt(~a)" e1 e2)]
    [`(unsafe-fx>= ,e1 ,e2) (simple "!__int_lt(~a)" e1 e2)]
    [`(unsafe-fx<= ,e1 ,e2) (simple "!__int_gt(~a)" e1 e2)]
    [`(unsafe-fx= ,e1 ,e2) (simple "__same_obj(~a)" e1 e2)]
    [`(,rator ,rand)
     (define k (and (symbol? rator)
                    (hash-ref knowns rator #f)))
     (cond
       [(struct-predicate? k)
        (define si (struct-predicate-si k))
        (define s-id (struct-info-struct-id si))
        (values (list rand)
                #f
                (cond
                  [(struct-info-authentic? si)
                   (lambda (s) (format "__is_authentic_struct_instance(~a, __top->~a)" s (cify s-id)))]
                  [else
                   (lambda (s) (format "__is_struct_instance(~a, __top->~a)" s (cify s-id)))]))]
       [else (generic e)])]
    [`,_ (generic e)]))

(define (known-non-procedure? e knowns)
  (or (boolean? e)
      (number? e)
      (eq? e 'null)
      (and (symbol? e)
           (let ([k (hash-ref knowns e #f)])
             (or (symbol? k)
                 (eq? k '#:non-procedure))))))
