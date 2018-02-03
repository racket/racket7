#lang racket/base
(require "match.rkt"
         "id.rkt"
         "vehicle.rkt"
         "struct.rkt")

(provide inline-function
         extract-inline-predicate)

(define (inline-function rator n rands in-lam knowns #:can-gc? [can-gc? #t])
  (case rator
    [(car unsafe-car) (and (= n 1) '__pair_car)]
    [(cdr unsafe-cdr) (and (= n 1) '__pair_cdr)]
    [(cadr) (and (= n 1) '__pair_cadr)]
    [(cdar) (and (= n 1) '__pair_cdar)]
    [(cddr) (and (= n 1) '__pair_cddr)]
    [(caar) (and (= n 1) '__pair_caar)]
    [(cons) (and (= n 2) can-gc? 'scheme_make_pair)]
    [(unbox unsafe-unbox unsafe-unbox*) (and (= n 1) '__box_ref)]
    [(weak-box-value) (and (or (= n 1) (= n 2)) '__weak_box_value)]
    [(set-box! unsafe-set-box! unsafe-set-box*!) (and (= n 2) '__box_set)]
    [(vector-ref unsafe-vector-ref) (and (= n 2) '__vector_ref)]
    [(vector*-set! unsafe-vector*-set! unsafe-vector*-set!) (and (= n 3) '__vector_set)]
    [(unsafe-vector*-ref) (and (= n 2) '__authentic_vector_ref)]
    [(vector-length unsafe-vector-length unsafe-vector*-length) (and (= n 1) '__vector_length)]
    [(fx+ unsafe-fx+) (and (= n 2) '__int_add)]
    [(add1) (and (= n 1) can-gc? '__number_add1)]
    [(sub1) (and (= n 1) can-gc? '__number_sub1)]
    [(hash-ref) (cond
                  [(= n 3) (and can-gc? (known-non-procedure? (caddr rands) knowns) '__hash_ref)]
                  [(= n 2) (and can-gc? '__hash_ref2)]
                  [else #f])]
    [(hash-set) (and (= n 3) can-gc? '__hash_set)]
    [(hash-count) (and (= n 1) can-gc? '__hash_count)]
    [(hash-iterate-first) (and (= n 1) can-gc? '__hash_iterate_first)]
    [(unsafe-immutable-hash-iterate-first) (and (= n 1) can-gc? '__unsafe_immutable_hash_iterate_first)]
    [(unsafe-immutable-hash-iterate-next) (and (= n 2) can-gc? '__unsafe_immutable_hash_iterate_next)]
    [(unsafe-immutable-hash-iterate-key) (and (= n 2) can-gc? '__unsafe_immutable_hash_iterate_key)]
    [(unsafe-immutable-hash-iterate-key+value) (and (= n 2) can-gc? '__unsafe_immutable_hash_iterate_key_value)]
    [(prefab-struct-key) (and (= n 1) '__prefab_struct_key)]
    [else
     (define-values (pred-exprs pred-gc? pred-inliner)
       (extract-inline-predicate (cons rator (for/list ([i (in-range n)]) '__unknown)) in-lam knowns))
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

(define (extract-inline-predicate e in-lam knowns #:compose? [compose? #f])
  (define (compose e gc? wrapper)
    (define-values (new-es new-gc? new-wrapper) (extract-inline-predicate e in-lam knowns #:compose? compose?))
    (values new-es (or gc? new-gc?) (lambda (s) (wrapper (new-wrapper s)))))
  (define (generic e)
    (if compose?
        (values (list e) #f (lambda (s) (format "__scheme_truep(~a)" s)))
        (values #f #f #f)))
  ;; simple => no GC
  (define (simple template #:can-gc? [can-gc? #f] . args)
    (values args can-gc? (lambda (s) (format template s))))
  (match e
    [`(not ,e)
     (if compose?
         (compose e #f (lambda (s) (format "!~a" s)))
         (values (list e) #f (lambda (s) (format "__scheme_falsep(~a)" s))))]
    [`(null? ,e) (simple "__scheme_nullp(~a)" e)]
    [`(eof-object? ,e) (simple "__scheme_eof_objectp(~a)" e)]
    [`(void? ,e) (simple "__scheme_voidp(~a)" e)]
    [`(boolean? ,e) (simple "__scheme_boolp(~a)" e)]
    [`(number? ,e) (simple "__scheme_numberp(~a)" e)]
    [`(pair? ,e) (simple "__scheme_pairp(~a)" e)]
    [`(list? ,e) (simple "__scheme_listp(~a)" e)]
    [`(vector? ,e) (simple "__scheme_chaperone_vectorp(~a)" e)]
    [`(box? ,e) (simple "__scheme_chaperone_boxp(~a)" e)]
    [`(symbol? ,e) (simple "__scheme_symbolp(~a)" e)]
    [`(keyword? ,e) (simple "__scheme_keywordp(~a)" e)]
    [`(string? ,e) (simple "__scheme_char_stringp(~a)" e)]
    [`(bytes? ,e) (simple "__scheme_byte_stringp(~a)" e)]
    [`(char? ,e) (simple "__scheme_charp(~a)" e)]
    [`(hash? ,e) (simple "__scheme_hashp(~a)" e)]
    [`(eq? ,e1 ,e2) (simple "__same_obj(~a)" e1 e2)]
    [`(eqv? ,e1 ,e2) (simple "scheme_eqv(~a)" e1 e2)]
    [`(equal? ,e1 ,e2) (simple #:can-gc? #t "scheme_equal(~a)"e1 e2)]
    [`(char=? ,e1 ,e2) (simple "__scheme_char_eq(~a)" e1 e2)]
    [`(char-whitespace? ,e) (simple "__scheme_char_whitespacep(~a)" e)]
    [`(unsafe-fx< ,e1 ,e2) (simple "__int_lt(~a)" e1 e2)]
    [`(unsafe-fx> ,e1 ,e2) (simple "__int_gt(~a)" e1 e2)]
    [`(unsafe-fx>= ,e1 ,e2) (simple "!__int_lt(~a)" e1 e2)]
    [`(unsafe-fx<= ,e1 ,e2) (simple "!__int_gt(~a)" e1 e2)]
    [`(unsafe-fx= ,e1 ,e2) (simple "__same_obj(~a)" e1 e2)]
    [`(= ,e1 ,e2) (simple #:can-gc? #t "__number_eq(~a)" e1 e2)]
    [`(< ,e1 ,e2) (simple #:can-gc? #t "__number_lt(~a)" e1 e2)]
    [`(> ,e1 ,e2) (simple #:can-gc? #t "__number_gt(~a)" e1 e2)]
    [`(<= ,e1 ,e2) (simple #:can-gc? #t "__number_lt_eq(~a)" e1 e2)]
    [`(>= ,e1 ,e2) (simple #:can-gc? #t "__number_gt_eq(~a)" e1 e2)]
    [`(zero? ,e) (simple "__number_zerop(~a)" e)]
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
                   (lambda (s) (format "__is_authentic_struct_instance(~a, ~a)" s (top-ref in-lam s-id)))]
                  [else
                   (lambda (s) (format "__is_struct_instance(~a, ~a)" s (top-ref in-lam s-id)))]))]
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
