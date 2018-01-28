#lang racket/base
(require racket/list
         "inline.rkt"
         "state.rkt"
         "env.rkt"
         "id.rkt")

(provide simple?
         generate-simple
         simple-quote?
         generate-simple-quote)

;; A simple expression is one that can be reordered and can be nested
;; in other simple expression without aggravating xform.

(define (simple? e state knowns #:can-gc? [can-gc? #t])
  (or (and (symbol? e)
           (not (mutated? (hash-ref state e #f))))
      (boolean? e)
      (always-fixnum? e)
      (and (pair? e)
           (symbol? (car e))
           (inline-function (car e) (length (cdr e)) knowns #:can-gc? can-gc?)
           (for/and ([e (in-list (cdr e))])
             (simple? e state knowns #:can-gc? can-gc?)))))

;; The `e` argument can be a string as pre-generated
(define (generate-simple e env top-names knowns)
  (cond
    [(string? e) e]
    [(boolean? e) (if e "scheme_true" "scheme_false")]
    [(always-fixnum? e) (format "scheme_make_integer(~a)" e)]
    [(symbol? e)
     (cond
       [(hash-ref env e #f)
        => (lambda (b)
             (if (propagate? b)
                 (generate-simple b env top-names knowns)
                 (cify e)))]
       [(or (hash-ref top-names e #f)
            (hash-ref knowns e #f))
        (format "top.~a" (cify e))]
       [else
        (format "prims.~a" (cify e))])]
    [else
     (define inliner (inline-function (car e) (length (cdr e)) knowns))
     (define args (apply string-append
                         (append
                          (add-between
                           (for/list ([e (in-list (cdr e))])
                             (format "~a" (generate-simple e env top-names knowns)))
                           ", "))))
     (cond
       [(procedure? inliner) (inliner args)]
       [else
        (format "~a(~a)" inliner args)])]))

(define (simple-quote? e)
  (or (symbol? e)
      (string? e)
      (bytes? e)
      (number? e)
      (and (pair? e)
           (simple-quote? (car e))
           (simple-quote? (cdr e)))
      (boolean? e)
      (null? e)
      (void? e)
      (char? e)))

(define (generate-simple-quote e)
  (cond
    [(symbol? e)
     (format "scheme_intern_symbol(~s)" (symbol->string e))]
    [(string? e)
     (define s (string->bytes/utf-8 e))
     (format "scheme_make_sized_utf8_string(~s, ~a)"
             (bytes->string/latin-1 s)
             (bytes-length s))]
    [(bytes? e)
     (format "scheme_make_sized_byte_string(~s, ~a, 0)"
             (bytes->string/latin-1 e)
             (bytes-length e))]
    [(number? e)
     (cond
       [(always-fixnum? e)
        (format "scheme_make_integer(~a)" e)]
       [(eqv? e +inf.0) "scheme_inf_object"]
       [(eqv? e -inf.0) "scheme_minus_inf_object"]
       [(eqv? e +nan.0) "scheme_minus_inf_object"]
       [(eqv? e +inf.f) "scheme_single_inf_object"]
       [(eqv? e -inf.f) "scheme_single_minus_inf_object"]
       [(eqv? e +nan.f) "scheme_single_minus_inf_object"]
       [else
        (format "scheme_make_double(~a)" e)])]
    [(pair? e) (format "scheme_make_pair(~a, ~a)"
                       (generate-simple-quote (car e))
                       (generate-simple-quote (cdr e)))]
    [(boolean? e) (if e "scheme_true" "scheme_false")]
    [(null? e) "scheme_null"]
    [(void? e) "scheme_void"]
    [(char? e) (format "scheme_make_char(~a)" (char->integer e))]
    [else (error 'generate-simple-quote "not handled: ~e" e)]))

(define (always-fixnum? e)
  (and (integer? e)
       (exact? e)
       (<= (- (expt 2 30)) e (sub1 (expt 2 30)))))
