#lang racket/base
(require racket/list
         "inline.rkt"
         "state.rkt"
         "id.rkt"
         "ref.rkt"
         "runstack.rkt")

(provide simple?
         generate-simple
         simple-quote?
         always-fixnum?)

;; A simple expression is one that can be reordered and doesn't
;; trigger a GC.

(define (simple? e state knowns #:can-gc? [can-gc? #f])
  (or (and (symbol-ref? e)
           (not (mutated? (hash-ref state (unref e) #f))))
      (simple-quote? e)
      (and (pair? e)
           (symbol? (car e))
           (inline-function (car e) (length (cdr e)) (cdr e) knowns #:can-gc? can-gc?)
           (for/and ([e (in-list (cdr e))])
             (simple? e state knowns #:can-gc? can-gc?)))))

;; The `e` argument can be a string as pre-generated
(define (generate-simple e env state runstack top-names knowns prim-names)
  (cond
    [(string? e) e]
    [(boolean? e) (if e "scheme_true" "scheme_false")]
    [(always-fixnum? e) (format "scheme_make_integer(~a)" e)]
    [(symbol-ref? e)
     (cond
       [(ref? e)
        (ref-use! e state)
        (runstack-ref runstack (unref e) #:ref e)]
       [(or (hash-ref top-names e #f)
            (hash-ref knowns e #f))
        (format "__top->~a" (cify e))]
       [(hash-ref prim-names e #f)
        (format "__prims.~a" (cify e))]
       [else (runstack-ref runstack e)])]
    [else
     (define inliner (inline-function (car e) (length (cdr e)) (cdr e) knowns))
     (define args (apply string-append
                         (append
                          (add-between
                           (for/list ([e (in-list (cdr e))])
                             (format "~a" (generate-simple e env state runstack top-names knowns prim-names)))
                           ", "))))
     (cond
       [(procedure? inliner) (inliner args)]
       [else
        (format "~a(~a)" inliner args)])]))

;; ----------------------------------------

(define (simple-quote? e)
  (or (always-fixnum? e)
      (boolean? e)
      (null? e)
      (void? e)
      (and (char? e)
           (<= 0 (char->integer e) 255))))

(define (always-fixnum? e)
  (and (integer? e)
       (exact? e)
       (<= (- (expt 2 30)) e (sub1 (expt 2 30)))))
