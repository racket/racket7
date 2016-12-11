#lang racket/base
(require (for-syntax racket/base))

(provide adjust-for-loop-head-retention)

;; The `adjust-for-loop-head-retention` macro rewrites a `for/list` use
;; so that the head of a list is not implicitly retained while
;; evaluating the body of the loop. Instead, the `rest` is taken at
;; the start of the loop body, so that the head can be reclaimed by a
;; GC after the body stops using it.

(define-syntax (adjust-for-loop-head-retention stx)
  (syntax-case stx (for/list)
    [(_ (for/list ([var rhs] ...)
          body0 body ...))
     (with-syntax ([([loop-var init done? var head rest-var rest] ...)
                    (for/list ([var (in-list (syntax->list #'(var ...)))]
                               [rhs (in-list (syntax->list #'(rhs ...)))])
                      (with-syntax ([(loop-var rest-var)
                                     (generate-temporaries (list (format "~as" (syntax-e var))
                                                                 (format "~a-rest" (syntax-e var))))]
                                    [var var])
                        (with-syntax ([(init done? head rest)
                                       (syntax-case rhs (in-list in-naturals)
                                         [(in-list l)
                                          #'(l (null? loop-var) (car loop-var) (cdr loop-var))]
                                         [(in-naturals)
                                          #'(0 #f loop-var (add1 loop-var))])])
                          #'[loop-var init done? var head rest-var rest])))])
       #'(let for-loop/head ([loop-var init] ...)
           (cond
            [(or done? ...) null]
            [else
             (let ([rest-var rest] ...
                   [var head] ...)
               (cons (let () body0 body ...)
                     (for-loop/head rest-var ...)))])))]))
