#lang racket/base
(require '#%linklet
         (for-syntax racket/base))

(provide unwrap unwrap-list
         wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
         wrap-eq? wrap-equal?
         in-wrap-list

         define-wrap)

;; ----------------------------------------

;; Get `correlated?` and `correlated-e` reflectively, so that this
;; module can be flattened by the expander to a linklet that doesn't
;; refer to syntax primitives

(define kernel-table (primitive-table '#%kernel))

(define correlated?
  (or (and kernel-table
           (hash-ref kernel-table 'syntax?))
      (lambda (x) #f)))

(define correlated-e
  (or (and kernel-table
           (hash-ref kernel-table 'syntax-e))
      (lambda (x) x)))

;; ----------------------------------------

(define-syntax-rule (define-wrap (unwrap unwrap-list
                                         wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
                                         wrap-eq? wrap-equal?
                                         in-wrap-list)
                      correlated? correlated-e)
  (...
   (begin
     (define (unwrap v)
       (if (correlated? v)
           (correlated-e v)
           v))

     (define (unwrap-list v)
       (cond
         [(null? v) null]
         [(pair? v)
          (let ([r (unwrap-list (cdr v))])
            (if (eq? r (cdr v))
                v
                (cons (car v) r)))]
         [(correlated? v) (unwrap-list (correlated-e v))]
         [else v]))

     (define (wrap-car v)
       (if (correlated? v)
           (car (correlated-e v))
           (car v)))

     (define (wrap-cdr v)
       (if (correlated? v)
           (cdr (correlated-e v))
           (cdr v)))

     (define (wrap-pair? v)
       (pair? (unwrap v)))

     (define (wrap-null? v)
       (null? (unwrap v)))

     (define (wrap-list? v)
       (cond
         [(null? v) #t]
         [(correlated? v) (wrap-list? (correlated-e v))]
         [(pair? v) (wrap-list? (cdr v))]
         [else #f]))

     (define (wrap-eq? a b)
       (eq? (unwrap a) (unwrap b)))

     (define (wrap-equal? a b)
       (let ([b (unwrap b)])
         (or (and (not (pair? a))
                  (equal? a b))
             (and (pair? a)
                  (pair? b)
                  (wrap-equal? (car a) (car b))
                  (wrap-equal? (car a) (car b))))))

     (define-sequence-syntax in-wrap-list
       (lambda (stx) (raise-argument-error "allowed only in `for` forms" stx))
       (lambda (stx)
         (syntax-case stx ()
           [[(id) (_ lst-expr)]
            (for-clause-syntax-protect
             #'[(id)
                (:do-in
                 ;;outer bindings
                 ([(lst) lst-expr])
                 ;; outer check
                 (void)
                 ;; loop bindings
                 ([lst lst])
                 ;; pos check
                 (not (wrap-null? lst))
                 ;; inner bindings
                 ([(id) (if (wrap-pair? lst) (wrap-car lst) lst)]
                  [(rest) (if (wrap-pair? lst) (wrap-cdr lst) null)])
                 ;; pre guard
                 #t
                 ;; post guard
                 #t
                 ;; loop args
                 (rest))])]
           [_ #f]))))))

 (define-wrap (unwrap unwrap-list
                      wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
                      wrap-eq? wrap-equal?
                      in-wrap-list)
   correlated? correlated-e)
