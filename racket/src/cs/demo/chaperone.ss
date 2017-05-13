(import (core))

(define-syntax check
  (syntax-rules ()
    [(_ got expect)
     (let ([v got]
           [expect-v expect])
       (unless (equal? v expect-v)
         (error 'check (format "failed: ~s => ~s" 'got v))))]))

;; ----------------------------------------

(define v1 (vector 1 2 3))
(define v2 (vector 1 2 3))

(check (impersonator-of? v1 v2)
       #t)
(check (impersonator-of? v2 v1)
       #t)

(define v1i (impersonate-vector v1
                                (lambda (v i e) (- e))
                                (lambda (v i e) (* 2 e))))

(check (vector? v1) #t)
(check (vector? v2) #t)
(check (vector? v1i) #t)

(check (vector-ref v1i 1) -2)
(check (vector-ref v1 1) 2)
(check (vector-set! v1 1 5) (void))
(check (vector-ref v1i 1) -5)
(check (vector-ref v1 1) 5)
(check (vector-set! v1i 1 6) (void))
(check (vector-ref v1i 1) -12)
(check (vector-ref v1 1) 12)

(check (vector-set! v2 1 12) (void))

(check (impersonator-of? v1i v2)
       #t)
(check (impersonator-of? v2 v1i)
       #f)
(check (impersonator-of? v1i v1)
       #t)
(check (impersonator-of? v1 v1i)
       #f)

(define v1j (impersonate-vector v1
                                #f
                                #f))

(check (vector? v1j) #t)

(check (impersonator-of? v1j v1)
       #t)
(check (impersonator-of? v1 v1j)
       #t)

(define v1c (chaperone-vector v1
                              (lambda (v i e) e)
                              (lambda (v i e) e)))

(check (chaperone-of? v1c v1)
       #t)
(check (chaperone-of? v1i v1)
       #t)
(check (impersonator-of? v1c v1)
       #t)

(define vv (vector (vector 1 2 3)
                   (vector 4 5 6)))
(define vvc (chaperone-vector vv
                              (lambda (v i e)
                                (chaperone-vector
                                 e
                                 (lambda (v i e) e)
                                 #f))
                              (lambda (v i e) e)))
(check (chaperone-of? vvc vv)
       #t)
(check (chaperone-of? (vector-ref vvc 0) (vector-ref vv 0))
       #t)

;; ----------------------------------------

(define (f x y)
  (list x y))

(define fi (impersonate-procedure f (lambda (x y)
                                      (values (- x) (- y)))))
(define fc (chaperone-procedure f (lambda (x y)
                                    (values x y))))

(check (f 1 2) '(1 2))
(check (|#%app| fc 1 2) '(1 2))
(check (|#%app| fi 1 2) '(-1 -2))

(check (impersonator-of? fc f) #t)
(check (impersonator-of? fi f) #t)
(check (impersonator-of? fi fc) #f)
(check (impersonator-of? fc fi) #f)

(check (chaperone-of? fc f) #t)
(check (chaperone-of? fi f) #t)
(check (chaperone-of? fi fc) #f)
(check (chaperone-of? fc fi) #t)

(define fc2 (chaperone-procedure f
                                 (lambda (x y)
                                   (values (chaperone-vector
                                            x
                                            (lambda (v i e) e)
                                            (lambda (v i e) e))
                                           y))))

(check (|#%app| fc2 v1 0) (list v1 0))
(check (chaperone-of? (|#%app| fc2 v1 0) (list v1 0))
       #t)

(define fc* (chaperone-procedure* f (lambda (orig x y)
                                      (check orig fc*)
                                      (values x y))))
(check (|#%app| fc* 'a 'b) '(a b))

;; ----------------------------------------

(define-values (iprop:flavor flavor? flavor-ref)
  (make-impersonator-property 'flavor))

(check (|#%app| flavor? 1) #f)
(check (|#%app| flavor? f) #f)

(define fcp (chaperone-procedure f
                                 (lambda (x y)
                                   (values x y))
                                 iprop:flavor 'spicy))

(check (|#%app| flavor? fcp) #t)
(check (|#%app| flavor-ref fcp) 'spicy)

(check (|#%app| fcp 3 4) '(3 4))

;; ----------------------------------------

(define (g x y)
  (list x y (continuation-mark-set->list
             (current-continuation-marks)
             'calling)))

(check (g 1 2) '(1 2 ()))

(define gcam (chaperone-procedure g
                                  (lambda (x y)
                                    (values x y))
                                  impersonator-prop:application-mark
                                  (cons 'calling 'london)))

(check (|#%app| gcam 1 2) '(1 2 (london)))

(check (with-continuation-mark 'calling 'madrid
         (|#%app| gcam 1 2))
       '(1 2 (london)))


(define giam (impersonate-procedure g
                                    (lambda (x y)
                                      ;; Has a result wrapper, so call of `g`
                                      ;; will not be in tail positions
                                      (values (lambda (r) r)
                                              x
                                              (continuation-mark-set->list
                                               (current-continuation-marks)
                                               'calling)))
                                    impersonator-prop:application-mark
                                    (cons 'calling 'paris)))

(check (|#%app| giam 1 2) '(1 () (paris)))

(check (with-continuation-mark 'calling 'madrid
         (|#%app| giam 1 2))
       '(1 (madrid madrid) (paris madrid)))

(check (|#%app|
        (chaperone-procedure (lambda (x) (list
                                          (continuation-mark-set->list
                                           (current-continuation-marks)
                                           'a)
                                          x))
                             (lambda (x) (values 'mark 'a 'b x)))
        1)
       '((b) 1))
