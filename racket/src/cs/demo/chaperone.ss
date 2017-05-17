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

(define b1 (box 1))
(define b1c (chaperone-box b1 (lambda (b v) v) (lambda (b v) v)))
(define b1i (impersonate-box b1 (lambda (b v) (add1 v)) (lambda (b v) (sub1 v))))

(check (unbox b1) 1)
(check (set-box! b1 0) (void))
(check (unbox b1) 0)

(check (unbox b1c) 0)
(check (unbox b1i) 1)

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

;; ----------------------------------------

(let ()
  (define-values (prop:x x? x-ref) (make-struct-type-property 'x))
  (define-values (struct:s-a make-s-a s-a? s-a-ref s-a-set!)
    (make-struct-type 'x #f 2 0 #f (list (cons prop:x 5))))
  (define s-a-x (make-struct-field-accessor s-a-ref 0 'x))
  (define s-a-y (make-struct-field-accessor s-a-ref 1 'y))
  (define set-s-a-x! (make-struct-field-mutator s-a-set! 0 'x))
  (define counter 0)

  (define s1 (make-s-a 1 2))
  (define s1c (chaperone-struct s1
                                s-a-x (lambda (s v) (set! counter (add1 counter)) v)
                                x-ref (lambda (s v) (set! counter (add1 counter)) v)))
  (define s1i (impersonate-struct s1
                                  s-a-x (lambda (s v) (list v))
                                  set-s-a-x! (lambda (s v) (box v))))

  (check (chaperone-struct 7) 7)
  (check (impersonate-struct 7) 7)

  (check (impersonator-of? s1c s1) #t)

  (check (s-a-x s1) 1)
  (check (s-a-y s1) 2)

  (check counter 0)
  (check (s-a-x s1c) 1)
  (check counter 1)
  (check (s-a-y s1c) 2)
  (check counter 1)

  (check (s-a-x s1i) '(1))
  (check (s-a-y s1i) 2)
  (check (set-s-a-x! s1i 0) (void))
  (check (s-a-x s1i) '(#&0))

  (check counter 1)
  (check (|#%app| s-a-ref s1c 1) 2)
  (check counter 1)
  (check (|#%app| s-a-ref s1c 0) '#&0)
  (check counter 2)

  (check (|#%app| x-ref s1) 5)
  (check counter 2)
  (check (|#%app| x-ref s1c) 5)
  (check counter 3)

  (void))