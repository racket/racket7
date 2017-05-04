(import (core))

(define (show v) (printf "~s\n" v) v)

(define-syntax check
  (syntax-rules ()
    [(_ a b)
     (let ([v a])
       (unless (equal? v b)
         (error 'check (format "failed ~s => ~s" 'a v))))]))

;; ----------------------------------------

(define-values (prop:x x? x-ref) (make-struct-type-property 'x))

(define-values (struct:a make-a a? a-ref a-set!)
  (make-struct-type 'a #f 2 0 #f (list (cons prop:x 5))))
(define a-x (make-struct-field-accessor a-ref 0 'x))
(define a-y (make-struct-field-accessor a-ref 1 'y))
(define-values (struct:b make-b b? b-ref b-set!)
  (make-struct-type 'b #f 2 0 #f (list
                                  (cons prop:equal+hash
                                        (list (lambda (o t eql?)
                                                (eql? (b-x o) (b-x t)))
                                              (lambda (o hc) 0)
                                              (lambda (o hc) 0))))))
(define b-x (make-struct-field-accessor b-ref 0 'x))
(define b-y (make-struct-field-accessor b-ref 1 'y))

(define an-a (make-a 1 2))
(define b1 (make-b 3 4))
(define b2 (make-b 3 4))

(check (a-x an-a) 1)
(check (|#%app| a-ref an-a 0) 1)
(check (|#%app| a-ref an-a 1) 2)

(time (let loop ([i 10000000] [v1 (make-b 3 4)] [v2 (make-b 3 4)])
        (cond
         [(= i 0) (list b1 b2)]
         [else (loop (sub1 i) (if (equal? v1 v2) v2 v1) v1)])))

         
(define-values (struct:p make-p p? p-ref p-set!)
  (make-struct-type 'p #f 2 0 #f (list (cons prop:procedure 0))))

(check (|#%app| (make-p (lambda (x) (cons x x)) 'whatever) 10) '(10 . 10))

(check (procedure-arity (make-p add1 'x)) 1)
(check (procedure-arity (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x))
       (list 1 (arity-at-least 3)))
(check (procedure-arity-includes? (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x) 0)
       #f)
(check (procedure-arity-includes? (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x) 1)
       #t)
(check (procedure-arity-includes? (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x) 2)
       #f)
(check (procedure-arity-includes? (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x) 3)
       #t)
(check (procedure-arity-includes? (make-p (case-lambda [(x) 1] [(x y z . w) 2]) 'x) 3000)
       #t)

(define-values (struct:p0 make-p0 p0? p0-ref p0-set!)
  (make-struct-type 'p0 #f 2 0 #f))
(define-values (struct:p1 make-p1 p1? p1-ref p1-set!)
  (make-struct-type 'p1 struct:p0 2 0 #f (list (cons prop:procedure 0))))

(check (|#%app| (make-p (lambda (x) (cons x x)) 'whatever) 10) '(10 . 10))
(check (|#%app| (make-p1 'no 'nope (lambda (x) (list x x)) 'whatever) 11) '(11 11))

(define-values (struct:p2 make-p2 p2? p2-ref p2-set!)
  (make-struct-type 'p2 struct:p0 2 0 #f
                    (list (cons prop:procedure
                                (lambda (p2 x)
                                  (list (|#%app| p2-ref p2 0) x))))))

(check (|#%app| (make-p2 0 1 'a 'b) 'c) '(a c))
(check (procedure-arity (make-p2 0 1 'a 'b)) 1)
(check (procedure-arity-includes? (make-p2 0 1 'a 'b) 1) #t)
(check (procedure-arity-includes? (make-p2 0 1 'a 'b) 2) #f)

;; ----------------------------------------
;; Inspectors and `struct->vector`

(check (struct->vector an-a) '#(struct:a ...))

(define sub-i (make-inspector (current-inspector)))
(define-values (struct:q make-q q? q-ref q-set!)
  (make-struct-type 'q #f 2 0 #f '() sub-i))

(check (struct->vector (make-q 9 10)) '#(struct:q 9 10))

(define-values (struct:q+3 make-q+3 q+3? q+3-ref q+3-set!)
  (make-struct-type 'q+3 struct:q 3 0))

(define a-q+3 (make-q+3 9 10 'a 'b 'c))
(check (|#%app| q+3-ref a-q+3 0) 'a)
(check (|#%app| q+3-ref a-q+3 1) 'b)
(check ((make-struct-field-accessor q+3-ref 1 'second) a-q+3) 'b)
(check (struct->vector a-q+3) '#(struct:q+3 9 10 ...))

(define-values (struct:q+3+2 make-q+3+2 q+3+2? q+3+2-ref q+3+2-set!)
  (make-struct-type 'q+3+2 struct:q+3 2 0 #f '() sub-i))

(check (struct->vector (make-q+3+2 9 10 'a 'b 'c "x" "y")) '#(struct:q+3+2 9 10 ... "x" "y"))

;; ----------------------------------------
;; Prefabs

(check (prefab-key? 'a) #t)
(check (prefab-key? '(a)) #t)
(check (prefab-key? '(a 5)) #t)
(check (prefab-key? '(a 5 (0 #f))) #t)
(check (prefab-key? '(a 5 (3 #f))) #t)
(check (prefab-key? '(a (0 #f))) #t)
(check (prefab-key? '(a 3 (0 #f) #())) #t)
(check (prefab-key? '(a 3 #())) #t)
(check (prefab-key? '(a #())) #t)
(check (prefab-key? '(a 3 (0 #f) #(1 2))) #t)
(check (prefab-key? '(a 3 (10 #f) #(11 12))) #t)
(check (prefab-key? '(a #(100 101 99))) #t)
(check (prefab-key? '(a 3 (0 #f) #(2) b 1)) #t)
(check (prefab-key? '(a 3 b 1)) #t)
(check (prefab-key? '(a b 1)) #t)

(check (prefab-key? "a") #f)
(check (prefab-key? '(a a)) #f)
(check (prefab-key? '(a . 5)) #f)
(check (prefab-key? '(a 5 (x #f))) #f)
(check (prefab-key? '(a 5 (2))) #f)
(check (prefab-key? '(a 5 (3 #f 5))) #f)
(check (prefab-key? '(a (x #f))) #f)
(check (prefab-key? '(a 3 (0 #f) #(x))) #f)
(check (prefab-key? '(a 3 (0 #f) #(-2))) #f)
(check (prefab-key? '(a 3 (0 #f) #(3))) #f)
(check (prefab-key? '(a 3 #(11 12))) #f)
(check (prefab-key? '(a #(100 101 100))) #f)
(check (prefab-key? '(a 3 (0 #f) #(2) b)) #f)
(check (prefab-key? '(a 3 (0 #f) #(2) "b" 1)) #f)
(check (prefab-key? '(a 3 (0 #f) #(2) b -1)) #f)

;; ----------------------------------------

(let ()
  (define-values (struct:s-a make-s-a s-a? s-a-ref s-a-set!)
    (make-struct-type 'x #f 2 0 #f (list (cons prop:x 5))))
  (define s-a-x (make-struct-field-accessor s-a-ref 0 'x))
  (let ([an-a (make-s-a 1 2)])
    (time
     (let loop ([i 10000000] [v 0])
       (if (zero? i)
           v
           (loop (sub1 i) (+ v (s-a-x an-a))))))))

(let ()
  (define struct:s-a (make-record-type-descriptor 's #f #f #f #f '#((mutable x) (mutable y))))
  (define make-s-a (record-constructor
                    (make-record-constructor-descriptor struct:s-a #f #f)))
  (define s-a-x (record-accessor struct:s-a 0))
  (let ([an-a (make-s-a 1 2)])
    (time
     (let loop ([i 10000000] [v 0])
       (if (zero? i)
           v
           (loop (sub1 i) (+ v (s-a-x an-a))))))))

(let ()
  (define-record r-a (x y))

  (let ([an-a (make-r-a 1 2)])
    (time
     (let loop ([i 10000000] [v 0])
       (if (zero? i)
           v
           (loop (sub1 i) (+ v (r-a-x an-a))))))))

