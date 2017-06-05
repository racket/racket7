(import (core))

(define-syntax time
  (syntax-rules ()
    [(_ expr1 expr ...)
     (let ([pre-mem (current-memory-use 'cumulative)])
       (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
         (printf "cpu time: ~s real time: ~s gc time: ~s   MB: ~s\n" cpu user gc
                 (quotient (- (current-memory-use 'cumulative) pre-mem) (* 1024 1024)))
         (apply values v)))]))

(define-values (struct:top top top? top-ref top-set!)
  (make-struct-type 'top #f 2 0 #f
                    (list (cons prop:equal+hash
                                (list
                                 (lambda (a b eql?)
                                   (eql? (top-1 a)
                                         (top-1 b)))
                                 (lambda (a hc)
                                   (hc (top-1 a)))
                                 (lambda (a hc)
                                   (hc (top-1 a))))))))
(define top-1 (make-struct-field-accessor top-ref 0))

(define-values (struct:trans trans trans? trans-ref trans-set!)
  (make-struct-type 'top #f 2 0 #f '() #f))

(define-syntax check
  (syntax-rules ()
    [(_ a b)
     (unless (equal? a b)
       (error 'check (format "failed ~s" 'a)))]))

(check (equal? (top 1 2) (top 1 3)) #t)
(check (equal? (top 1 2) (top 2 2)) #f)
(check (hash-ref (hash-set (hash) (top 1 2) 'ok) (top 1 3) #f) 'ok)

(check (equal? (trans 1 2) (trans 1 2)) #t)
(check (equal? (trans 1 2) (trans 1 3)) #f)
(check (hash-ref (hash-set (hash) (trans 1 2) 'ok) (trans 1 2) #f) 'ok)
(check (hash-ref (hash-set (hash) (trans 1 2) 'ok) (trans 1 3) #f) #f)


(check (equal? (hash 1 'x 2 'y) (hash 2 'y 1 'x)) #t)
(check (hash-ref (hash (hash 1 'x 2 'y) 7) (hash 2 'y 1 'x) #f) 7)

(define (shuffle l)
  (define a (make-vector (length l)))
  (let loop ([l l] [i 0])
    (unless (null? l)
      (let ([x (car l)])
        (let ([j (random (add1 i))])
          (unless (= j i) (vector-set! a i (vector-ref a j)))
          (vector-set! a j x)))
      (loop (cdr l) (add1 i))))
  (vector->list a))

(define l (values #;shuffle (let loop ([i 1000])
                     (if (zero? i)
                         '()
                         (cons i (loop (sub1 i)))))))

(printf "large tables\n")
(time
 (let loop ([j 1000])
   (define numbers
     (let loop ([ht (hasheqv)] [l l])
       (if (null? l)
           ht
           (loop (hash-set ht (car l) #t) (cdr l)))))
   (unless (zero? j)
     (let loop ([v #f] [i 1000])
       (if (zero? i)
           v
           (loop (hash-ref numbers i (lambda () (error 'oops "bad"))) (sub1 i))))
     (loop (sub1 j)))))
(time
 (let loop ([j 1000])
   (define numbers
     (let loop ([ht (hasheq)] [l l])
       (if (null? l)
           ht
           (loop (hash-set ht l #t) (cdr l)))))
   (unless (zero? j)
     (let loop ([v #f] [l l])
       (if (null? l)
           v
           (loop (hash-ref numbers l (lambda () (error 'oops "bad"))) (cdr l))))
     (loop (sub1 j)))))

(printf "small tables\n")
(time
 (let loop ([j 100000])
   (define numbers
     (let loop ([ht (hasheqv)] [i 10])
       (if (zero? i)
           ht
           (loop (hash-set ht i #t) (sub1 i)))))
   (define numbers2
     (let loop ([ht (hasheqv)] [i 10])
       (if (zero? i)
           ht
           (loop (hash-set ht i #t) (sub1 i)))))
   (unless (zero? j)
     (let loop ([v #f] [i 10])
       (if (zero? i)
           v
           (and (hash-keys-subset? numbers numbers2)
                (loop (hash-ref numbers i (lambda () (error 'oops "bad"))) (sub1 i)))))
     (loop (sub1 j)))))

(define numbers
  (let loop ([ht (hasheqv)] [l l])
    (if (null? l)
        ht
        (loop (hash-set ht (car l) #t) (cdr l)))))

(printf "safe iterate\n")
(time
 (let loop ([j 1000])
   (unless (zero? j)
     (let loop ([v #f] [i (hash-iterate-first numbers)] [c (hash-count numbers)])
       (if i
           (loop (hash-iterate-value numbers i)
                 (hash-iterate-next numbers i)
                 (fx1- c))
           (if (zero? c)
               v
               (error 'safe-iterate "not enough"))))
     (loop (sub1 j)))))

(printf "unsafe iterate\n")
(time
 (let loop ([j 1000])
   (unless (zero? j)
     (let loop ([v #f] [i (unsafe-immutable-hash-iterate-first numbers)] [c (hash-count numbers)])
       (if i
           (loop (unsafe-immutable-hash-iterate-value numbers i)
                 (unsafe-immutable-hash-iterate-next numbers i)
                 (fx1- c))
           (if (zero? c)
               v
               (error 'unsafe-iterate "not enough"))))
     (loop (sub1 j)))))

(printf "safe vs. unsafe on small table\n")
(let ([ht (let loop ([ht (hasheq)] [i 8])
            (if (zero? i)
                ht
                (loop (hash-set ht (gensym) #t) (sub1 i))))]
      [N 1000000])
  (time
   (let loop ([j N])
     (unless (zero? j)
       (let loop ([v #f] [i (hash-iterate-first ht)])
         (if i
             (loop (hash-iterate-value ht i)
                   (hash-iterate-next ht i))
             v))
       (loop (sub1 j)))))
  (time
   (let loop ([j N])
     (unless (zero? j)
       (let loop ([v #f] [i (unsafe-immutable-hash-iterate-first ht)])
         (if i
             (loop (unsafe-immutable-hash-iterate-value ht i)
                   (unsafe-immutable-hash-iterate-next ht i))
             v))
       (loop (sub1 j))))))

;; ----------------------------------------

(printf "mutable large tables\n")
(time
 (let loop ([j 1000])
   (define numbers (make-hash))
   (let loop ([l l])
     (if (null? l)
         (void)
         (begin
           (hash-set! numbers (car l) #t)
           (loop (cdr l)))))
   (unless (zero? j)
     (let loop ([v #f] [i 1000])
       (if (zero? i)
           v
           (loop (hash-ref numbers i #f) (sub1 i))))
     (loop (sub1 j)))))

(printf "mutable small tables\n")
(time
 (let loop ([j 100000])
   (define numbers (make-hash))
   (let loop ([i 10])
     (if (zero? i)
         (void)
         (begin
           (hash-set! numbers i #t)
           (loop (sub1 i)))))
   (unless (zero? j)
     (let loop ([v #f] [i 10])
       (if (zero? i)
           v
           (loop (hash-ref numbers i #f) (sub1 i))))
     (loop (sub1 j)))))

(define mut-numbers (make-hasheqv))
(let loop ([l l])
  (unless (null? l)
    (hash-set! mut-numbers (car l) #t)
    (loop (cdr l))))

(printf "mutable iterate\n")
(time
 (let loop ([j 1000])
   (unless (zero? j)
     (let loop ([v #f] [i (hash-iterate-first mut-numbers)])
       (if i
           (loop (hash-iterate-value mut-numbers i)
                 (hash-iterate-next mut-numbers i))
           v))
     (loop (sub1 j)))))

(printf "mutable for-each\n")
(time
 (let loop ([j 1000])
   (unless (zero? j)
     (let ([a #f])
       (hash-for-each mut-numbers (lambda (k v) (set! a v))))
     (loop (sub1 j)))))

(printf "mutable destructive for-each\n")
(time
 (let loop ([j 1000])
   (define ht (hash-copy mut-numbers))
   (unless (zero? j)
     (let ([count 0])
       (hash-for-each ht
                      (lambda (k v)
                        (set! count (add1 count))
                        (hash-remove! ht k)))
       (unless (= count (hash-count mut-numbers))
         (error 'mutable-for-each-remove! "bad count")))
     (loop (sub1 j)))))

;; ----------------------------------------

(printf "primitive mutable large tables\n")
(time
 (let loop ([j 1000])
   (define numbers (make-hashtable equal-hash-code equal?))
   (let loop ([l l])
     (if (null? l)
         (void)
         (begin
           (hashtable-set! numbers (car l) #t)
           (loop (cdr l)))))
   (unless (zero? j)
     (let loop ([v #f] [i 1000])
       (if (zero? i)
           v
           (loop (hashtable-ref numbers i #f) (sub1 i))))
     (loop (sub1 j)))))

(printf "primitive mutable small tables\n")
(time
 (let loop ([j 100000])
   (define numbers (make-hashtable equal-hash-code equal?))
   (let loop ([i 10])
     (if (zero? i)
         (void)
         (begin
           (hashtable-set! numbers i #t)
           (loop (sub1 i)))))
   (unless (zero? j)
     (let loop ([v #f] [i 10])
       (if (zero? i)
           v
           (loop (hashtable-ref numbers i #f) (sub1 i))))
     (loop (sub1 j)))))

;; ----------------------------------------

(printf "weak equal table\n")
(let ([ht (make-weak-hash)])
  (define evens
    (let loop ([i 1000])
      (define s (format "~a" i))
      (hash-set! ht s i)
      (cond
       [(zero? i) '()]
       [(even? i)
        (cons s (loop (sub1 i)))]
       [else (loop (sub1 i))])))
  (collect)
  (printf "~s\n" (hash-count ht))
  (hash-set! ht "300" 'three-hundred)
  (hash-remove! ht "302")
  (for-each (lambda (e)
              (define v (hash-ref ht (number->string (string->number e)) #f))
              (cond
               [(equal? e "302")
                (when v (error 'weak "present"))]
               [else
                (unless v
                  (error 'weak "missing ~s" e))
                (unless (equal? v (if (equal? e "300")
                                      'three-hundred
                                      (string->number e)))
                  (error 'weak "wrong value"))]))
            evens))

;; ----------------------------------------

(let loop ([i 1000])
  (unless (zero? i)
    (let ([l2 (list-tail (shuffle l) (quotient (length l) 2))])
      (define half-numbers
        (let loop ([ht (hasheqv)] [l l2])
          (if (null? l)
              ht
              (loop (hash-set ht (car l) #t) (cdr l)))))
      (unless (hash-keys-subset? half-numbers numbers)
        (error 'subset? "failed"))
      (loop (sub1 i)))))

;; ---------------------------------------- 

(printf "many tables\n")
(collect-garbage)
(define m1 (current-memory-use))
(define hts
  (time 
   (let loop ([i 0])
     (if (< i 100)
         (cons
          (let loop2 ([i 0])
            (if (< i 10000)
                (hash-set 
                 (loop2 (+ i 1))
                 (gensym)
                 (cons (random 100) (random 100)))
                (hasheq)))
          (loop (+ 1 i)))
         null))))
(collect-garbage)
(printf "~a\n" (- (current-memory-use) m1))

