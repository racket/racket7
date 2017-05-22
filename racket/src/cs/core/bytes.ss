(define bytes bytevector)
(define bytes? bytevector?)

(define bytes-length bytevector-length)
(define make-bytes make-bytevector)    
(define bytes->list bytevector->u8-list)
(define list->bytes u8-list->bytevector)
(define bytes-ref bytevector-u8-ref)
(define bytes-set! bytevector-u8-set!)
(define bytes->immutable-bytes bytevector->immutable-bytevector)

(define bytes-copy!
  (case-lambda
    [(dest d-start src)
     (bytes-copy! dest d-start src 0 (bytes-length src))]
    [(dest d-start src s-start)
     (bytes-copy! dest d-start src s-start (bytes-length src))]
    [(dest d-start src s-start s-end)
     (unless (mutable-bytevector? dest)
       (raise-argument-error 'bytes-set! "(and/c bytes? (not/c immutable?))" dest))
     (bytevector-copy! src s-start dest d-start (- s-end s-start))]))

(define (bytes-fill! bstr b)
  (unless (byte? b)
    (raise-argument-error 'bytes-fill! "byte?" b))
  (bytevector-fill! bstr b))

(define bytes-copy bytevector-copy)

(define bytes=? bytevector=?)

(define (bytes<? a b)
  (define alen (bytes-length a))
  (define blen (bytes-length b))
  (let loop ([i 0])
    (cond
     [(= i alen) (if (= i blen)
                     #f
                     #t)]
     [(= i blen) #f]
     [else
      (let ([va (bytes-ref a i)]
            [vb (bytes-ref b i)])
        (cond
         [(fx< va vb) #t]
         [(fx= va vb) (loop (fx1+ i))]
         [else #f]))])))

(define (bytes>? a b)
  (define alen (bytes-length a))
  (define blen (bytes-length b))
  (let loop ([i 0])
    (cond
     [(= i alen) #f]
     [(= i blen) #t]
     [else
      (let ([va (bytes-ref a i)]
            [vb (bytes-ref b i)])
        (cond
         [(fx> va vb) #t]
         [(fx= va vb) (loop (fx1+ i))]
         [else #f]))])))

(define(bytes>=? a b)
  (not (bytes<? a b)))

(define(bytes<=? a b)
  (not (bytes>? a b)))

(define bytes-append
  (case-lambda 
    [(a b)
     (define alen (bytevector-length a))
     (define blen (bytevector-length b))
     (define c (make-bytevector (+ alen blen)))
     (bytevector-copy! a 0 c 0 alen)
     (bytevector-copy! b 0 c alen blen)
     c]
    [(a) a]
    [() #vu8()]
    [args
     (define size (let loop ([args args])
                    (cond
                     [(null? args) 0]
                     [else (+ (bytevector-length (car args))
                              (loop (cdr args)))])))
     (define c (make-bytevector size))
     (let loop ([args args] [pos 0])
       (cond
        [(null? args) c]
        [else
         (let ([len (bytevector-length (car args))])
           (bytevector-copy! (car args) 0 c pos len)
           (loop (cdr args) (+ pos len)))]))]))

(define subbytes
  (case-lambda
    [(bstr start end)
     (define len (- end start))
     (define c (make-bytevector len))
     (bytevector-copy! bstr start c 0 len)
     c]
    [(bstr start)
     (subbytes bstr start (bytes-length bstr))]))
