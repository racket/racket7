(define/who (bytes . args)
  ;; `bytevector` allows negative numbers that fit in a byte,
  ;; but `bytes` does not
  (for-each (lambda (arg)
              (check who byte? arg))
            args)
  (apply #2%bytevector args))

(define/who (shared-bytes . args)
  (for-each (lambda (arg)
              (check who byte? arg))
            args)
  (apply #2%bytevector args))

(define bytes? #2%bytevector?)

(define bytes-length #2%bytevector-length)

(define/who make-bytes
  (case-lambda
   [(n) (#2%make-bytevector n)]
   [(n b)
    (check who exact-nonnegative-integer? n)
    (check who byte? b)
    (#2%make-bytevector n b)]))

(define/who make-shared-bytes
  (case-lambda
   [(n) (#2%make-bytevector n)]
   [(n b)
    (check who exact-nonnegative-integer? n)
    (check who byte? b)
    (#2%make-bytevector n b)]))

(define/who (list->bytes lst)
  (check who
         :test (and (list? lst) (for-each byte? lst))
         :contract "(listof byte?)"
         lst)
  (u8-list->bytevector lst))

(define bytes->list #2%bytevector->u8-list)

(define bytes-ref #2%bytevector-u8-ref)
(define bytes-set! #2%bytevector-u8-set!)
(define bytes->immutable-bytes #2%bytevector->immutable-bytevector)

(define/who bytes-copy!
  (case-lambda
    [(dest d-start src)
     (bytes-copy! dest d-start src 0 (bytes-length src))]
    [(dest d-start src s-start)
     (bytes-copy! dest d-start src s-start (bytes-length src))]
    [(dest d-start src s-start s-end)
     (check who mutable-bytevector? :contract "(and/c bytes? (not/c immutable?))" dest)
     (bytevector-copy! src s-start dest d-start (- s-end s-start))]))

(define/who (bytes-fill! bstr b)
  (check who bytes? bstr)
  (check who byte? b)
  (bytevector-fill! bstr b))

(define bytes-copy #2%bytevector-copy)

(define bytes=? bytevector=?)

(define (do-bytes<? a b)
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

(define (do-bytes>? a b)
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

(define/who (bytes<? a b)
  (check who bytes? a)
  (check who bytes? b)
  (do-bytes<? a b))

(define/who (bytes>? a b)
  (check who bytes? a)
  (check who bytes? b)
  (do-bytes>? a b))

(define/who (bytes>=? a b)
  (check who bytes? a)
  (check who bytes? b)
  (not (do-bytes<? a b)))

(define/who (bytes<=? a b)
  (check who bytes? a)
  (check who bytes? b)
  (not (do-bytes>? a b)))

(define/who bytes-append
  (case-lambda 
   [(a b)
    (check who bytes? a)
    (check who bytes? b)
    (let ([alen (bytevector-length a)]
          [blen (bytevector-length b)])
      (let ([c (make-bytevector (+ alen blen))])
        (bytevector-copy! a 0 c 0 alen)
        (bytevector-copy! b 0 c alen blen)
        c))]
   [(a)
    (check who bytes? a)
    a]
   [() #vu8()]
   [args
    (let* ([size (let loop ([args args])
                   (cond
                    [(null? args) 0]
                    [else (+ (bytevector-length (car args))
                             (loop (cdr args)))]))]
           [c (make-bytevector size)])
      (let loop ([args args] [pos 0])
        (cond
         [(null? args) c]
         [else
          (let ([len (bytevector-length (car args))])
            (bytevector-copy! (car args) 0 c pos len)
            (loop (cdr args) (+ pos len)))])))]))

(define/who subbytes
  (case-lambda
   [(bstr start end)
    (check who bytes? bstr)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (let ([len (bytes-length bstr)])
      (unless (<= start len)
        (raise-range-error who "byte string" "" start bstr 0 len #f))
      (unless (<= start end len)
        (raise-range-error who "byte string" "" end bstr 0 len start))
      (let* ([len (- end start)]
             [c (make-bytevector len)])
        (bytevector-copy! bstr start c 0 len)
        c))]
   [(bstr start)
    (subbytes bstr start (bytes-length bstr))]))
