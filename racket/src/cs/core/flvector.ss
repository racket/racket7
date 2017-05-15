
(define-record-type (flvector create-flvector flvector?)
  (fields bstr))

(define (do-flvector who xs)
  (let ([bstr (make-bytevector (* 8 (length xs)))])
    (let loop ([xs xs] [i 0])
      (unless (null? xs)
        (let ([x (car xs)])
          (unless (flonum? x)
            (raise-argument-error who "flonum?" x))
          (bytevector-ieee-double-set! bstr i x (native-endianness))
          (loop (cdr xs) (fx+ i 8)))))
    (create-flvector bstr)))

(define new-flvector
  (let ([flvector
         (lambda xs
           (do-flvector 'flvector xs))])
    flvector))

(define (do-make-flvector who size init)
  (unless (exact-nonnegative-integer? size)
    (raise-argument-error who "exact-nonnegative-integer?" size))
  (cond
   [(eqv? init 0.0)
    ;; 0-fill bytevector => 0.0-fill flvector
    (create-flvector (make-bytevector (bitwise-arithmetic-shift-left size 3) 0))]
   [else
    (unless (flonum? init)
      (raise-argument-error who "flonum?" init))
    (let* ([bsize (* 8 size)]
           [bstr (make-bytevector bsize)])
      (let loop ([i 0])
        (unless (= i bsize)
          (bytevector-ieee-double-set! bstr i init (native-endianness))
          (loop (fx+ i 8)))))]))

(define make-flvector
  (case-lambda
   [(size) (make-flvector size 0.0)]
   [(size init) (do-make-flvector 'make-flvector size init)]))

(define (flvector-length flvec)
  (unless (flvector? flvec)
    (raise-argument-error 'flvector-length flvector? flvec))
  (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3))

(define (unsafe-flvector-length flvec)
  (#3%fxsll (#3%bytevector-length (flvector-bstr flvec)) 3))

(define (flvector-ref flvec pos)
  (unless (flvector? flvec)
    (raise-argument-error 'flvector-ref flvector? flvec))
  (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
    (unless (exact-nonnegative-integer? pos)
      (raise-argument-error 'flvector-ref "exact-nonnegative-integer?" pos))
    (unless (and (>= pos 0)
                 (< pos len))
      (raise-range-error 'flvector-ref "flvector" ""  pos flvec 0 len)))
  (bytevector-ieee-double-ref (flvector-bstr flvec)
                              (bitwise-arithmetic-shift-left pos 3)
                              (native-endianness)))

(define (unsafe-flvector-ref flvec pos)
  (#3%bytevector-ieee-double-ref (flvector-bstr flvec)
                                 (#3%fxsll pos 3)
                                 (native-endianness)))

(define (flvector-set! flvec pos val)
  (unless (flvector? flvec)
    (raise-argument-error 'flvector-set! flvector? flvec))
  (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
    (unless (exact-nonnegative-integer? pos)
      (raise-argument-error 'flvector-set! "exact-nonnegative-integer?" pos))
    (unless (and (>= pos 0)
                 (< pos len))
      (raise-range-error 'flvector-set! "flvector" ""  pos flvec 0 len)))
  (unless (flonum? val)
    (raise-argument-error 'flvector-set! "flonum?" val))
  (bytevector-ieee-double-set! (flvector-bstr flvec)
                               (bitwise-arithmetic-shift-left pos 3)
                               val
                               (native-endianness)))

(define (unsafe-flvector-set! flvec pos val)
  (#3%bytevector-ieee-double-set! (flvector-bstr flvec)
                                  (#3%fxsll pos 3)
                                  (native-endianness)
                                  val))

(define flvector-copy
  (case-lambda
   [(flvec) (flvector-copy flvec 0 (flvector-length flvec))]
   [(flvec start) (flvector-copy flvec start (flvector-length flvec))]
   [(flvec start end)
    (unless (flvector? flvec)
      (raise-argument-error 'flvector-copy flvector? flvec))
    (let ([len (bitwise-arithmetic-shift-right (bytevector-length (flvector-bstr flvec)) 3)])
      (unless (exact-nonnegative-integer? start)
        (raise-argument-error 'flvector-set! "exact-nonnegative-integer?" start))
      (unless (exact-nonnegative-integer? end)
        (raise-argument-error 'flvector-set! "exact-nonnegative-integer?" end))
      (unless (and (>= start 0)
                   (< start len))
        (raise-range-error 'flvector-set! "flvector" ""  start flvec 0 len))
      (unless (and (>= end start)
                   (< end len))
        (raise-range-error 'flvector-set! "flvector" ""  end flvec 0 len start))
      (let* ([new-len (bitwise-arithmetic-shift-left (- end start) 3)]
             [bstr (make-bytevector new-len)])
        (bytes-copy! bstr 0 (flvector-bstr flvec) (bitwise-arithmetic-shift-left start 3) new-len)
        (create-flvector bstr)))]))

(define (shared-flvector . xs)
  (do-flvector 'shared-flvector xs))

(define make-shared-flvector
  (case-lambda
   [(size) (make-shared-flvector size 0.0)]
   [(size init) (do-make-flvector 'make-shared-flvector size init)]))
