
(define (exact-integer? n) (and (integer? n) (exact? n)))
(define (exact-nonnegative-integer? n) (and (exact-integer? n) (>= n 0)))
(define (exact-positive-integer? n) (and (exact-integer? n) (> n 0)))
(define (byte? n) (and (exact-integer? n) (>= n 0) (<= n 255)))

(define arithmetic-shift bitwise-arithmetic-shift)

(define (system-big-endian?)
  (eq? (native-endianness) (endianness big)))

(define integer->integer-bytes
  (case-lambda
   [(num size signed? big-endian? bstr start)
    (case size
      [(2)
       (if signed?
           (bytevector-s16-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little)))
           (bytevector-u16-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little))))]
      [(4)
       (if signed?
           (bytevector-s32-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little)))
           (bytevector-u32-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little))))]
      [(8)
       (if signed?
           (bytevector-s64-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little)))
           (bytevector-u64-set! bstr start num (if big-endian?
                                                   (endianness big)
                                                   (endianness little))))]
      [else
       (raise-argument-error 'integer->integer-bytes
                             "(or/c 2 4 8)" size)])]
   [(num size signed?)
    (integer->integer-bytes num size signed? (system-big-endian?)
                            (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size signed? big-endian?)
    (integer->integer-bytes num size signed? big-endian?
                            (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size signed? big-endian? bstr)
    (integer->integer-bytes num size signed? big-endian? bstr 0)]))

(define integer-bytes->integer
  (case-lambda
   [(bstr signed? big-endian? start end)
    (case (- end start)
      [(2)
       (if signed?
           (bytevector-s16-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u16-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [(4)
       (if signed?
           (bytevector-s32-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u32-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [(8)
       (if signed?
           (bytevector-s64-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little)))
           (bytevector-u64-ref bstr start (if big-endian?
                                              (endianness big)
                                              (endianness little))))]
      [else
       (raise-arguments-error 'integer-bytes->integer
                              "length is not 2, 4, or 8 bytes"
                              "length" (- end start))])]
   [(bstr signed?)
    (integer-bytes->integer bstr signed? (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr signed? big-endian?)
    (integer-bytes->integer bstr signed? big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr signed? big-endian? start)
    (integer-bytes->integer bstr signed? big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

(define real->floating-point-bytes
  (case-lambda
   [(num size big-endian? bstr start)
    (case size
      [(4)
       (bytevector-ieee-single-set! bstr start num (if big-endian?
                                                       (endianness big)
                                                       (endianness little)))]
      [(8)
       (bytevector-ieee-double-set! bstr start num (if big-endian?
                                                       (endianness big)
                                                       (endianness little)))]
      [else
       (raise-argument-error 'real->floating-point-bytes
                             "(or/c 4 8)" size)])]
   [(num size signed?)
    (real->floating-point-bytes num size (system-big-endian?)
                                (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size signed? big-endian?)
    (real->floating-point-bytes num size big-endian?
                                (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
   [(num size big-endian? bstr)
    (real->floating-point-bytes num size big-endian? bstr 0)]))

(define floating-point-bytes->real
  (case-lambda
   [(bstr big-endian? start end)
    (case (- end start)
      [(4)
       (bytevector-ieee-single-ref bstr start (if big-endian?
                                                  (endianness big)
                                                  (endianness little)))]
      [(8)
       (bytevector-ieee-double-ref bstr start (if big-endian?
                                                  (endianness big)
                                                  (endianness little)))]
      [else
       (raise-arguments-error 'floating-point-bytes->real
                              "length is not 4 or 8 bytes"
                              "length" (- end start))])]
   [(bstr)
    (floating-point-bytes->real bstr (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian?)
    (floating-point-bytes->real bstr big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
   [(bstr big-endian? start)
    (floating-point-bytes->real bstr big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

(define string->number
  (case-lambda
   [(s) (string->number s 10 #f 'decimal-as-inexact)]
   [(s radix) (string->number s radix #f 'decimal-as-inexact)]
   [(s radix mode) (string->number s radix mode 'decimal-as-inexact)]
   [(s radix mode decimal)
    ;; FIXME
    (cond
     [(equal? s "+nan.f") +nan.0]
     [else (chez:string->number s radix)])]))
