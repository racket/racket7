
(define (exact-integer? n) (and (integer? n) (exact? n)))
(define (exact-nonnegative-integer? n) (and (exact-integer? n) (>= n 0)))
(define (exact-positive-integer? n) (and (exact-integer? n) (> n 0)))
(define (byte? n) (and (exact-integer? n) (>= n 0) (<= n 255)))

(define arithmetic-shift bitwise-arithmetic-shift)

;; ----------------------------------------

;; FIXME: change to MRG32k3a

(define-record pseudo-random-generator ())

(define random
  (case-lambda
    [() (random 1.0)]
    [(n)
     (unless (and (integer? n)
                  (exact? n)
                  (<= 1 n 4294967087))
       (raise-argument-error 'random "(integer-in 1 4294967087)" n))
     (chez:random n)]
    [(n)
     (cond
      [(pseudo-random-generator? n)
       (random 1.0)]
      [else
       (unless (and (integer? n)
                    (exact? n)
                    (<= 1 n 4294967087))
         (raise-argument-error 'random "(or/c (integer-in 1 4294967087) pseudo-random-generator?)" n))
       (chez:random n)])]
    [(n prg)
     (unless (and (integer? n)
                  (exact? n)
                  (<= 1 n 4294967087))
       (raise-argument-error 'random "(integer-in 1 4294967087)" n))
     (unless (pseudo-random-generator? prg)
       (raise-argument-error 'random "pseudo-random-generator?" prg))
     (chez:random n)]))
