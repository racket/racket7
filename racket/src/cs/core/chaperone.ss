
(struct impersonator* (val))

(define (impersonate*-ref s? acc i)
  (if (impersonator*? i)
      (acc (impersonator*-val i))
      (acc i)))

(define (impersonate*-set! s? mut i v)
  (if (impersonator*? i)
      (let ([s (impersonator*-val i)])
        (if (s? s)
            (mut s v)
            (mut s v)))
      (mut i v)))
