(define vector-copy!
  (case-lambda
   [(dest dest-start src)
    (vector-copy! dest dest-start src 0
                  (if (vector? src) (vector-length src) 0))]
   [(src src-start dest dest-start)
    (vector-copy! dest dest-start src src-start
                  (if (vector? src) (vector-length src) 0))]
   [(dest dest-start src src-start src-end)
    (unless (mutable-vector? dest)
      (raise-argument-error 'vector-copy! "(and/c vector? (not/c immutable?))" dest))
    (let loop ([i (- src-end src-start)])
      (unless (zero? i)
        (let ([i (sub1 i)])
          (vector-set! dest (+ dest-start i) (vector-ref src (+ src-start i)))
          (loop i))))]))

(define (vector-immutable . args)
  (let ([vec (apply vector args)])
    (#%$vector-set-immutable! vec)
    vec))

(define vector->values
  (case-lambda
    [(vec)
     (let ([len (vector-length vec)])
       (cond
        [(fx= len 0) (values)]
        [(fx= len 1) (vector-ref vec 0)]
        [(fx= len 2) (values (vector-ref vec 0) (vector-ref vec 1))]
        [(fx= len 3) (values (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2))]
        [else (chez:apply values (vector->list vec))]))]
    [(vec start)
     (vector->values vec start (vector-length vec))]
    [(vec start end)
     ;; FIXME: check range
     (chez:apply values
                 (let loop ([start start])
                   (cond
                    [(fx= start end) null]
                    [else (cons (vector-ref vec start)
                                (loop (fx1+ start)))])))]))
