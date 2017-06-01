(define/who string-copy!
  (case-lambda
   [(dest dest-start src)
    (check who mutable-string? :contract "(and/c string? (not/c immutable?))" dest)
    (#2%string-copy! src 0 dest dest-start
                     (if (string? src) (string-length src) 0))]
   [(dest dest-start src src-start)
    (check who mutable-string? :contract "(and/c string? (not/c immutable?))" dest)
    (#2%string-copy! src src-start dest dest-start
                     (if (and (string? src) (number? src-start))
                         (- (string-length src) src-start)
                         0))]
   [(dest dest-start src src-start src-end)
    (check who mutable-string? :contract "(and/c string? (not/c immutable?))" dest)
    (#2%string-copy! src src-start dest dest-start
                     (if (and (number? src-start) (number? src-end))
                         (- src-end src-start)
                         0))]))

(define substring
  (case-lambda
   [(s start) (#2%substring s start (if (string? s) (string-length s) 0))]
   [(s start end) (#2%substring s start end)]))
