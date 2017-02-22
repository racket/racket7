#lang racket/base

(provide utf-8-encode!)

;; Returns (values chars-used bytes-written (or/c 'complete 'aborts))
;; where 'aborts is the result when the result byte string doesn't
;; have enough room
(define (utf-8-encode! in-str in-start in-end
                       out-bstr out-start out-end)  ; `out-bstr` and `out-end` can be #f no bytes result needed
  ;; Iterate through the given string
  (let loop ([i in-start] [j out-start])
    (cond
     [(= i in-end)
      (values (- in-end in-start) (- j out-start) 'complete)]
     [else
      (define b (char->integer (string-ref in-str i)))
      (cond
       [(byte? b)
        (cond
         [(and out-end (= j out-end))
          (values (- i in-start) (- j out-start) 'aborts)]
         [else
          (when out-bstr (bytes-set! out-bstr j b))
          (loop (add1 i) (add1 j))])]
       [(b . < . #x7FF)
        (cond
         [(and out-end ((add1 j) . >= . out-end))
          (values (- i in-start) (- j out-start) 'aborts)]
         [else
          (when out-bstr
            (bytes-set! out-bstr j (bitwise-ior #b11000000 (arithmetic-shift b -6)))
            (bytes-set! out-bstr (add1 j) (bitwise-ior #b10000000 (bitwise-and b #b111111))))
          (loop (add1 i) (+ j 2))])]
       [(b . < . #xFFFF)
        (cond
         [(and out-end ((+ j 2) . >= . out-end))
          (values (- i in-start) (- j out-start) 'aborts)]
         [else
          (when out-bstr
            (bytes-set! out-bstr j (bitwise-ior #b11100000 (arithmetic-shift b -12)))
            (bytes-set! out-bstr (+ j 1) (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift b -6)
                                                                              #b111111)))
            (bytes-set! out-bstr (+ j 2) (bitwise-ior #b10000000 (bitwise-and b #b111111))))
          (loop (add1 i) (+ j 3))])]
       [else
        (cond
         [(and out-end ((+ j 3) . >= . out-end))
          (values (- i in-start) (- j out-start) 'aborts)]
         [else
          (when out-bstr
            (bytes-set! out-bstr j (bitwise-ior #b11110000 (arithmetic-shift b -18)))
            (bytes-set! out-bstr (+ j 1) (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift b -12)
                                                                              #b111111)))
            (bytes-set! out-bstr (+ j 2) (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift b -6)
                                                                              #b111111)))
            (bytes-set! out-bstr (+ j 3) (bitwise-ior #b10000000 (bitwise-and b #b111111))))
          (loop (add1 i) (+ j 4))])])])))
