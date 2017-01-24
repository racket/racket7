#lang racket/base
(require "regexp.rkt"
         "lazy-bytes.rkt"
         "interp.rkt")

(provide search-match)

;; ------------------------------------------------------------
;; The driver iterates through the input (unless the pattern is
;; anchored) to find a match

(define (search-match rx in pos start-pos end-pos state)
  (define matcher (rx:regexp-matcher rx))
  (define anchored? (rx:regexp-anchored? rx))
  (define must-string (rx:regexp-must-string rx))
  (cond
   [(not (check-must-string must-string in pos end-pos))
    (values #f #f)]
   [else
    (let loop ([pos pos])
      (cond
       [(and anchored? (not (= pos start-pos)))
        (values #f #f)]
       [else
        (define pos2 (interp matcher in pos start-pos end-pos state))
        (cond
         [pos2 (values pos pos2)]
         [(if (integer? end-pos)
              (pos . < . end-pos)
              (lazy-bytes-before-end? in pos end-pos))
          (define pos2 (add1 pos))
          (unless (bytes? in)
            (lazy-bytes-advance! in pos2 #f))
          (loop pos2)]
         [else (values #f #f)])]))]))

;; ------------------------------------------------------------------
;; Checking for a must string (before iterating though the input) can
;; speed up a match failure by avoiding backtracking:

(define (check-must-string must-string in pos end-pos)
  (cond
   [(not must-string) #t]
   [(not (bytes? in)) #t]
   [(= 1 (bytes-length must-string))
    (define mc (bytes-ref must-string 0))
    (for/or ([c (in-bytes in pos end-pos)])
      (= c mc))]
   [else
    (for/or ([i (in-range pos (- end-pos (sub1 (bytes-length must-string))))])
      (for/and ([c (in-bytes in i)]
                [mc (in-bytes must-string)])
        (= c mc)))]))

