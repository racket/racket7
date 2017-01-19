#lang racket/base
(require "ast.rkt"
         "range.rkt"
         "lazy-bytes.rkt")

;; A standard regexp interpreter based on success and failure
;; continuations, but curried to define a closure compiler.

;; If a regular (sub)expression doesn't need backtracking, then the
;; compiler uses a protocol that avoids allocating extra success
;; continuations, where a success is represented by a non-#f result
;; for the advanced match position.

(provide compile)

(define (compile rx)
  (cond
   [(needs-backtrack? rx)
    (define m (compile/bt rx))
    (lambda (s pos start end state)
      (m s pos start end state
         (lambda (pos fail-k) pos)
         (lambda () #f)))]
   [(exact-integer? rx)
    (lambda (s pos start end state)
      (if (bytes? s)
          (and (pos . < . end)
               (= rx (bytes-ref s pos))
               (add1 pos))
          (and (lazy-bytes-before-end? s pos end)
               (= rx (lazy-bytes-ref s pos))
               (add1 pos))))]
   [(bytes? rx)
    (define len (bytes-length rx))
    (lambda (s pos start end state)
      (if (bytes? s)
          (and ((+ pos len) . <= . end)
               (for/and ([c1 (in-bytes rx 0 len)]
                         [c2 (in-bytes s pos (+ pos len))])
                 (= c1 c2))
               (+ pos len))
          (and (lazy-bytes-before-end? s (+ pos len) end)
               (for/and ([c1 (in-bytes rx 0 len)]
                         [i (in-naturals pos)])
                 (define c2 (lazy-bytes-ref s i))
                 (= c1 c2))
               (+ pos len))))]
   [(eq? rx rx:empty)
    (lambda (s pos start end state)
      pos)]
   [(eq? rx rx:never)
    (lambda (s pos start end state)
      #f)]
   [(eq? rx rx:any)
    (lambda (s pos start end state)
      (and (if (bytes? s)
               (pos . < . end)
               (lazy-bytes-before-end? s pos end))
           (add1 pos)))]
   [(rx:range? rx)
    (define range (rx:range-range rx))
    (lambda (s pos start end state)
      (if (bytes? s)
          (and (pos . < . end)
               (range-in? range (bytes-ref s pos))
               (add1 pos))
          (and (lazy-bytes-before-end? s pos end)
               (range-in? range (lazy-bytes-ref s pos))
               (add1 pos))))]
   [(eq? rx rx:start)
    (lambda (s pos start end state)
      (and (= pos start)
           pos))]
   [(eq? rx rx:end)
    (lambda (s pos start end state)
      (and (if (bytes? s)
               (= pos end)
               (not (lazy-bytes-before-end? s pos end)))
           pos))]
   [(eq? rx rx:line-start)
    (lambda (s pos start end state)
      (and (or (= pos start)
               (= (char->integer #\newline)
                  (if (bytes? s)
                      (bytes-ref s (sub1 pos))
                      (lazy-bytes-ref s (sub1 pos)))))
           pos))]
   [(eq? rx rx:line-end)
    (lambda (s pos start end state)
      (and (or (if (bytes? s)
                   (= pos end)
                   (not (lazy-bytes-before-end? s pos end)))
               (= (char->integer #\newline)
                  (if (bytes? s)
                      (bytes-ref s pos)
                      (lazy-bytes-ref s pos))))
           pos))]
   [(eq? rx rx:word-boundary)
    (lambda (s pos start end state)
      (and (word-boundary? s pos start end)
           pos))]
   [(eq? rx rx:not-word-boundary)
    (lambda (s pos start end state)
      (and (not (word-boundary? s pos start end))
           pos))]
   [(rx:sequence? rx)
    (compile-sequence (rx:sequence-rxs rx))]
   [else (error 'compile "internal error: unrecognized ~s" rx)]))

(define (compile-sequence l)
  (let loop ([l l])
    (cond
     [(null? l)
      (lambda (s pos start end state)
        pos)]
     [(null? (cdr l))
      (compile (car l))]
     [else
      (define m1 (compile (car l)))
      (define m2 (loop (cdr l)))
      (lambda (s pos start end state)
        (define pos2 (m1 s pos start end state))
        (and pos2
             (m2 s pos2 start end state)))])))

(define (compile/bt rx)
  (cond
   [(not (needs-backtrack? rx))
    (define m (compile rx))
    (lambda (s pos start end state success-k fail-k)
      (define pos2 (m s pos start end state))
      (if pos2
          (success-k pos2 fail-k)
          (fail-k)))]
   [(rx:alts? rx)
    (cond
     [(and (needs-backtrack? (rx:alts-rx1 rx))
           (needs-backtrack? (rx:alts-rx2 rx)))
      ;; General backtracking for both alternatives
      (define m1 (compile/bt (rx:alts-rx1 rx)))
      (define m2 (compile/bt (rx:alts-rx2 rx)))
      (lambda (s pos start end state success-k fail-k)
        (define ((next-k fail-k))
          (m2 s pos start end state success-k fail-k))
        (m1 s pos start end state
            (lambda (pos fail-k)
              (success-k pos (next-k fail-k)))
            (next-k fail-k)))]
     [(needs-backtrack? (rx:alts-rx1 rx))
      ;; Don't need success and fail for second
      (define m1 (compile/bt (rx:alts-rx1 rx)))
      (define m2 (compile (rx:alts-rx2 rx)))
      (lambda (s pos start end state success-k fail-k)
        (define ((next-k fail-k))
          (define pos2 (m2 s pos start end state))
          (if pos2
              (success-k pos2 fail-k)
              (fail-k)))
        (m1 s pos start end state
            (lambda (pos fail-k)
              (success-k pos (next-k fail-k)))
            (next-k fail-k)))]
     [(needs-backtrack? (rx:alts-rx2 rx))
      ;; Don't need success and fail for first
      (define m1 (compile (rx:alts-rx1 rx)))
      (define m2 (compile/bt (rx:alts-rx2 rx)))
      (lambda (s pos start end state success-k fail-k)
        (define pos2 (m1 s pos start end state))
        (if pos2
            (success-k pos2
                       (lambda ()
                         (m2 s pos start end state success-k fail-k)))
            (m2 s pos start end state success-k fail-k)))]
     [else
      ;; Don't need success and fail for either
      (define m1 (compile (rx:alts-rx1 rx)))
      (define m2 (compile (rx:alts-rx2 rx)))
      (lambda (s pos start end state success-k fail-k)
        (define pos2 (m1 s pos start end state))
        (if pos2
            (success-k pos2
                       (lambda ()
                         (define pos2 (m2 s pos start end state))
                         (if pos2
                             (success-k pos2 fail-k)
                             (fail-k))))
            (let ([pos2 (m2 s pos start end state)])
              (if pos2
                  (success-k pos2 fail-k)
                  (fail-k)))))])]
   [(rx:sequence? rx)
    ;; Some part of the sequence needs backtracking, but maybe
    ;; there's a tail that doesn't.
    (define backtrack-prefix (count-backtrack-prefix (rx:sequence-rxs rx)))
    (let loop ([l (rx:sequence-rxs rx)] [backtrack-prefix backtrack-prefix])
      (cond
       [(null? l)
        ;; We should have optimized away an empty sequence
        (error 'compile/bt "internal error")]
       [(null? (cdr l)) (compile/bt (car l))]
       [(zero? backtrack-prefix)
        ;; The rest of the sequence doesn't need backtracking:
        (define m (compile-sequence l))
        (lambda (s pos start end state success-k fail-k)
          (define pos2 (m s pos start end state))
          (if pos2
              (success-k pos2 fail-k)
              (fail-k)))]
       [(not (needs-backtrack? (car l)))
        ;; The first element can be compiled as non-backtracking
        (define m1 (compile (car l)))
        (define m2 (loop (cdr l) (sub1 backtrack-prefix)))
        (lambda (s pos start end state success-k fail-k)
          (define pos2 (m1 s pos start end state))
          (if pos2
              (m2 s pos2 start end state success-k fail-k)
              (fail-k)))]
       [(and (rx:repeat? (car l))
             (not (needs-backtrack? (rx:repeat-rx (car l))))
             (zero? (sub1 backtrack-prefix)))
        ;; It's worth manually inling the immediate continuation
        ;; of the repeat in this case, since it's statically
        ;; known and doesn't need its own backtracking:
        (define m2 (compile-sequence (cdr l)))
        (compile-repeat (car l) m2)]
       [else
        ;; General case: backtracking head and tail
        (define m1 (compile/bt (car l)))
        (define m2 (loop (cdr l) (sub1 backtrack-prefix)))
        (lambda (s pos start end state success-k fail-k)
          (m1 s pos start end state
              (lambda (pos fail-k)
                (m2 s pos start end state success-k fail-k))
              fail-k))]))]
   [(rx:repeat? rx)
    (compile-repeat rx #f)]
   [(rx:maybe? rx)
    (cond
     [(not (needs-backtrack? (rx:maybe-rx rx)))
      ;; pattern to repeat is simple enough that we don't need to
      ;; create success & fail continuations for it
      (define r-rx (compile (rx:maybe-rx rx)))
      (cond
       [(rx:maybe-non-greedy? rx)
        (lambda (s pos start end state success-k fail-k)
          (success-k pos (lambda ()
                           (define pos2 (r-rx s pos start end state))
                           (if pos2
                               (success-k pos2 fail-k)
                               (fail-k)))))]
       [else
        (lambda (s pos start end state success-k fail-k)
          (define pos2 (r-rx s pos start end state))
          (if pos2
              (success-k pos2 (lambda ()
                                (success-k pos fail-k)))
              (success-k pos fail-k)))])]
     [else
      ;; General backtracking
      (define r-rx (compile/bt (rx:maybe-rx rx)))
      (cond
       [(rx:maybe-non-greedy? rx)
        (lambda (s pos start end state success-k fail-k)
          (success-k pos (lambda ()
                           (r-rx s pos start end state
                                 (lambda (pos2 fail-k)
                                   (success-k pos2 fail-k))
                                 fail-k))))]
       [else
        (lambda (s pos start end state success-k fail-k)
          (r-rx s pos start end state
                (lambda (pos2 fail-k)
                  (success-k pos2 (lambda ()
                                    (success-k pos fail-k))))
                (lambda ()
                  (success-k pos fail-k))))])])]
   [(rx:group? rx)
    (define n (rx:group-number rx))
    (define r-rx (compile/bt (rx:group-rx rx)))
    (lambda (s pos start end state success-k fail-k)
      (define old-span (and state (vector-ref state n)))
      (r-rx s pos start end state
            (lambda (m-pos fail-k)
              (when state
                (vector-set! state n (cons pos m-pos)))
              (success-k m-pos fail-k))
            (lambda ()
              (when state
                (vector-set! state n old-span))
              (fail-k))))]
   [else (error 'compile/bt "internal error: unrecognized ~s" rx)]))

;; If `next-m` is not #f, then it's a compiled. non-backtracking form.
;; The `next-m` argument will be non-#f only when `(rx:repeat-rx rx)`
;; also doesn't need backtracking, just to limit the number of cases
;; that we have to cover here.
(define (compile-repeat rx next-m)
  (define min (rx:repeat-min rx))
  (define max (rx:repeat-max rx))
  (cond
   [(not (needs-backtrack? (rx:repeat-rx rx)))
    ;; Pattern to repeat is simple enough that we don't need to
    ;; create success & fail continuations for it
    (define r-rx (compile (rx:repeat-rx rx)))
    (cond
     [(rx:repeat-non-greedy? rx)
      (cond
       [next-m
        ;; Non-greedy, non-backtracking element, known next step ---------
        (lambda (s pos start end state success-k fail-k)
          (let loop ([pos pos] [n 0])
            (cond
             [(n . < . min)
              (define pos2 (r-rx s pos start end state))
              (if pos2
                  (loop pos2 (add1 n))
                  (fail-k))]
             [(= n max)
              (define pos2 (next-m s pos start end state))
              (if pos2
                  (success-k pos2 fail-k)
                  (fail-k))]
             [else
              (define pos2 (next-m s pos start end state))
              (if pos2
                  (success-k pos2 (lambda () (loop pos (add1 n))))
                  (loop pos (add1 n)))])))]
       [else
        ;; Non-greedy, non-backtracking element, no extra step ---------
        (lambda (s pos start end state success-k fail-k)
          (let loop ([pos pos] [n 0])
            (cond
             [(n . < . min)
              (define pos2 (r-rx s pos start end state))
              (if pos2
                  (loop pos2 (add1 n))
                  (fail-k))]
             [(= n max) (success-k pos fail-k)]
             [else
              (success-k pos (lambda () (loop pos (add1 n))))])))])]
     [else
      (cond
       [next-m
        ;; Greedy (normal), non-backtracking element, known next step ---------
        (lambda (s pos start end state success-k fail-k)
          (let loop ([pos pos] [n 0] [back-amt 0])
            (cond
             [(n . < . max)
              (define pos2 (r-rx s pos start end state))
              (if pos2
                  (loop pos2 (add1 n) (- pos2 pos))
                  (let backup ([n n] [pos pos])
                    (cond
                     [(n . >= . min)
                      (define pos2 (next-m s pos start end state))
                      (if pos2
                          (success-k pos2
                                     (lambda () (backup (sub1 n) (- pos back-amt))))
                          (backup (sub1 n) (- pos back-amt)))]
                     [else (fail-k)])))]
             [else
              (success-k pos fail-k)])))]
       [else
        ;; Greedy (normal), non-backtracking element, no extra step ---------
        (lambda (s pos start end state success-k fail-k)
          (let loop ([pos pos] [n 0] [back-amt 0])
            (cond
             [(n . < . max)
              (define pos2 (r-rx s pos start end state))
              (if pos2
                  (loop pos2 (add1 n) (- pos2 pos))
                  (let backup ([n n] [pos pos])
                    (cond
                     [(n . > . min)
                      (success-k pos (lambda () (backup (sub1 n) (- pos back-amt))))]
                     [else (fail-k)])))]
             [else
              (success-k pos fail-k)])))])])]
   [else
    ;; Non-greedy, backtracking element ---------
    (define r-rx (compile/bt (rx:repeat-rx rx)))
    (cond
     [(rx:repeat-non-greedy? rx)
      (lambda (s pos start end state success-k fail-k)
        (let loop ([pos pos] [n 0] [fail-k fail-k])
          (cond
           [(n . < . min)
            (r-rx s pos start end state
                  (lambda (pos fail-k) (loop pos (add1 n) fail-k))
                  fail-k)]
           [(= n max) (success-k pos fail-k)]
           [else
            (success-k pos (lambda () (loop pos (add1 n) fail-k)))])))]
     [else
      ;; Greedy (normal), backtracking element --------------------
      (lambda (s pos start end state success-k fail-k)
        (let loop ([pos pos] [n 0] [fail-k fail-k])
          (cond
           [(n . < . max)
            (r-rx s pos start end state
                  (lambda (pos fail-k) (loop pos (add1 n) fail-k))
                  (lambda ()
                    (cond
                     [(n . > . min)
                      (success-k pos fail-k)]
                     [else (fail-k)])))]
           [else
            (success-k pos fail-k)])))])]))

(define (word-boundary? s pos start end)
  (not (eq? (or (= pos start)
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s (sub1 pos))
                                     (lazy-bytes-ref s (sub1 pos))))))
            (or (= pos end)
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s pos)
                                     (lazy-bytes-ref s pos))))))))

(define (word-byte? c)
  (or (and (c . >= . (char->integer #\a)) (c . <= . (char->integer #\z)))
      (and (c . >= . (char->integer #\A)) (c . <= . (char->integer #\Z)))
      (= c (char->integer #\_))))

;; Determine the length of the prefix of `l` that needs backtracking:
(define (count-backtrack-prefix l)
  (let loop ([l l] [total 0] [non-bt 0])
    (cond
     [(null? l) (- total non-bt)]
     [(needs-backtrack? (car l))
      (loop (cdr l) (add1 total) 0)]
     [else
      (loop (cdr l) (add1 total) (add1 non-bt))])))
