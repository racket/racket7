#lang racket/base
(require "range.rkt"
         "lazy-bytes.rkt"
         "utf-8.rkt")

;; An AST is converted to a pile of matcher closures by "compile.rkt"
(provide interp
         
         done-m
         continue-m
         limit-m
         
         byte-tail-matcher
         byte-simple-matcher
         byte-matcher
         byte-matcher*
         
         bytes-tail-matcher
         bytes-simple-matcher
         bytes-matcher
         bytes-matcher*
         
         never-matcher
         
         any-tail-matcher
         any-simple-matcher
         any-matcher
         any-matcher*
         
         range-tail-matcher
         range-simple-matcher
         range-matcher
         range-matcher*
         
         start-matcher
         end-matcher
         line-start-matcher
         line-end-matcher
         word-boundary-matcher
         not-word-boundary-matcher
         
         alts-matcher
         
         repeat-matcher
         repeat-simple-many-matcher
         repeat-simple-many+simple-matcher
         repeat-simple-matcher
         repeat-simple+simple-matcher
         lazy-repeat-matcher
         lazy-repeat-simple-matcher

         group-push-matcher
         group-set-matcher
          
         reference-matcher
         reference-matcher/case-insensitive
         
         cut-matcher
         conditional/reference-matcher
         conditional/look-matcher
         lookahead-matcher
         lookbehind-matcher
         
         unicode-categories-matcher)

;; ----------------------------------------

;; Compilation produces a matcher function; see "match.rkt"
(define (interp m      ; the compiled matcher function
                s      ; input bytes or lazy-bytes
                pos    ; starting seach position, can be > `start`, must be < `limit`
                start  ; input start in the sense of `^`; don't read before this
                limit/end ; don't read past `limit`; `end` corresponds to `$` and can be < `limit`
                state) ; vector where group position-pair matches are installed
  ;; The search `pos` can be greater than `start` due to prefix bytes
  ;; passed to `regexp-match`.
  ;; The search `limit` and `end` start out the same, but `limit`
  ;; can be less than `end` for a lookbehind match.
  (m s pos start limit/end limit/end state null (lambda () #f)))

(define done-m (lambda (s pos start limit end state stack fail-k)
                 pos))
(define continue-m (lambda (s pos start limit end state stack fail-k)
                     ((car stack) pos fail-k)))
(define limit-m (lambda (s pos start limit end state stack fail-k)
                  (= pos limit)))


;; ----------------------------------------

(define-syntax-rule (define-plain+simple+tail (general-matcher simple-matcher tail-matcher arg ... next-m)
                      (lambda (s pos start limit end)
                        tst
                        next-pos))
  (begin
    ;; General mode for `next-m` that may backtrack
    (define (general-matcher arg ... next-m)
      (lambda (s pos start limit end state stack fail-k)
        (if tst
            (next-m s next-pos start limit end state stack fail-k)
            (fail-k))))
    ;; Simple mode when we don't need to use `fail-k`,
    ;; because `next-m` can't backtrack
    (define (simple-matcher arg ... next-m)
      (lambda (s pos start limit end state stack fail-k)
        (and tst
             (next-m s next-pos start limit end state stack fail-k))))
    ;; Tail mode when `next-m` is `done-m` (which not only
    ;; can't backtrack, but just returns the given posiion)
    (define (tail-matcher arg ...)
      (lambda (s pos start limit end state stack fail-k)
        (and tst
             next-pos)))))

;; An iterator performs a single match as many times as possible, up
;; to a specified max number of times, and it returns the position
;; and the number of items; this mode is used only when each match
;; has a fixed size
(define-syntax-rule (define-iterate (op-matcher* arg ...)
                      defn ...
                      (lambda (s pos2 start limit end)
                        #:size size
                        #:s-test s-tst
                        #:ls-test ls-tst))
  (define (op-matcher* arg ... max)
    defn ...
    (lambda (s pos start limit end)
      (if (bytes? s)
          (let ([limit (if max
                           (min limit (+ pos (* size max)))
                           limit)])
            (let loop ([pos2 pos] [n 0])
              (cond
               [(or ((+ pos2 size) . > . limit)
                    (not s-tst))
                (values pos2 n)]
               [else (loop (+ pos2 size) (add1 n))])))
          (let ([limit (and max (+ pos (* size max)))])
            (let loop ([pos2 pos] [n 0])
              (cond
               [(or (and limit ((+ pos2 size) . > . limit))
                    (not (lazy-bytes-before-end? s (+ pos2 (sub1 size)) limit))
                    (not ls-tst))
                (values pos2 n)]
               [else
                (loop (+ pos2 size) (add1 n))])))))))

;; ----------------------------------------
;; Single-byte matching

(define-plain+simple+tail (byte-matcher byte-simple-matcher byte-tail-matcher b next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and (pos . < . limit)
             (= b (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos limit)
             (= b (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (byte-matcher* b)
  (lambda (s pos start limit end)
    #:size 1
    #:s-test (= b (bytes-ref s pos))
    #:ls-test (= b (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Byte-string matching

(define-plain+simple+tail (bytes-matcher bytes-simple-matcher bytes-tail-matcher bstr len next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and ((+ pos len) . <= . limit)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [c2 (in-bytes s pos (+ pos len))])
               (= c1 c2)))
        (and (lazy-bytes-before-end? s (sub1 (+ pos len)) limit)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [i (in-naturals pos)])
               (define c2 (lazy-bytes-ref s i))
               (= c1 c2))))
    (+ pos len)))

(define-iterate (bytes-matcher* bstr)
  (define len (bytes-length bstr))
  (lambda (s pos start limit end)
    #:size len
    #:s-test (for/and ([c1 (in-bytes s 0 len)]
                       [c2 (in-bytes s pos (+ pos len))])
               (= c1 c2))
    #:ls-test (for/and ([c1 (in-bytes bstr 0 len)]
                        [i (in-naturals pos)])
                (define c2 (lazy-bytes-ref s i))
                (= c1 c2))))

;; ----------------------------------------
;; An always-fail pattern

(define (never-matcher)
  (lambda (s pos start limit end state stack fail-k)
    (fail-k)))

;; ----------------------------------------
;; Match any byte

(define-plain+simple+tail (any-matcher any-simple-matcher any-tail-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (pos . < . limit)
        (lazy-bytes-before-end? s pos limit))
    (add1 pos)))

(define-iterate (any-matcher*)
  (lambda (s pos start limit end)
    #:size 1
    #:s-test #t
    #:ls-test #t))

;; ----------------------------------------
;; Match any byte in a set

(define-plain+simple+tail (range-matcher range-simple-matcher range-tail-matcher rng next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and (pos . < . limit)
             (rng-in? rng (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos limit)
             (rng-in? rng (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (range-matcher* rng)
  (lambda (s pos start limit end)
    #:size 1
    #:s-test (rng-in? rng (bytes-ref s pos))
    #:ls-test (rng-in? rng (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Matches that don't consume any characters,
;; such as end-of-string or word-boundary

(define-syntax-rule (define-zero-width (op-matcher arg ... next-m)
                      (lambda (s pos start limit end)
                        tst))
  (define (op-matcher arg ... next-m)
    (lambda (s pos start limit end state stack fail-k)
      (if tst
          (next-m s pos start limit end state stack fail-k)
          (fail-k)))))

(define-zero-width (start-matcher next-m)
  (lambda (s pos start limit end)
    (= pos start)))

(define-zero-width (end-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (= pos end)
        (not (lazy-bytes-before-end? s pos end)))))

(define-zero-width (line-start-matcher next-m)
  (lambda (s pos start limit end)
    (or (= pos start)
        (= (char->integer #\newline)
           (if (bytes? s)
               (bytes-ref s (sub1 pos))
               (lazy-bytes-ref s (sub1 pos)))))))

(define-zero-width (line-end-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (or (= pos end)
            (= (char->integer #\newline) (bytes-ref s pos)))
        (or (not (lazy-bytes-before-end? s pos end))
            (= (char->integer #\newline)
               (lazy-bytes-ref s pos))))))

(define-zero-width (word-boundary-matcher next-m)
  (lambda (s pos start limit end)
    (word-boundary? s pos start limit end)))

(define-zero-width (not-word-boundary-matcher next-m)
  (lambda (s pos start limit end)
    (not (word-boundary? s pos start limit end))))

(define (word-boundary? s pos start limit end)
  (not (eq? (or (= pos start)
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s (sub1 pos))
                                     (lazy-bytes-ref s (sub1 pos))))))
            (or (if (bytes? s)
                    (= pos end)
                    (not (lazy-bytes-before-end? s pos end)))
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s pos)
                                     (lazy-bytes-ref s pos))))))))

(define (word-byte? c)
  (or (and (c . >= . (char->integer #\0)) (c . <= . (char->integer #\9)))
      (and (c . >= . (char->integer #\a)) (c . <= . (char->integer #\z)))
      (and (c . >= . (char->integer #\A)) (c . <= . (char->integer #\Z)))
      (= c (char->integer #\_))))

;; ----------------------------------------
;; Alternatives

(define (alts-matcher m1 m2)
  (lambda (s pos start limit end state stack fail-k)
    (m1 s pos start limit end state stack
        (lambda ()
          (m2 s pos start limit end state stack fail-k)))))

;; ----------------------------------------
;; Repeats, greedy (normal) and non-greedy,
;; in various optimized forms

(define (repeat-matcher r-m min max next-m)
  ;; The tail of `r-m` is set to `continue-m` instead
  ;; of `done-m`, so we can supply a success continuation
  ;; by pushing it onto the stack
  (lambda (s pos start limit end state stack fail-k)
    (let rloop ([pos pos] [n 0] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) fail-k))
                                stack))
        (r-m s pos start limit end state new-stack fail-k)]
       [(and max (= n max)) (next-m s pos start limit end state stack fail-k)]
       [else
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) fail-k))
                                stack))
        (r-m s pos start limit end state new-stack
             (lambda () (next-m s pos start limit end state stack fail-k)))]))))

(define r-stack (list (lambda (pos fail-k) pos)))

(define (repeat-simple-matcher r-m min max next-m)
  ;; The `r-m` matcher doesn't need bcktracking, so
  ;; we can supply a failure continuation that just
  ;; returns `#f` (which avoids allocating that
  ;; continuation)
  (lambda (s pos start limit end state stack fail-k)
    (let rloop ([pos pos] [n 0] [back-amt 0])
      (define pos2
        (if (or (not max) (n . < . max))
            (r-m s pos start limit end state r-stack (lambda () #f))
            #f))
      (if pos2
          (rloop pos2 (add1 n) (- pos2 pos))
          (let bloop ([pos pos] [n n])
            (cond
             [(n . < . min) (fail-k)]
             [else (next-m s pos start limit end state stack
                           (lambda () (bloop (- pos back-amt) (sub1 n))))]))))))

(define (repeat-simple+simple-matcher r-m min max next-m)
  ;; The `r-m` matcher doesn't need bcktracking, and neither does
  ;; `next-m`, so we can avoid yet more continuations.
  (lambda (s pos start limit end state stack fail-k)
    (let rloop ([pos pos] [n 0] [back-amt 0])
      (define pos2
        (if (or (not max) (n . < . max))
            (r-m s pos start limit end state r-stack (lambda () #f))
            #f))
      (if pos2
          (rloop pos2 (add1 n) (- pos2 pos))
          (let bloop ([pos pos] [n n])
            (cond
             [(n . < . min) (fail-k)]
             [else
              (define pos2 (next-m s pos start limit end state stack (lambda () #f)))
              (or pos2
                  (bloop (- pos back-amt) (sub1 n)))]))))))

(define (repeat-simple-many-matcher r-m* back-amt min max next-m)
  ;; Instead of `r-m`, we have a `r-m*` that finds as many matches as
  ;; possible (up to max) in one go
  (lambda (s pos start limit end state stack fail-k)
    (define-values (pos2 n) (r-m* s pos start limit end))
    (let bloop ([pos pos2] [n n])
      (cond
       [(n . < . min) (fail-k)]
       [else (next-m s pos start limit end state stack
                     (lambda () (bloop (- pos back-amt) (sub1 n))))]))))

(define (repeat-simple-many+simple-matcher r-m* back-amt min max next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define-values (pos2 n) (r-m* s pos start limit end))
    (let bloop ([pos pos2] [n n])
      (cond
       [(n . < . min) (fail-k)]
       [else
        (define pos2 (next-m s pos start limit end state stack (lambda () #f)))
        (or pos2
            (bloop (- pos back-amt) (sub1 n)))]))))

(define (lazy-repeat-matcher r-m min max next-m)
  ;; Like `repeat-matcher`: the tail of `r-m` is set to `continue-m`
  (lambda (s pos start limit end state stack fail-k)
    (let rloop ([pos pos] [n 0] [min min] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) min fail-k))
                                stack))
        (r-m s pos start limit end state new-stack fail-k)]
       [(and max (= n max))
        (next-m s pos start limit end state stack fail-k)]
       [else
        (next-m s pos start limit end state stack
                (lambda () (rloop pos n (add1 min) fail-k)))]))))

(define (lazy-repeat-simple-matcher r-m min max next-m)
  ;; Like `repeat-simple-matcher`: no backtracking `r-m`
  (lambda (s pos start limit end state stack fail-k)
    (let rloop ([pos pos] [n 0] [min min] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define pos2 (r-m s pos start limit end state stack (lambda () #f)))
        (if pos2
            (rloop pos2 (add1 n) min fail-k)
            (fail-k))]
       [(and max (= n max))
        (next-m s pos start limit end state stack fail-k)]
       [else
        (next-m s pos start limit end state stack
                (lambda () (rloop pos n (add1 min) fail-k)))]))))

;; ----------------------------------------
;; Recording and referencing group matches

(define (group-push-matcher n next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define new-stack (cons (cons pos (and state (vector-ref state n)))
                            stack))
    (next-m s pos start limit end state new-stack fail-k)))

(define (group-set-matcher n next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define old-pos+span (car stack))
    (define old-span (cdr old-pos+span))
    (when state
      (vector-set! state n (cons (car old-pos+span) pos)))
    (next-m s pos start limit end state (cdr stack)
            (lambda ()
              (when state (vector-set! state n old-span))
              (fail-k)))))

(define-syntax-rule (define-reference-matcher reference-matcher chyte=?)
  (define (reference-matcher n next-m)
    (lambda (s pos start limit end state stack fail-k)
      (define p (vector-ref state n))
      (cond
       [(not p) (fail-k)]
       [else
        (define len (- (cdr p) (car p)))
        (define matches?
          (if (bytes? s)
              (and ((+ pos len) . <= . limit)
                   (for/and ([c1 (in-bytes s (car p) (cdr p))]
                             [c2 (in-bytes s pos (+ pos len))])
                     (chyte=? c1 c2)))
              (and (lazy-bytes-before-end? s (sub1 (+ pos len)) limit)
                   (for/and ([j (in-range (car p) (cdr p))]
                             [i (in-naturals pos)])
                     (define c1 (lazy-bytes-ref s j))
                     (define c2 (lazy-bytes-ref s i))
                     (chyte=? c1 c2)))))
        (if matches?
            (next-m s (+ pos len) start limit end state stack fail-k)
            (fail-k))]))))

(define-reference-matcher reference-matcher =)

(define-reference-matcher reference-matcher/case-insensitive
  (lambda (c1 c2) (= (chyte-to-lower c1) (chyte-to-lower c2))))

(define (chyte-to-lower c)
  (if (and (c . >= . (char->integer #\A)) (c . <= . (char->integer #\Z)))
      (+ c (- (char->integer #\a) (char->integer #\A)))
      c))

;; ----------------------------------------
;; Lookahead, lookbehind, conditionals, and cut

(define (lookahead-matcher match? sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define old-state (save-groups state n-start num-n))
    (define pos2 (sub-m s pos start limit end state null (lambda () #f)))
    (cond
     [match?
      (if pos2
          (next-m s pos start limit end state stack
                  (make-restore-groups fail-k state old-state n-start num-n))
          (fail-k))]
     [pos2
      (restore-groups state old-state n-start num-n)
      (fail-k)]
     [else
      (next-m s pos start limit end state stack fail-k)])))

(define (lookbehind-matcher match? lb-min lb-max sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define lb-min-pos (max start (- pos lb-max)))
    (let loop ([lb-pos (- pos lb-min)])
      (cond
       [(lb-pos . < . lb-min-pos)
        (if match?
            (fail-k)
            (next-m s pos start limit end state stack fail-k))]
       [else
        (define old-state (save-groups state n-start num-n))
        (define pos2 (sub-m s lb-pos start pos end state null (lambda () #f)))
        (cond
         [match?
          (if pos2
              (next-m s pos start limit end state stack
                      (make-restore-groups fail-k state old-state n-start num-n))
              (loop (sub1 lb-pos)))]
         [pos2
          (restore-groups state old-state n-start num-n)
          (fail-k)]
         [else
          (next-m s pos start limit end state stack fail-k)])]))))

(define (conditional/reference-matcher n m1 m2)
  (lambda (s pos start limit end state stack fail-k)
    (if (vector-ref state n)
        (m1 s pos start limit end state stack fail-k)
        (m2 s pos start limit end state stack fail-k))))

(define (conditional/look-matcher tst-m m1 m2 n-start num-n)
  (lambda (s pos start limit end state stack fail-k)
    (define old-state (save-groups state n-start num-n))
    (define branch-fail-k (make-restore-groups fail-k state old-state n-start num-n))
    (if (tst-m s pos start limit end state null (lambda () #f))
        (m1 s pos start limit end state stack branch-fail-k)
        (m2 s pos start limit end state stack branch-fail-k))))

(define (cut-matcher sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack fail-k)
    (define old-state (save-groups state n-start num-n))
    (define pos2 (sub-m s pos start limit end state null (lambda () #f)))
    (if pos2
        (next-m s pos2 start limit end state stack
                (make-restore-groups fail-k state old-state n-start num-n))
        (fail-k))))


(define (save-groups state n-start num-n)
  (cond
   [(zero? num-n) #f]
   [(not state) #f]
   [else
    (define vec (make-vector num-n))
    (vector-copy! vec 0 state n-start (+ n-start num-n))
    vec]))

(define (restore-groups state old-state n-start num-n)
  (when old-state
    (vector-copy! state n-start old-state)))

(define (make-restore-groups fail-k state old-state n-start num-n)
  (cond
   [(not old-state) fail-k]
   [else (lambda ()
           (restore-groups state old-state n-start num-n)
           (fail-k))]))

;; ----------------------------------------
;; Unicode characters in UTF-8 encoding

(define (unicode-categories-matcher cats match? next-m)
  (lambda (s pos start limit end state stack fail-k)
    (let loop ([pos pos] [accum null])
      (define b
        (if (bytes? s)
            (and (pos . < . limit)
                 (bytes-ref s pos))
            (and (lazy-bytes-before-end? s pos limit)
                 (lazy-bytes-ref s pos))))
      (cond
       [(not b) (fail-k)]
       [else
        (define c (bytes->char/utf-8 b accum))
        (cond
         [(char? c)
          (if (eq? match?
                   (let ([c-cat (char-general-category c)])
                     (if (list? cats)
                         (for/or ([cat (in-list cats)])
                           (eq? cat c-cat))
                         (eq? cats c-cat))))
              (next-m s (add1 pos) start limit end state stack fail-k)
              (fail-k))]
         [(eq? c 'fail)
          (fail-k)]
         [else
          ;; c must be 'continue
          (loop (add1 pos) (cons b accum))])]))))
