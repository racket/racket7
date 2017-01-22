#lang racket/base
(require "ast.rkt"
         "range.rkt"
         "lazy-bytes.rkt"
         "utf-8.rkt")

;; Compile to a Spencer-style interpretation of a regular expression,
;; where sequences are implemented by record chaining and backtracking
;; is implemented by a failure continuation plus the implicit
;; continuation, which mostly serves as the success continuation. A
;; stack of success continuations is used as needed or repetition
;; patterns. Meanwhile, further specializing on simple patterns that
;; don't need backtracking avoids many continuation allocations and/or
;; takes more advantage of the implicit continuation.

;; Spenser's implementation in C dispatches on records, but we compile
;; to closures, instead. A function like `BYTE` allocates a closure to
;; implement byte matching. Matcher-creation functions usually take a
;; closure to use as the next step, so the closure tree is built
;; bottom-up.

(provide compile
         interp)

(define (interp m s pos start end state)
  (m s pos start end state null (lambda () #f)))

(define done-m (lambda (s pos start end state stack fail-k)
                 pos))
(define continue-m (lambda (s pos start end state stack fail-k)
                     ((car stack) pos fail-k)))

(define (compile rx)
  (let compile ([rx rx] [next-m done-m] [next-backtracks? #f])
    (define-syntax-rule (mode-cond
                         #:tail tail
                         #:simple simple
                         #:general general)
      (cond
       [next-backtracks? general]
       [(eq? next-m done-m) tail]
       [else simple]))
    (cond
     [(exact-integer? rx)
      (mode-cond
       #:tail (BYTE-TAIL rx)
       #:simple (BYTE-SIMPLE rx next-m)
       #:general (BYTE rx next-m))]
     [(bytes? rx)
      (define len (bytes-length rx))
      (mode-cond
       #:tail (BYTES-TAIL rx len)
       #:simple (BYTES-SIMPLE rx len next-m)
       #:general (BYTES rx len next-m))]
     [(eq? rx rx:empty)
      next-m]
     [(eq? rx rx:never)
      (NEVER)]
     [(eq? rx rx:any)
      (mode-cond
       #:tail (ANY-TAIL)
       #:simple (ANY-SIMPLE next-m)
       #:general (ANY next-m))]
     [(rx:range? rx)
      (define rng (compile-range (rx:range-range rx)))
      (mode-cond
       #:tail (RANGE-TAIL rng)
       #:simple (RANGE-SIMPLE rng next-m)
       #:general (RANGE rng next-m))]
     [(eq? rx rx:start)
      (START next-m)]
     [(eq? rx rx:end)
      (END next-m)]
     [(eq? rx rx:line-start)
      (LINE-START next-m)]
     [(eq? rx rx:line-end)
      (LINE-END next-m)]
     [(eq? rx rx:word-boundary)
      (WORD-BOUNDARY next-m)]
     [(eq? rx rx:not-word-boundary)
      (NOT-WORD-BOUNDARY next-m)]
     [(rx:sequence? rx)
      (define rxs (rx:sequence-rxs rx))
      (define backtracks (if next-backtracks?
                             0
                             (count-backtrack-prefix rxs)))
      (let loop ([rxs rxs] [backtracks backtracks])
        (cond
         [(null? rxs) next-m]
         [else
          (define rest-node (loop (cdr rxs) (sub1 backtracks)))
          (compile (car rxs) rest-node (or next-backtracks? (backtracks . > . 1)))]))]
     [(rx:alts? rx)
      ;; Specializations for non-backtracking subforms might be useful here
      (define m1 (compile (rx:alts-rx1 rx) next-m next-backtracks?))
      (define m2 (compile (rx:alts-rx2 rx) next-m next-backtracks?))
      (ALTS m1 m2)]
     [(rx:maybe? rx)
      (if (rx:maybe-non-greedy? rx)
          (ALTS next-m
                (compile (rx:maybe-rx rx) next-m next-backtracks?))
          (ALTS (compile (rx:maybe-rx rx) next-m #t)
                next-m))]
     [(rx:repeat? rx)
      (define simple? (not (needs-backtrack? (rx:repeat-rx rx))))
      (define min (rx:repeat-min rx))
      (define max (let ([n (rx:repeat-max rx)])
                    (if (= n +inf.0) #f n)))
      (define-values (r-m* back-amt) (compile*/maybe (rx:repeat-rx rx) min max))
      (cond
       [(and r-m*
             (not (rx:repeat-non-greedy? rx)))
        (if next-backtracks?
            (REPEAT-SIMPLE-MANY r-m* back-amt min max next-m)
            (REPEAT-SIMPLE-MANY+SIMPLE r-m* back-amt min max next-m))]
       [else
        (define r-m (compile (rx:repeat-rx rx)
                             (if simple? done-m continue-m)
                             (not simple?)))
        (cond
         [(rx:repeat-non-greedy? rx)
          (if simple?
              (LAZY-REPEAT-SIMPLE r-m min max next-m)
              (LAZY-REPEAT r-m min max next-m))]
         [else
          (if simple?
              (if next-backtracks?
                  (REPEAT-SIMPLE r-m min max next-m)
                  (REPEAT-SIMPLE+SIMPLE r-m min max next-m))
              (REPEAT r-m min max next-m))])])]
     [(rx:group? rx)
      (define n (rx:group-number rx))
      (define m (compile (rx:group-rx rx)
                         (GROUP-SET n next-m)
                         #t))
      (GROUP-PUSH n m)]
     [(rx:reference? rx)
      (define n (rx:reference-n rx))
      (if (zero? n)
          (NEVER)
          (REFERENCE (sub1 n) next-m))]
     [(rx:cut? rx)
      (CUT (compile (rx:cut-rx rx) done-m #f) next-m)]
     [(rx:conditional? rx)
      (define tst (rx:conditional-tst rx))
      (define m1 (compile (rx:conditional-rx1 rx) next-m next-backtracks?))
      (define m2 (compile (rx:conditional-rx2 rx) next-m next-backtracks?))
      (cond
       [(rx:reference? tst)
        (CONDITIONAL/REFERENCE (sub1 (rx:reference-n tst)) m1 m2)]
       [else
        (CONDITIONAL/LOOK (compile tst done-m #f) m1 m2)])]
     [(rx:lookahead? rx)
      (LOOKAHEAD (rx:lookahead-match? rx)
                 (compile (rx:lookahead-rx rx) done-m #f)
                 next-m)]
     [(rx:lookbehind? rx)
      (LOOKBEHIND (rx:lookbehind-match? rx)
                  (rx:lookbehind-lb-min rx)
                  (rx:lookbehind-lb-max rx)
                  (compile (rx:lookbehind-rx rx) done-m #f)
                  next-m)]
     [(rx:unicode-categories? rx)
      (UNICODE-CATEGORIES (rx:unicode-categories-symlist rx)
                          (rx:unicode-categories-match? rx)
                          next-m)]
     [else (error 'compile/bt "internal error: unrecognized ~s" rx)])))

;; Compile a matcher repeater, if possible; the result is
;; the repeating matcher and the (consistent) length of each match
(define (compile*/maybe rx min max)
  (cond
   [(exact-integer? rx)
    (values (BYTE* rx max) 1)]
   [(bytes? rx)
    (values (BYTES* rx max) (bytes-length rx))]
   [(eq? rx rx:any)
    (values (ANY* max) 1)]
   [(rx:range? rx)
    (values (RANGE* (compile-range (rx:range-range rx)) max) 1)]
   [else
    (values #f #f)]))

;; Determine the length of the prefix of `l` that needs backtracking:
(define (count-backtrack-prefix l)
  (let loop ([l l] [total 0] [non-bt 0])
    (cond
     [(null? l) (- total non-bt)]
     [(needs-backtrack? (car l))
      (loop (cdr l) (add1 total) 0)]
     [else
      (loop (cdr l) (add1 total) (add1 non-bt))])))

;; ----------------------------------------

(define-syntax-rule (define-plain+simple+tail (GENERAL SIMPLE TAIL arg ... next-m)
                      (lambda (s pos start end)
                        tst
                        next-pos))
  (begin
    ;; General mode for `next-m` that may backtrack
    (define (GENERAL arg ... next-m)
      (lambda (s pos start end state stack fail-k)
        (if tst
            (next-m s next-pos start end state stack fail-k)
            (fail-k))))
    ;; Simple mode when we don't need to use `fail-k`,
    ;; because `next-m` can't backtrack
    (define (SIMPLE arg ... next-m)
      (lambda (s pos start end state stack fail-k)
        (and tst
             (next-m s next-pos start end state stack fail-k))))
    ;; Tail mode when `next-m` is `done-m` (which not only
    ;; can't backtrack, but just returns the given posiion)
    (define (TAIL arg ...)
      (lambda (s pos start end state stack fail-k)
        (and tst
             next-pos)))))

;; An iterator performs a single match as many times as possible, up
;; to a specified max number of times, and it returns the position
;; and the number of items; this mode is used only when each match
;; has a fixed size
(define-syntax-rule (define-iterate (OP* arg ...)
                      defn ...
                      (lambda (s pos2 start end)
                        #:size size
                        #:s-test s-tst
                        #:ls-test ls-tst))
  (define (OP* arg ... max)
    defn ...
    (lambda (s pos start end)
      (if (bytes? s)
          (let ([end (if max
                         (min end (+ pos (* size max)))
                         end)])
            (let loop ([pos2 pos])
              (cond
               [(or ((+ pos2 size) . > . end)
                    (not s-tst))
                (values pos2 (- pos2 pos))]
               [else (loop (+ pos2 size))])))
          (let ([end (and max (+ pos (* size max)))])
            (let loop ([pos2 pos])
              (cond
               [(or (and end ((+ pos2 size) . > . end))
                    (lazy-bytes-before-end? s (sub1 (+ pos2 size)) end)
                    (not ls-tst))
                (values pos2 (- pos2 pos))]
               [else
                (loop (+ pos2 size))])))))))

;; ----------------------------------------
;; Single-byte matching

(define-plain+simple+tail (BYTE BYTE-SIMPLE BYTE-TAIL b next-m)
  (lambda (s pos start end)
    (if (bytes? s)
        (and (pos . < . end)
             (= b (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos end)
             (= b (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (BYTE* b)
  (lambda (s pos start end)
    #:size 1
    #:s-test (= b (bytes-ref s pos))
    #:ls-test (= b (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Byte-string matching

(define-plain+simple+tail (BYTES BYTES-SIMPLE BYTES-TAIL bstr len next-m)
  (lambda (s pos start end)
    (if (bytes? s)
        (and ((+ pos len) . <= . end)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [c2 (in-bytes s pos (+ pos len))])
               (= c1 c2)))
        (and (lazy-bytes-before-end? s (+ pos len) end)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [i (in-naturals pos)])
               (define c2 (lazy-bytes-ref s i))
               (= c1 c2))))
    (+ pos len)))

(define-iterate (BYTES* bstr)
  (define len (bytes-length bstr))
  (lambda (s pos start end)
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

(define (NEVER)
  (lambda (s pos start end state stack fail-k)
    (fail-k)))

;; ----------------------------------------
;; Match any byte

(define-plain+simple+tail (ANY ANY-SIMPLE ANY-TAIL next-m)
  (lambda (s pos start end)
    (if (bytes? s)
        (pos . < . end)
        (lazy-bytes-before-end? s pos end))
    (add1 pos)))

(define-iterate (ANY*)
  (lambda (s pos start end)
    #:size 1
    #:s-test #t
    #:ls-test #t))

;; ----------------------------------------
;; Match any byte in a set

(define-plain+simple+tail (RANGE RANGE-SIMPLE RANGE-TAIL rng next-m)
  (lambda (s pos start end)
    (if (bytes? s)
        (and (pos . < . end)
             (rng-in? rng (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos end)
             (rng-in? rng (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (RANGE* rng)
  (lambda (s pos start end)
    #:size 1
    #:s-test (rng-in? rng (bytes-ref s pos))
    #:ls-test (rng-in? rng (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Matches that don't consume any characters,
;; such as end-of-string or word-boundary

(define-syntax-rule (define-zero-width (OP arg ... next-m)
                      (lambda (s pos start end)
                        tst))
  (define (OP arg ... next-m)
    (lambda (s pos start end state stack fail-k)
      (if tst
          (next-m s pos start end state stack fail-k)
          (fail-k)))))

(define-zero-width (START next-m)
  (lambda (s pos start end)
    (= pos start)))

(define-zero-width (END next-m)
  (lambda (s pos start end)
    (= pos end)))

(define-zero-width (LINE-START next-m)
  (lambda (s pos start end)
    (or (= pos start)
        (= (char->integer #\newline)
           (if (bytes? s)
               (bytes-ref s (sub1 pos))
               (lazy-bytes-ref s (sub1 pos)))))))

(define-zero-width (LINE-END next-m)
  (lambda (s pos start end)
    (or (if (bytes? s)
            (= pos end)
            (not (lazy-bytes-before-end? s pos end)))
        (= (char->integer #\newline)
           (if (bytes? s)
               (bytes-ref s pos)
               (lazy-bytes-ref s pos))))))

(define-zero-width (WORD-BOUNDARY next-m)
  (lambda (s pos start end)
    (word-boundary? s pos start end)))

(define-zero-width (NOT-WORD-BOUNDARY next-m)
  (lambda (s pos start end)
    (not (word-boundary? s pos start end))))

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

;; ----------------------------------------
;; Alternatives

(define (ALTS m1 m2)
  (lambda (s pos start end state stack fail-k)
    (m1 s pos start end state stack
        (lambda ()
          (m2 s pos start end state stack fail-k)))))

;; ----------------------------------------
;; Repeats, greedy (normal) and non-greedy,
;; in various optimized forms

(define (REPEAT r-m min max next-m)
  ;; The tail of `r-m` is set to `continue-m` instead
  ;; of `done-m`, so we can supply a success continuation
  ;; by pushing it onto the stack
  (lambda (s pos start end state stack fail-k)
    (let rloop ([pos pos] [n 0] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) fail-k))
                                stack))
        (r-m s pos start end state new-stack fail-k)]
       [(and max (= n max)) (next-m s pos start end state stack fail-k)]
       [else
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) fail-k))
                                stack))
        (r-m s pos start end state new-stack
             (lambda () (next-m s pos start end state stack fail-k)))]))))

(define r-stack (list (lambda (pos fail-k) pos)))

(define (REPEAT-SIMPLE r-m min max next-m)
  ;; The `r-m` matcher doesn't need bcktracking, so
  ;; we can supply a failure continuation that just
  ;; returns `#f` (which avoids allocating that
  ;; continuation)
  (lambda (s pos start end state stack fail-k)
    (let rloop ([pos pos] [n 0] [back-amt 0])
      (define pos2
        (if (or (not max) (n . < . max))
            (r-m s pos start end state r-stack (lambda () #f))
            #f))
      (if pos2
          (rloop pos2 (add1 n) (- pos2 pos))
          (let bloop ([pos pos] [n n])
            (cond
             [(n . < . min) (fail-k)]
             [else (next-m s pos start end state stack
                           (lambda () (bloop (- pos back-amt) (sub1 n))))]))))))

(define (REPEAT-SIMPLE+SIMPLE r-m min max next-m)
  ;; The `r-m` matcher doesn't need bcktracking, and neither does
  ;; `next-m`, so we can avoid yet more continuations.
  (lambda (s pos start end state stack fail-k)
    (let rloop ([pos pos] [n 0] [back-amt 0])
      (define pos2
        (if (or (not max) (n . < . max))
            (r-m s pos start end state r-stack (lambda () #f))
            #f))
      (if pos2
          (rloop pos2 (add1 n) (- pos2 pos))
          (let bloop ([pos pos] [n n])
            (cond
             [(n . < . min) (fail-k)]
             [else
              (define pos2 (next-m s pos start end state stack (lambda () #f)))
              (or pos2
                  (bloop (- pos back-amt) (sub1 n)))]))))))

(define (REPEAT-SIMPLE-MANY r-m* back-amt min max next-m)
  ;; Instead of `r-m`, we have a `r-m*` that finds as many matches as
  ;; possible (up to max) in one go
  (lambda (s pos start end state stack fail-k)
    (define-values (pos2 n) (r-m* s pos start end))
    (let bloop ([pos pos2] [n n])
      (cond
       [(n . < . min) (fail-k)]
       [else (next-m s pos start end state stack
                     (lambda () (bloop (- pos back-amt) (sub1 n))))]))))

(define (REPEAT-SIMPLE-MANY+SIMPLE r-m* back-amt min max next-m)
  (lambda (s pos start end state stack fail-k)
    (define-values (pos2 n) (r-m* s pos start end))
    (let bloop ([pos pos2] [n n])
      (cond
       [(n . < . min) (fail-k)]
       [else
        (define pos2 (next-m s pos start end state stack (lambda () #f)))
        (or pos2
            (bloop (- pos back-amt) (sub1 n)))]))))

(define (LAZY-REPEAT r-m min max next-m)
  ;; Like `REPEAT`: the tail of `r-m` is set to `continue-m`
  (lambda (s pos start end state stack fail-k)
    (let rloop ([pos pos] [n 0] [min min] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos fail-k)
                                  (rloop pos (add1 n) fail-k))
                                stack))
        (r-m s pos start end state new-stack fail-k)]
       [(and max (= n max))
        (next-m s pos start end state stack fail-k)]
       [else
        (next-m s pos start end state stack
                (lambda () (rloop pos n (add1 min) fail-k)))]))))
  
(define (LAZY-REPEAT-SIMPLE r-m min max next-m)
  ;; Like `REPEAT-SIMPLE`: no backtracking `r-m`
  (lambda (s pos start end state stack fail-k)
    (let rloop ([pos pos] [n 0] [min min] [fail-k fail-k])
      (cond
       [(n . < . min)
        (define pos2 (r-m s pos start end state stack (lambda () #f)))
        (if pos2
            (rloop pos2 (add1 n) min fail-k)
            (fail-k))]
       [(and max (= n max))
        (next-m s pos start end state stack fail-k)]
       [else
        (next-m s pos start end state stack
                (lambda () (rloop pos n (add1 min) fail-k)))]))))

;; ----------------------------------------
;; Recording and referencing group matches

(define (GROUP-PUSH n next-m)
  (lambda (s pos start end state stack fail-k)
    (define new-stack (cons (cons pos (and state (vector-ref state n)))
                            stack))
    (next-m s pos start end state new-stack fail-k)))

(define (GROUP-SET n next-m)
  (lambda (s pos start end state stack fail-k)
    (define old-pos+span (car stack))
    (define old-span (cdr old-pos+span))
    (when state
      (vector-set! state n (cons (car old-pos+span) pos)))
    (next-m s pos start end state (cdr stack)
            (lambda ()
              (when state (vector-set! state n old-span))
              (fail-k)))))

(define (REFERENCE n next-m)
  (lambda (s pos start end state stack fail-k)
    (define p (vector-ref state n))
    (cond
     [(not p) (fail-k)]
     [else
      (define len (- (cdr p) (car p)))
      (define matches?
        (if (bytes? s)
            (and ((+ pos len) . <= . end)
                 (for/and ([c1 (in-bytes s (car p) (cdr p))]
                           [c2 (in-bytes s pos (+ pos len))])
                   (= c1 c2)))
            (and (lazy-bytes-before-end? s (+ pos len) end)
                 (for/and ([j (in-range (car p) (cdr p))]
                           [i (in-naturals pos)])
                   (define c1 (lazy-bytes-ref s j))
                   (define c2 (lazy-bytes-ref s i))
                   (= c1 c2)))))
      (if matches?
          (next-m s pos start end state stack fail-k)
          (fail-k))])))

;; ----------------------------------------
;; Lookahead, lookbehind, conditionals, and cut

(define (LOOKAHEAD match? sub-m next-m)
  (lambda (s pos start end state stack fail-k)
    (define pos2 (sub-m s pos start end state null (lambda () #f)))
    (if (eq? match? (and pos2 #t))
        (next-m s pos start end state stack fail-k)
        (fail-k))))

(define (LOOKBEHIND match? lb-min lb-max sub-m next-m)
  (lambda (s pos start end state stack fail-k)
    (define lb-min-pos (max 0 (- pos lb-max)))
    (let loop ([lb-pos (- pos lb-max)])
      (cond
       [(lb-pos . < . lb-min-pos)
        (if match?
            (fail-k)
            (next-m s pos start end state stack fail-k))]
       [else
        (define pos2 (sub-m s lb-pos start end state null (lambda () #f)))
        (if (eq? match? (and pos2 #t))
            (next-m s pos start end state stack fail-k)
            (loop (sub1 lb-pos)))]))))

(define (CONDITIONAL/REFERENCE n m1 m2)
  (lambda (s pos start end state stack fail-k)
    (if (vector-ref state n)
        (m1 s pos start end state stack fail-k)
        (m2 s pos start end state stack fail-k))))

(define (CONDITIONAL/LOOK tst-m m1 m2)
  (lambda (s pos start end state stack fail-k)
    (if (tst-m s pos start end state null (lambda () #f))
        (m1 s pos start end state stack fail-k)
        (m2 s pos start end state stack fail-k))))

(define (CUT sub-m next-m)
  (lambda (s pos start end state stack fail-k)
    (define pos2 (sub-m s pos start end state null (lambda () #f)))
    (if pos2
        (next-m s pos2 start end state stack fail-k)
        (fail-k))))

;; ----------------------------------------
;; Unicode characters in UTF-8 encoding

(define (UNICODE-CATEGORIES cats match? next-m)
  (lambda (s pos start end state stack fail-k)
    (let loop ([pos pos] [accum null])
      (define b
        (if (bytes? s)
            (and (pos . < . end)
                 (bytes-ref s pos))
            (and (lazy-bytes-before-end? s pos end)
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
              (next-m s (add1 pos) start end state stack fail-k)
              (fail-k))]
         [(eq? c 'fail)
          (fail-k)]
         [else
          ;; c must be 'continue
          (loop (add1 pos (cons b accum)))])]))))
