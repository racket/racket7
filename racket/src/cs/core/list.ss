
;; Make `list?` an amoritized constant-time operation, which is
;; possible because `set-cdr!` is not exposed. Make it constant-time
;; by caching the result for an N-item list at the N/2-tail pair,
;; where a second request will cache at the N/4-tail pair, etc.
;; Detect cycles using the same `slow` tortoise that is used for
;; caching.

(define lists (make-weak-eq-hashtable))

(define (list? v)
  (cond
   [(null? v) #t]
   [(not (pair? v)) #f]
   [else
    (let loop ([fast (cdr v)] [slow v] [slow-step? #f] [depth 0])
      (define (return result)
        (when (fx> depth 10)
          (hashtable-set! lists slow result))
        result)
      (cond
       [(null? fast) (return #t)]
       [(not (pair? fast)) (return #f)]
       [(eq? fast slow) (return #f)] ; cycle
       [else
        (let ([is-list? (hashtable-ref lists fast none)])
          (cond
           [(eq? is-list? none)
            (loop (cdr fast) (if slow-step? (cdr slow) slow) (not slow-step?) (fx1+ depth))]
           [else
            (return is-list?)]))]))]))

(define (append-n l n l2)
  (cond
   [(fx= 0 n) l2]
   [else (cons (car l) (append-n (cdr l) (fx1- n) l2))]))
  
