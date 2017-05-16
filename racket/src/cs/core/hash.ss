;; To support iteration and locking, we wrap Chez's mutable hash
;; tables in a `mutable-hash` record:
(define-record mutable-hash (ht keys keys-stale? lock))
(define (create-mutable-hash ht) (make-mutable-hash ht '#() #t (make-lock)))

(define (hash? v) (or (hamt? v) (mutable-hash? v) (weak-equal-hash? v)))

(define (make-hash) (create-mutable-hash (make-hashtable equal-hash-code equal?)))

(define (make-hasheq) (create-mutable-hash (make-eq-hashtable)))
(define (make-weak-hasheq) (create-mutable-hash (make-weak-eq-hashtable)))
(define (make-hasheqv) (create-mutable-hash (make-eqv-hashtable)))
(define (make-weak-hasheqv) (create-mutable-hash (make-weak-eqv-hashtable)))

(define-syntax define-hash-constructors
  (syntax-rules ()
    [(_ vararg-ctor list-ctor empty-hash)
     (begin
       (define (vararg-ctor . kvs)
         (let loop ([kvs kvs] [h empty-hash])
           (cond [(null? kvs) h]
                 [else
                  (loop (cddr kvs) (hamt-set h (car kvs) (cadr kvs)))])))

       (define list-ctor
         (case-lambda
          [() (vararg-ctor)]
          [(assocs)
           (let loop ([h (vararg-ctor)] [assocs assocs])
             (if (null? assocs)
                 h
                 (loop (hamt-set h (caar assocs) (cdar assocs))
                       (cdr assocs))))])))]))

(define-hash-constructors hash make-immutable-hash empty-hash)
(define-hash-constructors hasheqv make-immutable-hasheqv empty-hasheqv)
(define-hash-constructors hasheq make-immutable-hasheq empty-hasheq)

(define (hash-set! ht k v)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (set-mutable-hash-keys-stale?! ht #t)
    (hashtable-set! (mutable-hash-ht ht) k v)
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-set! ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-remove! ht k)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (set-mutable-hash-keys-stale?! ht #t)
    (hashtable-delete! (mutable-hash-ht ht) k)
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-remove! ht k)]
   [else (raise-argument-error 'hash-remove! "(and/c hash? (not/c immutable?))" ht)]))

#|
 hash-clear! procedures do not use the table’s semaphore to guard the traversal as a whole. 
 Changes by one thread to a hash table can affect the keys and values seen by another thread 
 part-way through its traversal of the same hash table.

 Maybe lock isn't needed even for set-mutable-hash-keys! ?
|#
(define (hash-clear! ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (set-mutable-hash-keys! ht '#())
    (hashtable-clear! (mutable-hash-ht ht))
    (lock-release (mutable-hash-lock ht))]
   [(weak-equal-hash? ht) (weak-hash-clear! ht)]
   [else (raise-argument-error 'hash-clear! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-copy ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (let ([new-ht (create-mutable-hash (hashtable-copy (mutable-hash-ht ht) #t))])
      (lock-release (mutable-hash-lock ht))
      new-ht)]
   [(weak-equal-hash? ht) (weak-hash-copy ht)]
   [(hamt? ht)
    (let ([ht (cond
               [(hamt-eq? ht) (make-hasheq)]
               [(hamt-eqv? ht) (make-hasheqv)]
               [else (make-hash)])])
      (let loop ([i (hamt-iterate-first ht)])
        (when i
          (let-values ([(k v) (hamt-iterate-key+value ht i #f)])
            (hashtable-set! ht k v)
            (loop (hamt-iterate-next ht i)))))
      (create-mutable-hash ht))]
   [else (raise-argument-error 'hash-copy "hash?" ht)]))

(define (hash-set ht k v)
  (cond
   [(hamt? ht) (hamt-set ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? immutable?)" ht)]))

(define (hash-remove ht k)
  (cond
   [(hamt? ht) (hamt-remove ht k)]
   [else (raise-argument-error 'hash-remove "(and/c hash? immutable?)" ht)]))

(define (hash-eq? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) eq?)]
   [(hamt? ht)
    (hamt-eq? ht)]
   [(weak-equal-hash? ht) #f]
   [else (raise-argument-error 'hash-eq? "hash?" ht)]))

(define (hash-eqv? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) eqv?)]
   [(hamt? ht)
    (hamt-eqv? ht)]
   [(weak-equal-hash? ht) #f]
   [else (raise-argument-error 'hash-eqv? "hash?" ht)]))

(define (hash-equal? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (hashtable-equivalence-function (mutable-hash-ht ht)) equal?)]
   [(hamt? ht)
    (hamt-equal? ht)]
   [(weak-equal-hash? ht) #t]
   [else (raise-argument-error 'hash-equal? "hash?" ht)]))

(define (hash-weak? ht)
  (cond
   [(mutable-hash? ht)
    (hashtable-weak? (mutable-hash-ht ht))]
   [(hamt? ht) #f]
   [(weak-equal-hash? ht) #t]
   [else (raise-argument-error 'hash-weak? "hash?" ht)]))

(define hash-ref
  (case-lambda
    [(ht k) 
     (let ([v (hash-ref ht k none)])
       (if (eq? v none)
           (raise-arguments-error
            'hash-ref
            "no value found for key"
            "key" k)
           v))]
    [(ht k fail)
     (cond
      [(mutable-hash? ht)
       (if (procedure? fail)
           (let ([v (hashtable-ref (mutable-hash-ht ht) k none)])
             (if (eq? v none)
                 (fail)
                 v))
           (hashtable-ref (mutable-hash-ht ht) k fail))]
      [(hamt? ht) (hamt-ref ht k fail)]
      [(weak-equal-hash? ht) (weak-hash-ref ht k fail)]
      [else (raise-argument-error 'hash-ref "hash?" ht)])]))

(define (hash-for-each ht proc)
  (cond
   [(mutable-hash? ht)
    (let loop ([i (hash-iterate-first ht)])
      (when i
        (let-values ([(key val) (hash-iterate-key+value ht i)])
          (proc key val))
        (loop (hash-iterate-next ht i))))]
   [(hamt? ht) (hamt-for-each ht proc)]
   [(weak-equal-hash? ht) (weak-hash-for-each ht proc)]
   [else (raise-argument-error 'hash-for-each "hash?" ht)]))

(define (hash-map ht proc)
  (cond
   [(mutable-hash? ht)
    (let loop ([i (hash-iterate-first ht)])
      (if (not i)
          '()
          (cons
           (let-values ([(key val) (hash-iterate-key+value ht i)])
             (proc key val))
           (loop (hash-iterate-next ht i)))))]
   [(hamt? ht) (hamt-map ht proc)]
   [(weak-equal-hash? ht) (weak-hash-map ht proc)]
   [else (raise-argument-error 'hash-map "hash?" ht)]))

(define (hash-count ht)
  (cond
   [(mutable-hash? ht) (hashtable-size (mutable-hash-ht ht))]
   [(hamt? ht) (hamt-count ht)]
   [(weak-equal-hash? ht) (weak-hash-count ht)]
   [else (raise-argument-error 'hash-count "hash?" ht)]))

(define (hash-keys-subset? ht1 ht2)
  (cond
   [(and (hamt? ht1)
         (hamt? ht2)
         (or (and (hamt-eq? ht1)
                  (hamt-eq? ht2))
             (and (hamt-eqv? ht1)
                  (hamt-eqv? ht2))
             (and (hamt-equal? ht1)
                  (hamt-equal? ht2))))
    (hamt-keys-subset? ht1 ht2)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2)))
         (eq? (hash-weak? ht1) (hash-weak? ht2)))
    (and (<= (hash-count ht1) (hash-count ht2))
         (let ([ok? #t])
           (hash-table-for-each
            ht1
            (lambda (k v)
              (not (eq? none (hashtable-ref ht2 k none)))))))]
   [(not (hash? ht1))
    (raise-argument-error 'hash-keys-subset? "hash?" ht1)]
   [(not (hash? ht2))
    (raise-argument-error 'hash-keys-subset? "hash?" ht2)]
   [else
    (raise-arguments-error 'hash-keys-subset?
                           "given hash tables do not use the same key comparison"
                           "first table" ht1
                           "first table" ht2)]))

;; Use `eql?` for recursive comparisons
(define (hash=? ht1 ht2 eql?)
  (cond
   [(and (hamt? ht1)
         (hamt? ht2)
         (or (and (hamt-eq? ht1)
                  (hamt-eq? ht2))
             (and (hamt-eqv? ht1)
                  (hamt-eqv? ht2))
             (and (hamt-equal? ht1)
                  (hamt-equal? ht2))))
    (hamt=? ht1 ht2 eql?)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2)))
         (eq? (hash-weak? ht1) (hash-weak? ht2)))
    (and (= (hash-count ht1) (hash-count ht2))
         (let loop ([i (hash-iterate-first ht1)])
           (cond
            [(not i) #t]
            [else
             (let-values ([(key val) (hash-iterate-key+value ht1 i)])
               (let ([val2 (hash-ref ht2 key none)])
                 (cond
                  [(eq? val2 none) #f]
                  [else (and (eql? val val2)
                             (loop (hash-iterate-next ht1 i)))])))])))]
   [else #f]))


;; Use `hash` for recursive hashing
(define (hash-hash-code ht hash)
  (cond
   [(hamt? ht) (hamt-hash-code ht hash)]
   [else
    (let loop ([hc 0] [i (hash-iterate-first ht)])
      (cond
       [(not i) hc]
       [else
        (let* ([eq-key? (hash-eq? ht)]
               [eqv-key? (and (not eq?) (hash-eqv? ht))])
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (let ([hc (hash-code-combine hc
                                         (cond
                                          [eq-key? (eq-hash-code key)]
                                          [eqv-key? (eqv-hash-code key)]
                                          [else (hash key)]))])
              (loop (hash-code-combine hc (hash val))
                    (hash-iterate-next ht i)))))]))]))
    

;; A `hash-iterate-first` operation triggers an O(n)
;; gathering of the keys of a mutable hash table. That's
;; unfortunate, but there appears to be no way around it.
(define (prepare-iterate! ht i)
  (let ([vec (mutable-hash-keys ht)])
    (or (and (or i (not (mutable-hash-keys-stale? ht)))
             vec)
        (let ([vec (hashtable-keys (mutable-hash-ht ht))])
          ;; Keep a weak reference to each key, in case
          ;; it's removed or we have a weak hash table:
          (let loop ([i (vector-length vec)])
            (unless (zero? i)
              (let* ([i (sub1 i)]
                     [key (vector-ref vec i)])
                (vector-set! vec i (weak-cons key #f))
                (loop i))))
	  (lock-acquire (mutable-hash-lock ht))
          (set-mutable-hash-keys! ht vec)
          (set-mutable-hash-keys-stale?! ht #f)
	  (lock-release (mutable-hash-lock ht))
          vec))))

(define (hash-iterate-first ht)
  (cond
   [(hamt? ht)
    (hamt-iterate-first ht)]
   [(mutable-hash? ht)
    (mutable-hash-iterate-next ht #f)]
   [(weak-equal-hash? ht) (weak-hash-iterate-first ht)]
   [else (raise-argument-error 'hash-iterate-first "hash?" ht)]))

(define (check-i who i)
  (unless (and (integer? i)
               (exact? i)
               (>= i 0))
    (raise-argument-error who "exact-nonnegative-integer?" i)))

(define (hash-iterate-next ht i)
  (cond
   [(hamt? ht)
    (check-i 'hash-iterate-next i)
    (hamt-iterate-next ht i)]
   [(mutable-hash? ht)
    (check-i 'hash-iterate-next i)
    (mutable-hash-iterate-next ht i)]
   [(weak-equal-hash? ht)
    (check-i 'hash-iterate-next i)
    (weak-hash-iterate-next ht i)]
   [else (raise-argument-error 'hash-iterate-next "hash?" ht)]))

(define (mutable-hash-iterate-next ht init-i)
  (let* ([vec (prepare-iterate! ht init-i)] ; vec expected to have > `init-i` elements
         [len (vector-length vec)])
    (let loop ([i (or init-i -1)])
      (let ([i (add1 i)])
        (cond
         [(> i len)
          (raise-arguments-error 'hash-iterate-next "no element at index"
                                 "index" init-i)]
         [(= i len)
          #f]
         [else
          (let* ([p (vector-ref vec i)]
                 [key (car p)])
            (cond
             [(bwp-object? key)
              ;; A hash table change or disappeared weak reference
              (loop (add1 i))]
             [else
              (if (or (not (mutable-hash-keys-stale? ht))
                      (hashtable-contains? (mutable-hash-ht ht) key))
                  i
                  ;; Skip, due to a hash table change
                  (loop (add1 i)))]))])))))

(define (do-hash-iterate-key+value who ht i
                                   hamt-iterate-key+value
                                   weak-hash-iterate-key+value
                                   key? value? pair?)
  (cond
   [(hamt? ht)
    (check-i who i)
    (call-with-values (lambda () (hamt-iterate-key+value ht i none))
      (case-lambda
        [(v) (if (eq? v none)
                 (raise-arguments-error who "no element at index"
                                        "index" i)
                 v)]
        [(k v) (values k v)]))]
   [(mutable-hash? ht)
    (check-i who i)
    (let* ([vec (prepare-iterate! ht i)]
           [len (vector-length vec)]
           [p (if (< i len)
                  (vector-ref vec i)
                  '(#f . #f))]
           [key (car p)]
           [v (if (bwp-object? key)
                  none
                  (hashtable-ref (mutable-hash-ht ht) key none))])
      (if (eq? v none)
          (raise-arguments-error who "no element at index"
                                 "index" i)
          (cond
           [(and key? value?)
            (if pair?
                (cons key v)
                (values key v))]
           [key? key]
           [else v])))]
   [(weak-equal-hash? ht)
    (check-i who i)
    (weak-hash-iterate-key+value ht i)]
   [else (raise-argument-error who "hash?" ht)]))

(define (hash-iterate-key ht i)
  (do-hash-iterate-key+value 'hash-iterate-key ht i
                             hamt-iterate-key
                             weak-hash-iterate-key
                             #t #f #f))

(define (hash-iterate-value ht i)
  (do-hash-iterate-key+value 'hash-iterate-value ht i
                             hamt-iterate-value
                             weak-hash-iterate-value
                             #f #t #f))

(define (hash-iterate-key+value ht i)
  (do-hash-iterate-key+value 'hash-iterate-key+value ht i
                             hamt-iterate-key+value
                             weak-hash-iterate-key+value
                             #t #t #f))

(define (hash-iterate-pair ht i)
  (do-hash-iterate-key+value 'hash-iterate-pair ht i
                             hamt-iterate-pair
                             weak-hash-iterate-pair
                             #t #t #t))

(define unsafe-mutable-hash-iterate-first hash-iterate-first)
(define unsafe-mutable-hash-iterate-next hash-iterate-next)
(define unsafe-mutable-hash-iterate-key hash-iterate-key)
(define unsafe-mutable-hash-iterate-value hash-iterate-value)
(define unsafe-mutable-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-mutable-hash-iterate-pair hash-iterate-pair)

(define unsafe-weak-hash-iterate-first hash-iterate-first)
(define unsafe-weak-hash-iterate-next hash-iterate-next)
(define unsafe-weak-hash-iterate-key hash-iterate-key)
(define unsafe-weak-hash-iterate-value hash-iterate-value)
(define unsafe-weak-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-weak-hash-iterate-pair hash-iterate-pair)

;;  ----------------------------------------

;; Chez doesn't provide weak hash table with `equal?` comparisons,
;; so build our own

(define-record weak-equal-hash (ht         ; integer[hash code] -> list of weak pairs
                                count      ; number of items in the table (= sum of list lengths)
                                prune-at   ; count at which we should try to prune empty weak boxes
                                keys))     ; for iteration: a vector that is enlarged on demand

(define (make-weak-hash)
  (make-weak-equal-hash (hasheqv) 0 128 #f))

(define (weak-hash-copy ht)
  (make-weak-equal-hash (weak-equal-hash-ht ht)
                        (weak-equal-hash-count ht)
                        (weak-equal-hash-prune-at ht)
                        #f))

(define (weak-hash-ref t key fail)
  (let* ([code (equal-hash-code key)]
         [vals (hamt-ref (weak-equal-hash-ht t) code '())])
    (let loop ([vals vals])
      (cond
       [(null? vals)
        ;; Not in the table:
        (if (procedure? fail)
            (fail)
            fail)]
       [(equal? (caar vals) key)
        (cdar vals)]
       [else (loop (cdr vals))]))))

(define (weak-hash-ref-key ht key)
  (let* ([code (equal-hash-code key)]
         [vals (hamt-ref (weak-equal-hash-ht ht) code '())])
    (let loop ([vals vals])
      (cond
       [(null? vals) #f]
       [(equal? (car vals) key) (car vals)]
       [else (loop (cdr vals))]))))

(define (weak-hash-set! t k v)
  (let* ([code (equal-hash-code k)]
         [vals (hamt-ref (weak-equal-hash-ht t) code '())])
    (let loop ([vals vals])
      (cond
       [(null? vals)
        ;; Not in the table:
        (set-weak-equal-hash-keys! t #f)
        (when (= (weak-equal-hash-count t) (weak-equal-hash-prune-at t))
          (prune-table! t))
        (let* ([ht (weak-equal-hash-ht t)])
          (set-weak-equal-hash-count! t
                                      (add1 (weak-equal-hash-count t)))
          (set-weak-equal-hash-ht! t
                                   (hamt-set ht code
                                                       (cons (weak-cons k v)
                                                             (hamt-ref ht code '())))))]
       [(equal? (caar vals) k)
        (set-cdr! (car vals) v)]
       [else (loop (cdr vals))]))))

(define (weak-hash-remove! t k)
  (let* ([code (equal-hash-code k)]
         [vals (hamt-ref (weak-equal-hash-ht t) code '())])
    (let loop ([vals vals])
      (cond
       [(null? vals)
        ;; Not in the table
        (void)]
       [(equal? (caar vals) k)
        (set-car! (car vals) #f)
        (set-cdr! (car vals) #f)]
       [else (loop (cdr vals))]))))

(define (weak-hash-clear! t)
  (set-weak-equal-hash-ht! t (hasheqv))
  (set-weak-equal-hash-count! t 0)
  (set-weak-equal-hash-prune-at! t 128))

(define (weak-hash-for-each ht proc)
  (hamt-for-each
   (weak-equal-hash-ht ht)
   (lambda (k l)
     (let loop ([l l])
       (cond
        [(null? l) (void)]
        [else
         (let ([k (caar l)])
           (unless (bwp-object? k)
             (proc k (cdar l))))
         (loop (cdr l))])))))

(define (weak-hash-map t proc)
  (let ([ht (weak-equal-hash-ht t)])
    (let loop ([i (unsafe-hamt-iterate-first ht)])
      (cond
       [i (let iloop ([l (unsafe-hamt-iterate-value ht i)])
            (cond
             [(null? l) (loop (unsafe-hamt-iterate-next ht i))]
             [else
              (let ([k (caar l)])
                (if (bwp-object? k)
                    (iloop (cdr l))
                    (cons (proc k (cdar l))
                          (iloop (cdr l)))))]))]
       [else '()]))))

(define (weak-hash-count ht)
  (define c 0)
  (weak-hash-for-each ht (lambda (k v) (set! c (add1 c))))
  c)

(define (prepare-weak-iterate! ht i)
  (let* ([current-vec (weak-equal-hash-keys ht)])
    (or (and i
             (> (vector-length current-vec) i)
             current-vec)
        (let* ([len (max 16
                         (* 2 (if current-vec
                                  (vector-length current-vec)
                                  0))
                         (if i (* 2 i) 0))]
               [vec (make-vector len #f)]
               [pos 0])
          (call/cc
           (lambda (esc)
             (hamt-for-each
              (weak-equal-hash-ht ht)
              (lambda (k l)
                (let loop ([l l])
                  (cond
                   [(null? l) (void)]
                   [else
                    ;; Add `(car l)` even if the key is #!bwp,
                    ;; so that iteration works right if a key
                    ;; is removed
                    (vector-set! vec pos (car l))
                    (set! pos (add1 pos))
                    (if (= pos len)
                        ;; That's enough keys
                        (esc (void))
                        (loop (cdr l)))]))))))
          vec))))

(define (weak-hash-iterate-first ht)
  (weak-hash-iterate-next ht #f))

(define (weak-hash-iterate-next ht init-i)
  (let retry ([i init-i])
    (let* ([vec (prepare-weak-iterate! ht i)]
           [len (vector-length vec)])
      (let loop ([i i])
        (cond
         [(= i len)
          ;; expand set of prepared keys
          (retry i)]
         [(> i len)
          (raise-arguments-error 'hash-iterate-next "no element at index"
                                 "index" init-i)]
         [else
          (let ([p (vector-ref vec i)])
            (cond
             [(not p)
              ;; no more keys available
              #f]
             [(bwp-object? (car p)) (add1 i)]
             [else i]))])))))

(define (do-weak-hash-iterate-key who ht i)
  (let* ([vec (weak-equal-hash-keys ht)]
         [p (and vec
                 (< i (vector-length vec))
                 (vector-ref vec i))]
         [k (if p
                (car p)
                #!bwp)])
    (cond
     [(bwp-object? k)
      (raise-arguments-error who "no element at index"
                             "index" i)]
     [else k])))

(define (weak-hash-iterate-key ht i)
  (do-weak-hash-iterate-key 'weak-hash-iterate-key ht i))

(define (weak-hash-iterate-value ht i)
  (define key (do-weak-hash-iterate-key 'weak-hash-iterate-value ht i))
  (weak-hash-ref ht key (lambda ()
                          (raise-arguments-error
                           'weak-hash-iterate-value "no element at index"
                           "index" i))))

(define (weak-hash-iterate-key+value ht i)
  (define key (do-weak-hash-iterate-key 'weak-hash-iterate-key+value ht i))
  (values key
          (weak-hash-ref ht key (lambda ()
                                  (raise-arguments-error
                                   'weak-hash-iterate-key+value "no element at index"
                                   "index" i)))))

(define (weak-hash-iterate-pair ht i)
  (define key (do-weak-hash-iterate-key 'weak-hash-iterate-pair ht i))
  (cons key
        (weak-hash-ref ht key (lambda ()
                                (raise-arguments-error
                                 'weak-hash-iterate-paur "no element at index"
                                 "index" i)))))

;; Remove empty weak boxes from a table. Count the number
;; of remaining entries, and remember to prune again when
;; the number of entries doubles (up to at least reaches 128)
(define (prune-table! t)
  (let ([ht (weak-equal-hash-ht t)])
    (let-values ([(new-ht count)
                  (let loop ([ht ht]
                             [i (hamt-iterate-first ht)]
                             [count 0])
                    (cond
                     [(not i) (values ht count)]
                     [else
                      (let-values ([(key l) (hamt-iterate-key+value ht i #f)])
                        (let ([l (chez:filter (lambda (p) (not (bwp-object? (car p)))) l)])
                          (loop (if (null? l)
                                    ht
                                    (hash-set ht key l))
                                (hamt-iterate-next ht i)
                                (+ count (length l)))))]))])
      (set-weak-equal-hash-ht! t new-ht)
      (set-weak-equal-hash-count! t count)
      (set-weak-equal-hash-prune-at! t (max 128 (* 2 count))))))

;; ----------------------------------------

(define ignored/hash
  (begin
    (record-type-equal-procedure (record-type-descriptor mutable-hash)
                                 hash=?)
    (record-type-hash-procedure (record-type-descriptor mutable-hash)
                                hash-hash-code)
    (record-type-equal-procedure (record-type-descriptor weak-equal-hash)
                                 hash=?)
    (record-type-hash-procedure (record-type-descriptor weak-equal-hash)
                                hash-hash-code)))
