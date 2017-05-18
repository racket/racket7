;; maps (cons prefab-key total-field-count) to rtd:
(define prefabs #f)

(define (prefab-struct-key v)
  (and (record? v)
       (let ([p (getprop (record-type-uid (record-rtd v)) 'prefab-key+count #f)])
         (and p (car p)))))

(define (prefab-key->struct-type key field-count)
  (let ([norm-key (normalized-prefab-key/check 'prefab-key->struct-type key field-count)])
    (let ([prefab-key+count (cons norm-key field-count)])
      (unless prefabs (set! prefabs (make-weak-hash)))
      (cond
       [(and prefabs
             (hash-ref prefabs prefab-key+count #f))
        => (lambda (rtd) rtd)]
       [else
        (let* ([parent-prefab-key+count (prefab-key->parent-prefab-key+count norm-key)]
               [parent-rtd (and parent-prefab-key+count
                                (prefab-key->struct-type (car parent-prefab-key+count)
                                                         (cdr parent-prefab-key+count)))]
               [name (if (symbol? norm-key)
                         norm-key
                         (car norm-key))]
               [rtd (make-record-type parent-rtd
                                      (symbol->string name)
                                      (make-field-names
                                       (- field-count
                                          (if parent-rtd
                                              (struct-type-field-count parent-rtd)
                                              0))))])
          ;; FIXME: make atomic
          (hash-set! prefabs prefab-key+count rtd)
          (putprop (record-type-uid rtd) 'prefab-key+count prefab-key+count)
          (inspector-set! rtd #f)
          (unless parent-rtd
            (record-type-equal-procedure rtd default-struct-equal?)
            (record-type-hash-procedure rtd default-struct-hash))
          rtd)]))))

(define (make-prefab-struct key . args)
  (let* ([field-count (length args)]
         [norm-key (normalized-prefab-key/check 'make-prefab-struct key field-count)])
    (let ([rtd
           (cond
            [(and prefabs
                  (hash-ref prefabs (cons norm-key field-count) #f))
             => (lambda (rtd) rtd)]
            [else (prefab-key->struct-type key field-count)])])
      (apply (record-constructor rtd) args))))

;; ----------------------------------------

;; Check that `k` is valid as a prefab key
(define (prefab-key? k)
  (or (symbol? k)
      (and (pair? k)
           (symbol? (car k))
           (let* ([k (cdr k)] ; skip name
                  [prev-k k]
                  ;; The initial field count can be omitted:
                  [k (if (and (pair? k)
                              (exact-nonnegative-integer? (car k)))
                         (cdr k)
                         k)]
                  [field-count (if (eq? prev-k k)
                                   #f
                                   (car prev-k))])
             (let loop ([field-count field-count] [k k]) ; `k` is after name and field count
               (or (null? k)
                   (and (pair? k)
                        (let* ([prev-k k]
                               [k (if (and (pair? (car k))
                                           (pair? (cdar k))
                                           (null? (cddar k))
                                           (exact-nonnegative-integer? (caar k)))
                                      ;; Has a (list <auto-count> <auto-val>) element
                                      (cdr k)
                                      ;; Doesn't have auto-value element:
                                      k)]
                               [auto-count (if (eq? prev-k k)
                                               0
                                               (caar prev-k))])
                          (or (null? k)
                              (and (pair? k)
                                   (let* ([k (if (and (pair? k)
                                                      (vector? (car k)))
                                                 ;; Make sure it's a vector of indices
                                                 ;; that are in range and distinct:
                                                 (let* ([vec (car k)]
                                                        [len (vector-length vec)])
                                                   (let loop ([i 0] [set 0])
                                                     (cond
                                                      [(= i len) (cdr k)]
                                                      [else
                                                       (let ([pos (vector-ref vec i)])
                                                         (and (exact-nonnegative-integer? pos)
                                                              (or (not field-count)
                                                                  (< pos (+ field-count auto-count)))
                                                              (not (bitwise-bit-set? set pos))
                                                              (loop (add1 i) (bitwise-ior set (bitwise-arithmetic-shift-left 1 pos)))))])))
                                                 k)])
                                     (or (null? k)
                                         (and (pair? k)
                                              ;; Supertype: make sure it's a pair with a
                                              ;; symbol and a field count, and loop:
                                              (symbol? (car k))
                                              (pair? (cdr k))
                                              (exact-nonnegative-integer? (cadr k))
                                              (loop (cadr k) (cddr k)))))))))))))))

;; Assuming `(prefab-key? k)`, check that it's consistent with the
;; given total field count
(define (prefab-key-compatible-count? k total-field-count)
  (define (field-count-after-name+count k)
    (cond
     [(null? k) 0]
     [(pair? (car k))
      (+ (caar k)
         (field-count-after-name+count+auto (cdr k)))]
     [else
      (field-count-after-name+count+auto k)]))
  (define (field-count-after-name+count+auto k)
    (cond
     [(null? k) 0]
     [(vector? (car k))
      (if (null? (cdr k))
          0
          (field-count (cdr k)))]
     [else (field-count k)]))
  (define (field-count k) ; k has symbol and count
    (+ (cadr k)
       (field-count-after-name+count (cddr k))))
  (cond
   [(symbol? k) #t]
   [(null? (cdr k)) #t]
   [(exact-integer? (cadr k))
    ;; Info must match exactly
    (= total-field-count
       (+ (cadr k) (field-count-after-name+count (cddr k))))]
   [else
    (let ([n (field-count-after-name+count (cdr k))])
      (and
       ;; Field count must be <= total-field-count
       (>= total-field-count n)
       ;; Initial mutables vector (if any) must be in range
       ;; for the target field count
       (let* ([k (cdr k)]
              [auto (and (pair? (car k))
                         (car k))]
              [k (if auto
                     (cdr k)
                     k)])
         (or (null? k)
             (not (vector? (car k)))
             (let* ([n (- total-field-count
                          n
                          (if auto
                              (car auto)
                              0))]
                    [vec (car k)]
                    [len (vector-length vec)])
               (let loop ([i 0])
                 (or (= i len)
                     (let ([m (vector-ref vec i)])
                       (and (exact-nonnegative-integer? m) ; in case the vector is mutated
                            (< m n)
                            (loop (fx1+ i)))))))))))]))

;; Convert a prefab key to normalized, compact from
(define (normalize-prefab-key k keep-count?)
  (cond
   [(symbol? k) k]
   [else
    (let* ([name (car k)]
           [k (cdr k)]
           [count (if (and (pair? k)
                           (exact-nonnegative-integer? (car k)))
                      (car k)
                      #f)]
           [k (if count
                  (cdr k)
                  k)]
           [auto (if (and (pair? k)
                          (pair? (car k)))
                     (car k)
                     #f)]
           [k (if auto
                  (cdr k)
                  k)]
           [mutables (if (and (pair? k)
                              (vector? (car k)))
                         (car k)
                         #f)]
           [k (if mutables
                  (cdr k)
                  k)]
           [norm-auto (cond
                       [(not auto) #f]
                       [(eq? 0 (car auto)) #f]
                       [else auto])]
           [norm-mutables (cond
                           [(not mutables) #f]
                           [(zero? (vector-length mutables)) #f]
                           [else
                            (vector->immutable-vector
                             (chez:vector-sort (lambda (a b)
                                                 ;; Double-check exact integers, just in case
                                                 ;; a mutation happens; we'll have tou double-check
                                                 ;; that the result is still a prefab
                                                 (if (and (exact-nonnegative-integer? a)
                                                          (exact-nonnegative-integer? b))
                                                     (< a b)
                                                #f))
                                               mutables))])]
           [r (if (null? k)
                  '()
                  (normalize-prefab-key k #t))]
           [r (if norm-mutables
                  (cons norm-mutables
                        r)
                  r)]
           [r (if norm-auto
                  (cons norm-auto r)
                  r)])
      (if keep-count?
          (cons name (cons count r))
          (if (null? r)
              name
              (cons name r))))]))

(define (normalized-prefab-key/check who key field-count)
  (unless (prefab-key? key)
    (raise-argument-error who "prefab-key?" key))
  (unless (prefab-key-compatible-count? key field-count)
    (raise-arguments-error who
                           "mismatch between prefab key and field count"
                           "prefab key" key
                           "field count" field-count))
  (let ([norm-key (normalize-prefab-key key #f)])
    (unless (and (prefab-key? norm-key)
                 (prefab-key-compatible-count? norm-key field-count))
      (raise-arguments-error who
                             "prefab key mutated after initial check"
                             "prefab key" key))
    norm-key))

(define (prefab-key+size->prefab-key-tail key+size)
  (let ([key (car key+size)])
    (cond
     [(symbol? key)
      (list key (cdr key+size))]
     [else
      (cons* (car key)
             (- (cdr key+size)
                (prefab-key-count-explicit-fields key))
             (cdr key))])))

(define (prefab-key-count-explicit-fields key)
  ;; Count fields other than initial non-auto:
  (let loop ([k (cdr key)])
    (let* ([count (and (pair? k)
                       (exact-integer? (car k))
                       (car k))]
           [k (if count
                  (cdr k)
                  k)]
           [mutable (and (pair? k)
                         (pair? (car k))
                         (car k))]
           [k (if mutable
                  (cdr k)
                  k)]
           [k (if (and (pair? k)
                       (vector? (car k)))
                  (cdr k)
                  k)])
      (+ (or count 0)
         (if mutable (car mutable) 0)
         (cond
          [(null? k) 0]
          [else (loop (cdr k))])))))

(define (prefab-key->parent-prefab-key+count key)
  (cond
   [(symbol? key) #f]
   [else
    (let* ([k (cdr key)] ; skip name; non-auto count will no be present
           [k (if (and (pair? k)
                       (pair? (car k)))
                  (cdr k)
                  k)]
           [k (if (and (pair? k)
                       (vector? (car k)))
                  (cdr k)
                  k)])
      (if (null? k)
          #f
          ;; Normalize parent by dropping auto field count out:
          (let* ([name (car k)]
                 [count (cadr k)]
                 [rest-k (cddr k)]
                 [total-count (prefab-key-count-explicit-fields k)])
            (cond
             [(null? rest-k)
              (cons name total-count)]
             [else
              (cons (cons name rest-k) total-count)]))))]))
