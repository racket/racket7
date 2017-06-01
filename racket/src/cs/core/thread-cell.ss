;; A "thread cell" is actually an "engine cell" at the core level

;; FIXME: need emphemerons for weak mapping in engine thread cells

(define-record-type (thread-cell create-thread-cell thread-cell?)
  (fields default-value preserved?))

(define make-thread-cell
  (case-lambda
    [(v) (make-thread-cell v #f)]
    [(v preserved?) (create-thread-cell v preserved?)]))

(define/who (thread-cell-ref c)
  (check who thread-cell? c)
  (let* ([t (current-engine-thread-cell-values)]
         [v (if t
                (hashtable-ref t c none)
                none)])
    (cond
     [(eq? v none)
      (thread-cell-default-value c)]
     [else v])))

(define/who (thread-cell-set! c v)
  (check who thread-cell? c)
  (hashtable-set! (current-engine-thread-cell-values)
                  c
                  v))

;; ----------------------------------------

(define-record thread-cell-values (t))

(define/who current-preserved-thread-cell-values
  (case-lambda
   [() (make-thread-cell-values (new-engine-thread-cell-values))]
   [(tcvs)
    (check who thread-cell-values? tcvs)
    (set-current-engine-thread-cell-values! (thread-cell-values-t tcvs))]))
