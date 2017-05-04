;; A "thread cell" is actually an "engine cell" at the core level

;; FIXME: need emphemerons for weak mapping in engine thread cells

(define-record-type (thread-cell create-thread-cell thread-cell?)
  (fields default-value preserved?))

(define make-thread-cell
  (case-lambda
    [(v) (make-thread-cell v #f)]
    [(v preserved?) (create-thread-cell v preserved?)]))

(define (thread-cell-ref c)
  (unless (thread-cell? c)
    (raise-argument-error 'thread-cell-ref "thread-cell?" c))
  (let* ([t (current-engine-thread-cell-values)]
         [v (if t
                (hashtable-ref t c none)
                none)])
    (cond
     [(eq? v none)
      (thread-cell-default-value c)]
     [else v])))

(define (thread-cell-set! c v)
  (unless (thread-cell? c)
    (raise-argument-error 'thread-cell-set! "thread-cell?" c))
  (hashtable-set! (current-engine-thread-cell-values)
                  c
                  v))
