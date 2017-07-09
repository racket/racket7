
(define the-will-guardian (make-guardian))

;; Guardian callbacks are called fifo, but will executors are called
;; lifo. The `will-stacks` function maps a finalized value to a list
;; of finalizers, where each finalizer is an ephemeron pairing a will
;; executor with a will function (so that the function is not retained
;; if the will executor is dropped)
(define will-stacks (make-weak-eq-hashtable))

(define-record-type (will-executor create-will-executor will-executor?)
  (fields guardian (mutable ready)))

(define (make-will-executor)
  (create-will-executor the-will-guardian '()))

;; A "stubborn" will executor corresponds to an ordered guardian. It
;; doesn't need to make any guarantees about order for multiple
;; registrations, so use a fresh guardian each time.
(define (make-stubborn-will-executor)
  (create-will-executor (make-guardian #t) '()))

(define/who (will-register executor v proc)
  (check who will-executor? executor)
  (check who (procedure-arity-includes/c 1) proc)
  (disable-interrupts)
  (let ([l (hashtable-ref will-stacks v '())]
        ;; but using an ephemeron pair, if the excutor becomes
        ;; unreachable, then we can drop the finalizer procedure. That
        ;; pattern prevents unbreakable cycles by an untrusted process
        ;; that has no access to a will executor that outlives the
        ;; process.
        [e+proc (ephemeron-cons executor proc)])
    (hashtable-set! will-stacks v (cons e+proc l))
    (when (null? l)
      ((will-executor-guardian executor) v)))
  (enable-interrupts)
  (void))

(define/who will-try-execute
  (case-lambda
   [(executor) (will-try-execute executor #f)]
   [(executor default)
    (check who will-executor? executor)
    (disable-interrupts)
    ;; Poll the guard (which is possibly shared among will executors)
    ;; for ready values, and add any ready value to the receiving will
    ;; executor
    (let ([guardian (will-executor-guardian executor)])
      (let loop ()
        (let ([v (guardian)])
          (when v
            (let we-loop ([l (hashtable-ref will-stacks v '())])
              (when (pair? l)
                (let* ([e+proc (car l)]
                       [e (car e+proc)]
                       [proc (cdr e+proc)]
                       [l (cdr l)])
                  (cond
                   [(eq? #!bwp e)
                    ;; The will executor became inaccesible, so continue looking
                    (we-loop l)]
                   [else
                    (cond
                     [(null? l)
                      (hashtable-delete! will-stacks v)]
                     [else
                      ;; Re-finalize for the next will registration
                      (hashtable-set! will-stacks v l)
                      (guardian v)])
                    (will-executor-ready-set! e (cons (cons v proc) (will-executor-ready e)))]))))
            (loop)))))
    (let ([l (will-executor-ready executor)])
      (cond
       [(pair? l)
        (will-executor-ready-set! executor (cdr l))
        (enable-interrupts)
        ((cdar l) (caar l))]
       [else
        (enable-interrupts)
        default]))]))
