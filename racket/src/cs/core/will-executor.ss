
(define the-will-guardian (make-guardian))

;; Guardian callbacks are called fifo, but will executors
;; are called lifo
(define will-stacks (make-weak-eq-hashtable))

(define-record-type (will-executor create-will-executor will-executor?)
  (fields (mutable ready)))

(define (make-will-executor)
  (create-will-executor '()))

(define/who (will-register executor v proc)
  (check who will-executor? executor)
  (disable-interrupts)
  (let ([l (hashtable-ref will-stacks v '())]
        ;; If the excutor becomes unreachable, then
        ;; we can drop the finalizer procedure
        [e+proc (ephemeron-cons executor proc)])
    (hashtable-set! will-stacks v (cons e+proc l))
    (when (null? l)
      (the-will-guardian v)))
  (enable-interrupts)
  (void))

(define/who will-try-execute
  (case-lambda
   [(executor) (will-try-execute executor #f)]
   [(executor default)
    (check who will-executor? executor)
    (disable-interrupts)
    (let loop ()
      (let ([v (the-will-guardian)])
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
                    (the-will-guardian v)])
                  (will-executor-ready-set! e (cons (cons v proc) (will-executor-ready e)))]))))
          (loop))))
    (let ([l (will-executor-ready executor)])
      (cond
       [(pair? l)
        (will-executor-ready-set! executor (cdr l))
        (enable-interrupts)
        ((cdar l) (caar l))]
       [else
        (enable-interrupts)
        default]))]))
