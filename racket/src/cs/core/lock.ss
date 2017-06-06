;; locking code for core-hash.ss

(define make-scheduler-lock (lambda () #f))
(define scheduler-lock-acquire (lambda (l) (void)))
(define scheduler-lock-release (lambda (l) (void)))

(define (set-scheduler-lock-callbacks! make acquire release)
  (set! make-scheduler-lock make)
  (set! scheduler-lock-acquire acquire)
  (set! scheduler-lock-release release))

(meta-cond
 [(guard (x [#t #t]) (eval 'make-mutex) #f)
  ;; Using a Chez Scheme build without thread support,
  ;; but we need to cooperate with engine-based threads.

  ;; `eqv?`- and `eq?`-based tables appear to run with
  ;; interrupts disabled, so they're safe for engine-based
  ;; threads; just create a Racket-visible lock for
  ;; `equal?`-based hash tables
  (define (make-lock for-kind)
    (and (eq? for-kind 'equal?)
         (make-scheduler-lock)))

  (define (lock-acquire lock)
    (when lock
      ;; Thread layer sets this callback to wait
      ;; on a semaphore:
      (scheduler-lock-acquire lock)))

  (define (lock-release lock)
    (when lock
      (scheduler-lock-release lock)))]
 [else
  ;; Using a Chez Scheme build with thread support; make hash-table
  ;; access thread-safe at that level for `eq?`- and `eqv?`-based
  ;; tables; an `equal?`-based table is not thread-safe at that
  ;; level, because operations can take unbounded time, and then
  ;; the Racket scheduler could become blocked on a Chez-level mutex
  ;; in the same Chez-level thread.

  ;; Idea: instead of mutexes, which are relatively expansive, use
  ;;  https://lwn.net/Articles/590243/
  ;;  Synchronization Without Contention Mellor-Crummey Scott (MCS lock)
  #|
  (define-record mcs-spinlock (next locked?))

  (define (create-mcs-spinlock) (make-mcs-spinlock #f #f))
  |#

  (define (make-lock for-kind)
    (if (eq? for-kind 'equal?)
        (make-scheduler-lock)
        (make-mutex)))

  (define (lock-acquire lock)
    (if (mutex? lock)
        (mutex-acquire lock)
        (scheduler-lock-acquire lock)))

  (define (lock-release lock)
    (if (mutex? lock)
        (mutex-release lock)
        (scheduler-lock-release lock)))])
