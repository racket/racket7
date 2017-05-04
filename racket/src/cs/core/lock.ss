;; locking code for core-hash.ss

;; https://lwn.net/Articles/590243/
;; Synchronization Without Contention Mellor-Crummey Scott (MCS lock)
;; eventually 
#|
(define-record mcs-spinlock (next locked?))

(define (create-mcs-spinlock) (make-mcs-spinlock #f #f))
|#

(meta-cond
 [(guard (x [#t #t]) (eval 'make-mutex) #f)
  ;; Using a Chez Scheme build without thread support.
  ;; For now, make the lock do nothing (but this lock
  ;; will need to work with Racket's "green" threads
  ;; in the end]
  (define (make-lock) 'none)

  (define (lock-acquire l)
    (void))

  (define (lock-release l)
    (void))]
 [else
  (define (make-lock)
    (make-mutex))

  ;; FIXME: When Racket's "green" threads are implemented,
  ;; this lock acquire will need to cooperate, so that other
  ;; green therads can run if the lock cannot be acquired
  (define (lock-acquire lock)
    (mutex-acquire lock))

  (define (lock-release lock)
    (mutex-release lock))])
