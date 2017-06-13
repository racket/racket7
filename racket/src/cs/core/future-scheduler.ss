
;; A note: conditions only work with mutexes; so this currently works because locks are mutexes
;; but in the future if the locks are changed to something more efficient, conditions will need
;; to be reimplemented.

(meta-cond
 [(threaded?)
  (define global-scheduler #f)
  
  (define (scheduler-running?)
    (not (not global-scheduler)))
  
  (define-record-type (worker make-worker worker?)
    (fields id (mutable real-thread) (mutable work-queue) (mutable idle?) lock cond (mutable die?)))
  
  (define-record-type (scheduler make-scheduler scheduler?)
    (fields (mutable workers) lock))
  
  (define THREAD-COUNT 3)
  (define (start-scheduler)
    (cond
     [global-scheduler
      (void)]
     [else
      (set! global-scheduler (make-scheduler #f (make-lock #f)))
      (let ([workers (create-workers)])
	(scheduler-workers-set! global-scheduler workers)
	(start-workers workers))]))
  
  (define (kill-scheduler)
    (for-each (lambda (worker)
		(lock-acquire (worker-lock worker))
		(worker-die?-set! worker #t)
		(lock-release (worker-lock worker)))
	      (scheduler-workers global-scheduler)))
  
  (define (create-workers)
    (map (lambda (id-1)
	   (make-worker (+ 1 id-1) #f (make-queue) #t (make-lock #f) (make-condition) #f))
	 (iota THREAD-COUNT)))
  
  (define (start-workers workers)
    (for-each 
     (lambda (worker)
       (worker-real-thread-set! worker (fork-thread (worker-scheduler-func worker))))
     workers))
  
  (define (schedule-future f)
    (cond
     [(not global-scheduler)
      (error 'schedule-future "scheduler not running\n")]
     [else
      (let ([worker (pick-worker)])
	(lock-acquire (worker-lock worker))
	(queue-add! (worker-work-queue worker) f)
	(condition-signal (worker-cond worker))
	(lock-release (worker-lock worker)))]))

  (define (pick-worker)
    (define workers (scheduler-workers global-scheduler))
    (let loop ([workers* (cdr workers)]
	       [best (car workers)])
      (cond
       [(or (null? workers*)
	    (queue-empty? (worker-work-queue best)))
	best]
       [(< (queue-length (worker-work-queue (car workers*)))
	   (queue-length (worker-work-queue best)))
	(loop (cdr workers*)
	      (car workers*))]
       [else
	(loop (cdr workers*)
	      best)])))

  (define (wait-for-work worker)
    (define m (worker-lock worker))
    (let try ()
      (cond
       [(not (queue-empty? (worker-work-queue worker))) ;; got work in meantime
	(void)]
       [(lock-acquire m #f) ;; cannot acquire lock while worker is being given work.
	(condition-wait (worker-cond worker) m)
	(lock-release m)]
       [else ;; try to get lock again.
	(try)])))

  (define (worker-scheduler-func worker)
    (lambda ()
      
      (define (loop)
	(lock-acquire (worker-lock worker)) ;; block
	(cond
	 [(worker-die? worker) ;; worker was killed
	  (lock-release (worker-lock worker))]
	 [(queue-empty? (worker-work-queue worker)) ;; have lock. no work
	  (lock-release (worker-lock worker))
	  (cond
	   [(steal-work worker)  
	    (do-work)]
	   [else  
	    (wait-for-work worker) ;; doesn't seem to use cpu!
	    (loop)])]
	 [else  
	  (do-work)
	  (loop)]))
      
      (define (complete ticks args)
	(void))
      
      (define (expire future worker)
	(lambda (new-eng)
	  (lock-acquire (worker-lock worker))
	  (future*-engine-set! future new-eng)
	  (queue-add! (worker-work-queue worker) future)
	  (condition-signal (worker-cond worker)) ;; in case worker went to sleep.
	  (lock-release (worker-lock worker))))
      
      ;; need to have lock here.
      (define (do-work)
	(let ([work (queue-remove! (worker-work-queue worker))])
	  (current-future work)
	  (lock-release (worker-lock worker)) ;; release lock
	  ((future*-engine work) 100 void complete (expire work worker)) ;; call engine.
	  (current-future #f)))
      
      (loop)))

  (define (order-workers w1 w2)
    (cond
     [(< (worker-id w1) (worker-id w2))
      (values w1 w2)]
     [else
      (values w2 w1)]))

  ;; Acquire lock of peer with smallest id # first.
  ;; worker is attempting to steal work from peers
  (define (steal-work worker)
    (let loop ([q (scheduler-workers global-scheduler)])
      (cond
       [(null? q) #f] ;; failed to steal work.
       [(not (eq? (worker-id worker) (worker-id (car q)))) ;; not ourselves
	(let*-values ([(peer) (car q)]
		      [(w1 w2) (order-workers worker peer)]) ;; order them.
	  (lock-acquire (worker-lock w1))
	  (lock-acquire (worker-lock w2))
	  (cond
	   [(> (queue-length (worker-work-queue peer)) 2) ;; going to steal. Should likely made this # higher.
	    (do ([i (floor (/ (queue-length (worker-work-queue peer)) 2)) (- i 1)])
		[(zero? i) (void)]
	      (let ([work (queue-remove-end! (worker-work-queue peer))])
		(queue-add! (worker-work-queue worker) work)))
	    
	    (lock-release (worker-lock peer)) ;; don't want to release our own lock.
	    #t] ;; stole work
	   [else ;; try a different peer
	    (lock-release (worker-lock worker))
	    (lock-release (worker-lock peer))
	    (loop (cdr q))]))]
       [else (loop (cdr q))])))

  ]
 [else
  (void)
  ])
