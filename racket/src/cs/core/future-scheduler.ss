
(define-record-type (worker make-worker worker?)
  (fields id (mutable real-thread) (mutable work-queue) (mutable idle?) lock (mutable die?)))

(define-record-type (scheduler make-scheduler scheduler?)
  (fields (mutable workers) lock))

(define global-scheduler #f)
(define THREAD-COUNT 2)

(define (start-scheduler)
  (cond
   [global-scheduler
    (void)] ;; assume its aready been started
   [else
    (set! global-scheduler (make-scheduler #f (make-mutex)))
    (let ([workers (init-workers)])
      (scheduler-workers-set! global-scheduler workers))]))

(define (kill-scheduler)
  (for-each (lambda (worker)
	      (with-mutex (worker-lock worker)
			  (worker-die?-set! worker #t)))
	    (scheduler-workers global-scheduler)))

(define (init-workers)
  (map (lambda (id-1)
	 (let ([worker (make-worker (+ 1 id-1) #f (make-queue) #t (make-mutex) #f)])
	   (worker-real-thread-set! worker (fork-thread (worker-scheduler-func worker)))
	   worker))
       (iota THREAD-COUNT)))

(define (schedule-future f)
  (define worker (pick-worker))
  (with-mutex (worker-lock worker)
	      (queue-add! (worker-work-queue worker) f)))

(define (pick-worker)
  (define workers (scheduler-workers global-scheduler))
  (let loop ([workers* (cdr workers)]
	     [best (car workers)])
    (cond
     [(null? workers*)
      best]
     [(< (queue-length (worker-work-queue (car workers*)))
	 (queue-length (worker-work-queue best)))
      (loop (cdr workers*)
	    (car workers*))]
     [else
      (loop (cdr workers*)
	    best)])))

(define (worker-scheduler-func worker)
  (lambda ()
    
    (define (loop)
      (mutex-acquire (worker-lock worker)) ;; block
      (cond
       [(worker-die? worker) ;; worker was killed
	(mutex-release (worker-lock worker))]
       [(queue-empty? (worker-work-queue worker)) ;; have lock. no work
	(mutex-release (worker-lock worker))
	(if (steal-work worker) ;; holding worker lock if this call returns #t
	    (do-work)
	    (begin 
	      (chez:sleep (make-time 'time-duration 0 0))  ;; not sure what sleeping for 0 time does
	      (loop)))]
       [else ;; have lock. have work
	(do-work)]))

    (define (complete ticks args)
      (void))
 
    (define (expire future worker)
      (lambda (new-eng)
	(with-mutex (worker-lock worker)
		    (future*-engine-set! future new-eng)
		    (queue-add! (worker-work-queue worker) future))))

    ;; need to have lock here.
    (define (do-work)
      (let ([work (queue-remove! (worker-work-queue worker))])
	(mutex-release (worker-lock worker)) ;; release lock
	((future*-engine work) 100 void complete (expire work worker)) ;; call engine.
	(loop)))
    
    (loop)))

(define (worker-w-min-id worker1 worker2)
  (define id1 (worker-id worker1))
  (define id2 (worker-id worker2))
  (cond
   [(< id1 id2)
    worker1]
   [else
    worker2]))

;; worker is attempting to steal work from peers
(define (steal-work worker)
  (let loop ([q (scheduler-workers global-scheduler)])
    (cond
     [(null? q)
      #f] ;; failure.
     [(not (eq? (worker-id worker) (worker-id (car q)))) ;; not ourselves
      (let* ([peer (car q)]
	     [first (worker-w-min-id worker peer)] ;; create an odering on peers
	     [sec (if (eq? (worker-id first) (worker-id worker)) 
		      peer
		      worker)])
	(mutex-acquire (worker-lock first))
	(mutex-acquire (worker-lock sec))
	;; have both locks.
	(cond
	 [(> (queue-length (worker-work-queue peer)) 1) ;; going to steal
	  (do ([i (floor (/ (queue-length (worker-work-queue peer)) 2)) (- i 1)])
	      ((zero? i) (void))
	    (let ([w (queue-remove-end! (worker-work-queue peer))])
	      (queue-add! (worker-work-queue worker) w)))
	  (mutex-release (worker-lock peer))
	  #t]
	 [else ;; try a different peer
	  (mutex-release (worker-lock worker))
	  (mutex-release (worker-lock peer))
	  (loop (cdr q))]))]
     [else 
      (loop (cdr q))])))


#| Try a new locking scheme.  Acquire lock of peer with smallest id # first.

   Scenario:   Worker 1 wants to check if it can steal work from Worker 3
               Worker 3 wants to try and steal work from Worker 2
     

               Worker 1 BLOCKS to acquire its own lock.  Then Worker 1 BLOCKS to acquire 3's lock
               Worker 3 BLOCKS to acquire 1's lock.   Then Worker 3 BLOCKS to acquire its own lock.

               No deadlock because locks are acquired in same order.
|#
