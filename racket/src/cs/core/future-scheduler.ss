
(define-record-type (worker make-worker worker?)
  (fields id (mutable real-thread) (mutable work-queue) (mutable idle?) lock (mutable die?)))

(define-record-type (scheduler make-scheduler scheduler?)
  (fields (mutable workers) lock))

(define global-scheduler #f)
(define THREAD-COUNT 3)

(define (start-scheduler)
  (cond
   [global-scheduler
    (void)]
   [else
    (set! global-scheduler (make-scheduler #f (make-lock)))
    (let ([workers (create-workers)])
      (scheduler-workers-set! global-scheduler workers)
      (start-workers workers))]))

(define (scheduler-running?)
  (not (not global-scheduler)))

(define (kill-scheduler)
  (for-each (lambda (worker)
	      (lock-acquire (worker-lock worker))
	      (worker-die?-set! worker #t)
	      (lock-release (worker-lock worker)))
	    (scheduler-workers global-scheduler)))

(define (create-workers)
  (map (lambda (id-1)
	 (make-worker (+ 1 id-1) #f (make-queue) #t (make-lock) #f))
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
      (lock-release (worker-lock worker)))]))

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
      (lock-acquire (worker-lock worker)) ;; block
      (cond
       [(worker-die? worker) ;; worker was killed
	(lock-release (worker-lock worker))]
       [(queue-empty? (worker-work-queue worker)) ;; have lock. no work
	(lock-release (worker-lock worker))
	(if (steal-work worker) ;; holding worker lock if this call returns #t
	    (do-work)
	    (begin 
	      (chez:sleep (make-time 'time-duration 1000 0))
	      (loop)))]
       [else ;; have lock. have work
	(do-work)]))

    (define (complete ticks args)
      (void))
 
    (define (expire future worker)
      (lambda (new-eng)
	(lock-acquire (worker-lock worker))
	(future*-engine-set! future new-eng)
	(queue-add! (worker-work-queue worker) future)
	(lock-release (worker-lock worker))))

    ;; need to have lock here.
    (define (do-work)
      (let ([work (queue-remove! (worker-work-queue worker))])
	(current-future work)
	(lock-release (worker-lock worker)) ;; release lock
	((future*-engine work) 100 void complete (expire work worker)) ;; call engine.
	(current-future #f)
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

;; Acquire lock of peer with smallest id # first.
;; worker is attempting to steal work from peers
(define (steal-work worker)
  (let loop ([q (scheduler-workers global-scheduler)])
    (cond
     [(null? q) #f] ;; failed to steal work.
     [(not (eq? (worker-id worker) (worker-id (car q)))) ;; not ourselves
      (let* ([peer (car q)]
	     [first (worker-w-min-id worker peer)] ;; create an odering on peers
	     [sec (if (eq? (worker-id first) (worker-id worker)) 
		      peer
		      worker)])
	(lock-acquire (worker-lock first))
	(lock-acquire (worker-lock sec))
	(cond
	 [(> (queue-length (worker-work-queue peer)) 1) ;; going to steal
	  (do ([i (floor (/ (queue-length (worker-work-queue peer)) 2)) (- i 1)])
	      ((zero? i) (void))
	    (let ([w (queue-remove-end! (worker-work-queue peer))])
	      (queue-add! (worker-work-queue worker) w)))
	  (lock-release (worker-lock peer))
	  #t] ;; stole work
	 [else ;; try a different peer
	  (lock-release (worker-lock worker))
	  (lock-release (worker-lock peer))
	  (loop (cdr q))]))]
     [else (loop (cdr q))])))

