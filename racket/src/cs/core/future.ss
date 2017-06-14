;; Futures API

(define ID 1)

(define id-lock (make-lock #f))

(define (get-next-id)
  (lock-acquire id-lock)
  (let ([id ID])
    (set! ID (+ 1 id))
    (lock-release id-lock)
    id))

(define-record-type (future* make-future future?)
    (fields id would-be? (mutable thunk) (mutable engine) 
	    (mutable result) (mutable done?) cond lock))
;; future? defined by record.

(define (futures-enabled?)
  (threaded?))

#| Chez doesn't seem to have a built in function that does this.
Can call out to C in chez, so maybe can just duplicate what
racket currently does in C.
|#
(define (processor-count)
  1)

(define current-future (internal-make-thread-parameter #f))

(meta-cond
 [(threaded?)

  (define (thunk-wrapper f thunk)
    (lambda ()
      (let ([result (thunk)])
	(lock-acquire (future*-lock f))
	(future*-result-set! f result)
	(future*-done?-set! f #t)
	(condition-signal (future*-cond f))
	(lock-release (future*-lock f)))))

  (define (future thunk)
    (unless (scheduler-running?)
	    (start-scheduler))
    
    (let* ([f (make-future (get-next-id) #f (void) (void) (void) #f (make-condition) (make-lock #f))]
	   [th (thunk-wrapper f thunk)])
      (future*-engine-set! f (make-engine th #f #t))
      (schedule-future f)
      f))

  (define (would-be-future thunk)
    (let* ([f (make-future (get-next-id) #t (void) (void) (void) #f (void) (make-lock #f))]
	   [th (thunk-wrapper f thunk)])
      (future*-thunk-set! f th)
      f))
  
  (define (touch f)
    (cond
     [(future*-would-be? f)
      ((future*-thunk f))
      (future*-result f)]
     [(future? (current-future)) ;; are we running in a future?
      (touch-in-future f)]
     [(future*-done? f)
      (future*-result f)]
     [(lock-acquire (future*-lock f) #f)
      (condition-wait (future*-cond f) (future*-lock f)) ;; when this returns we will have result
      (let ([result (future*-result f)])
	(lock-release (future*-lock f))
	result)] ;; acquired 
     [else
      (touch f)])) ;; not acquired. might be writing result now.

  (define (touch-in-future f)
    (cond
     [(future*-done? f)
      (future*-result f)]
     [else
      (touch-in-future f)]))
  ]
 [else
  ;; not threaded
  
  (define (thunk-wrapper f thunk)
    (lambda ()
      (let ([result (thunk)])
	(future*-result-set! f result)
	(future*-done?-set! f #t) )))

  (define (future thunk)
    (would-be-future thunk))

  (define (would-be-future thunk)
    (let* ([f (make-future (get-next-id) #t (void) (void) (void) #f (void) (make-lock #f))]
	   [th (thunk-wrapper f thunk)])
      (future*-thunk-set! f th)
      f))

  (define (touch f)
    ((future*-thunk f))
    (future*-result f))
  
  ])
