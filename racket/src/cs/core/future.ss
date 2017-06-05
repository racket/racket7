;; Futures API
(define-record-type (future* make-future future?)
  (fields id would-be? (mutable thunk) (mutable engine) (mutable result) (mutable done?) cond lock))

(define ID 1) ;; this actually needs a lock.
(define (get-next-id)
  (let ([id ID])
    (set! ID (+ 1 id))
    id))

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

  (let* ([f (make-future (get-next-id) #f (void) (void) (void) #f (make-condition) (make-lock))]
	 [th (thunk-wrapper f thunk)])
    (future*-engine-set! f (make-engine th #f #t))
    (fprintf (current-error-port) "About to schedule a future\n")
    (schedule-future f)
    (fprintf (current-error-port) "Just made a future\n")
    f))

(define (would-be-future thunk)
  (let* ([f (make-future (get-next-id) #t (void) (void) (void) #f (make-condition) (make-lock))]
	 [th (thunk-wrapper f thunk)])
    (future*-thunk-set! f th)
    f))

(define (touch f)
  (cond
   [(future*-would-be? f)
    ((future*-thunk f))
    (future*-result f)]
   [(future*-done? f)
    (future*-result f)]
   [(lock-acquire (future*-lock f) #f)
    (condition-wait (future*-cond f) (future*-lock f)) ;; when this returns we will have result
    (let ([result (future*-result f)])
      (lock-release (future*-lock f))
      result)] ;; acquired 
   [else
    (touch f)])) ;; not acquired. might be writing result now.

(define (futures-enabled?)
  (threaded?))

(define current-future
  (if (threaded?)
      (chez:make-thread-parameter #f)
      (chez:make-parameter #f)))

;; future? defined by record.

#| Chez doesn't seem to have a built in function that does this.
   Can call out to C in chez, so maybe can just duplicate what
   racket currently does in C.
|#
(define (processor-count)
  0)

