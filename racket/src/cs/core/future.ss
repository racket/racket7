;; Futures API
(define-record-type (future* make-future future?)
  (fields id (mutable engine) (mutable result) (mutable done?) cond lock))

(define ID 1)
(define (get-next-id)
  (let ([id ID])
    (set! ID (+ 1 id))
    id))

(define (thunk-wrapper f thunk)
  (lambda ()
    (let ([result (thunk)])
      (mutex-acquire (future*-lock f))
      (future*-result-set! f result)
      (future*-done?-set! f #t)
      (condition-signal (future*-cond f))
      (mutex-release (future*-lock f)))))

(define (future thunk)
  (let* ([f (make-future (get-next-id) (void) (void) #f (make-condition) (make-mutex))]
	 [th (thunk-wrapper f thunk)])
    (future*-engine-set! f (make-engine th #f)) ;; what to put instead of #f?
    (schedule-future f)
    f))

(define (touch f)
  (cond
   [(future*-done? f)
    (future*-result f)]
   [(mutex-acquire (future*-lock f) #f)
    (condition-wait (future*-cond f) (future*-lock f)) ;; when this returns we will have result
    (let ([result (future*-result f)])
      (mutex-release (future*-lock f))
      result)] ;; acquired 
   [else
    (touch f)])) ;; not acquired. might be writing result now.

(define (futures-enabled?)
  (threaded?))

#| How this works currently is racket threads store the current future.
   I could duplicate this by adding a field to the new racket threads.
   But I don't know yet how this field would be updated and used.
   When does the current implmenetation change the current future field?
|#
(define (current-future)
  #f)
  ;;(define t (current-thread))
  

;; future? defined by record.

(define (would-be-future thunk)
  (void))

#| Chez doesn't seem to have a built in function that does this.
   Can call out to C in chez, so maybe can just duplicate what
   racket currently does in C.
|#
(define (processor-count)
  0)

;; todo: for/async
;; todo: for*/async


