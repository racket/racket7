#lang racket/base
(require "check.rkt"
         (rename-in "semaphore.rkt"
                    [semaphore-peek-evt raw:semaphore-peek-evt])
         (rename-in "evt.rkt"
                    [wrap-evt raw:wrap-evt]
                    [handle-evt raw:handle-evt]
                    [poll-guard-evt raw:poll-guard-evt]
                    [choice-evt raw:choice-evt])
         (rename-in "channel.rkt"
                    [channel-put-evt raw:channel-put-evt])
         (only-in "sync.rkt"
                  sync/enable-break))

(provide wrap-evt
         handle-evt
         guard-evt
         poll-guard-evt
         nack-guard-evt
         choice-evt
         channel-put-evt
         semaphore-peek-evt
         semaphore-wait/enable-break
         call-with-semaphore
         call-with-semaphore/enable-break)

(define (choice-evt . args)
  (for ([arg (in-list args)])
    (check 'choice-evt evt? arg))
  (raw:choice-evt args))

(define (wrap-evt evt proc)
  (check 'wrap-evt evt? evt)
  (check 'wrap-evt procedure? proc)
  (raw:wrap-evt evt proc))

(define (handle-evt evt proc)
  (check 'handle-evt evt? evt)
  (check 'handle-evt procedure? proc)
  (raw:handle-evt evt proc))

(define (guard-evt proc)
  (check 'guard-evt
         (lambda (v) (and (procedure? v) (procedure-arity-includes? v 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (raw:poll-guard-evt (lambda (poll?) (proc))))

(define (poll-guard-evt proc)
  (check 'poll-guard-evt
         (lambda (v) (and (procedure? v) (procedure-arity-includes? v 1)))
         #:contract "(procedure-arity-includes?/c 1)"
         proc)
  (raw:poll-guard-evt proc))

(define (nack-guard-evt proc)
  (check 'nack-guard-evt
         (lambda (v) (and (procedure? v) (procedure-arity-includes? v 1)))
         #:contract "(procedure-arity-includes?/c 1)"
         proc)
  (raw:poll-guard-evt
   (lambda (poll?)
     (define s (make-semaphore))
     ;; Return control-state-evt to register
     ;; the nack semaphore before exposing it to
     ;; the `proc` callback:
     (control-state-evt
      (raw:poll-guard-evt
       (lambda (poll?)
         (define v (proc (raw:semaphore-peek-evt s)))
         (if (evt? v)
             v
             (wrap-evt always-evt (lambda () v)))))
      void
      (lambda () (semaphore-post s))
      void))))

(define (channel-put-evt ch v)
  (check 'channel-put-evt channel? ch)
  (raw:channel-put-evt ch v))

(define (semaphore-peek-evt s)
  (check 'semaphore-peek-evt semaphore? s)
  (raw:semaphore-peek-evt s))

(define (semaphore-wait/enable-break s)
  (check 'semaphore-wait/enable-break semaphore? s)
  (sync/enable-break s)
  (void))

;; ----------------------------------------

(define (do-call-with-semaphore who s proc try-fail args #:enable-break? [enable-break? #f])
  (check who semaphore? s)
  (check who procedure? proc)
  (check who (lambda (p) (or (not try-fail)
                             (and (procedure? try-fail)
                                  (procedure-arity-includes? try-fail 0))))
         #:contract "(or/c #f (procedure-arity-includes/c 0))"
         try-fail)
  (define breaks-on? (or enable-break?
                         (break-enabled)))
  (define results #t) ; transitions to list of results unless semaphore-try fails
  (dynamic-wind
   (lambda ()
     (if try-fail
         (set! results (semaphore-try-wait? s))
         (if breaks-on?
             (semaphore-wait/enable-break s)
             (semaphore-wait s))))             
   (lambda ()
     (when results
       (call-with-continuation-barrier
        (lambda ()
          (set! results
                (call-with-values (lambda () (apply proc args)) list))))))
   (lambda ()
     (when results
       (semaphore-post s))))
  (if results
      (apply values results)
      (try-fail)))

(define (call-with-semaphore s proc [try-fail #f] . args)
  (do-call-with-semaphore 'call-with-semaphore s proc try-fail args))

(define (call-with-semaphore/enable-break s proc [try-fail #f] . args)
  (do-call-with-semaphore 'call-with-semaphore/enable-break s proc try-fail args #:enable-break? #t))
