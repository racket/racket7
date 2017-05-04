#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "waiter.rkt"
         "queue.rkt"
         "evt.rkt")

(provide make-semaphore
         semaphore?
         semaphore-post
         semaphore-post-all
         semaphore-wait
         
         semaphore-peek-evt
         semaphore-peek-evt?
         
         semaphore-any-waiters?)

(struct semaphore ([count #:mutable]
                   queue)
        #:property
        prop:evt
        (poller (lambda (s poll-ctx)
                  (semaphore-wait/poll s poll-ctx))))

(struct semaphore-peek-evt (sema)
        #:property
        prop:evt
        (poller (lambda (sp poll-ctx)
                  (semaphore-wait/poll (semaphore-peek-evt-sema sp)
                                       poll-ctx
                                       #:peek? #t
                                       #:result sp))))

(struct semaphore-peek-select-waiter select-waiter ())

(define (make-semaphore [init 0])
  (check 'make-semaphore exact-nonnegative-integer? init)
  (semaphore init (make-queue)))

;; ----------------------------------------

(define (semaphore-post s)
  (check 'semaphore-post semaphore? s)
  (atomically
   (let loop ()
     (define w (queue-remove! (semaphore-queue s)))
     (cond
      [(not w)
       (set-semaphore-count! s (add1 (semaphore-count s)))]
      [else
       (waiter-resume! w s)
       (when (semaphore-peek-select-waiter? w)
         ;; Don't consume a post for a peek waiter
         (loop))]))))      

(define (semaphore-post-all s)
  (atomically
   (set-semaphore-count! s +inf.0)
   (queue-remove-all!
    (semaphore-queue s)
    (lambda (w) (waiter-resume! w s)))))

;; In atomic mode:
(define (semaphore-any-waiters? s)
  (not (queue-empty? (semaphore-queue s))))

;; ----------------------------------------

(define (semaphore-wait s)
  (check 'semaphore-wait semaphore? s)
  ((atomically
    (define c (semaphore-count s))
    (cond
     [(positive? c)
      (set-semaphore-count! s (sub1 c))
      void]
     [else
      (define w (current-thread))
      (define q (semaphore-queue s))
      (define n (queue-add! q w))
      (let retry ()
        (waiter-suspend!
         w
         ;; On break signal (possibly to be ignored) or kill:
         (lambda ()
           (queue-remove-node! q n))
         ;; This callback is used, in addition to the previous one, if
         ;; the thread receives a break signal but doesn't escape
         ;; (either because breaks are disabled or the handler
         ;; continues):
         (lambda () (semaphore-wait s))))]))))

(define (semaphore-wait/poll s poll-ctx
                             #:peek? [peek? #f]
                             #:result [result s])
  ;; Similar to `semaphore-wait, but as called by `sync`,
  ;; so use a select waiter instead of the current thread
  (define c (semaphore-count s))
  (cond
   [(positive? c)
    (unless peek?
      (set-semaphore-count! s (sub1 c)))
    (values (list result) #f)]
   [(poll-ctx-poll? poll-ctx)
    (values #f never-evt)]
   [else
    (define w (if peek?
                  (semaphore-peek-select-waiter (poll-ctx-select-proc poll-ctx))
                  (select-waiter (poll-ctx-select-proc poll-ctx))))
    (define q (semaphore-queue s))
    (define n (queue-add! q w))
    ;; Replace with `async-evt`, but the `sema-waiter` can select the
    ;; event through a callback. Pair the event with a nack callback
    ;; to get back out of line.
    (values #f
            (wrap-evt
             (control-state-evt async-evt
                                (lambda () (queue-remove-node! q n))
                                void
                                (lambda ()
                                  ;; Retry: decrement or requeue
                                  (define c (semaphore-count s))
                                  (cond
                                   [(positive? c)
                                    (unless peek?
                                      (set-semaphore-count! s (sub1 c)))
                                    (values result #t)]
                                   [else
                                    (set! n (queue-add! q w))
                                    (values #f #f)])))
             (lambda (v) result)))]))
