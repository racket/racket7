#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "engine.rkt"
         "tree.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "semaphore.rkt"
         "thread-group.rkt"
         "atomic.rkt"
         "schedule-info.rkt")

(provide (rename-out [make-thread thread])
         thread?
         current-thread
         
         thread-running?
         thread-dead?
         
         thread-wait
         thread-suspend
         (rename-out [get-thread-dead-evt thread-dead-evt])
         
         break-thread
         kill-thread
         
         sleep
         
         check-for-break
         break-enabled-key
         
         thread-push-kill-callback!
         thread-pop-kill-callback!
         
         thread-internal-suspend!
         thread-internal-resume!
         thread-yield)

;; Exports needed by "schedule.rkt":
(module* scheduling #f
  (provide (struct-out thread)

           make-initial-thread
           thread-dead!
           thread-did-work!
           
           thread-internal-resume!

           sleeping-threads
           poll-done-threads
           
           check-for-break))

;; ----------------------------------------

(struct thread (name
                [engine #:mutable]
                parent
                [sleep-until #:mutable] ; non-#f => in `sleeping-threads`
                [sched-info #:mutable]

                suspend-to-kill?
                [kill-callbacks #:mutable] ; list of callbacks
                
                [interrupt-callback #:mutable] ; non-#f when suspended
                
                [dead-sema #:mutable] ; created on demand
                [dead-evt #:mutable] ; created on demand
                [suspended? #:mutable]
                [suspended-sema #:mutable]

                [delay-break-for-retry? #:mutable] ; => continuing implies an retry action
                [pending-break? #:mutable])
        #:property prop:waiter
        (make-waiter-methods 
         #:suspend! (lambda (t i-cb r-cb) (thread-internal-suspend! t #f i-cb r-cb))
         #:resume! (lambda (t v) (thread-internal-resume! t) v))
        #:property prop:evt (lambda (t) (wrap-evt (get-thread-dead-evt t)
                                             (lambda (v) t))))

;; ----------------------------------------
;; Thread creation

(define (do-make-thread who
                        proc
                        #:initial? [initial? #f]
                        #:suspend-to-kill? [suspend-to-kill? #f])
  (check who
         (lambda (proc)
           (and (procedure? proc)
                (procedure-arity-includes? proc 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (define p (current-thread-group))
  (define e (make-engine proc (if initial?
                                  break-enabled-default-cell
                                  (current-break-enabled-cell))))
  (define t (thread (gensym)
                    e
                    p
                    #f ; sleep-until
                    #f ; sched-info
                    
                    suspend-to-kill?
                    null
                    
                    #f ; interrupt-callback
                    
                    #f ; dead-sema
                    #f ; dead-evt
                    #f ; suspended?
                    #f ; suspended-sema
                    
                    #f ; delay-break-for-retry?
                    #f)) ; pending-break?
  (thread-group-add! p t)
  t)

(define make-thread
  (let ([thread (lambda (proc)
                  (do-make-thread 'thread proc))])
    thread))

(define (thread/suspend-to-kill proc)
  (do-make-thread 'thread/suspend-to-kill proc #:suspend-to-kill? #t))

(define (make-initial-thread thunk)
  (do-make-thread 'thread thunk #:initial? #t))

;; ----------------------------------------
;; Thread status

(define (thread-running? t)
  (check 'thread-running? thread? t)
  (and (not (eq? 'done (thread-engine t)))
       (not (thread-suspended? t))))

(define (thread-dead? t)
  (check 'thread-dead? thread? t)
  (eq? 'done (thread-engine t)))

;; In atomic mode
(define (thread-dead! t)
  (set-thread-engine! t 'done)
  (when (thread-dead-sema t)
    (semaphore-post-all (thread-dead-sema t)))
  (cond
   [(thread-interrupt-callback t)
    => (lambda (interrupt)
         (set-thread-interrupt-callback! t void)
         (interrupt))]
   [else
    (thread-group-remove! (thread-parent t) t)])
  (remove-from-sleeping-threads! t)
  (run-kill-callbacks! t))

;; ----------------------------------------
;; Thread termination

;; Called in atomic mode:
(define (thread-push-kill-callback! cb)
  (define t (current-thread))
  (set-thread-kill-callbacks! t (cons cb (thread-kill-callbacks t))))

;; Called in atomic mode:
(define (thread-pop-kill-callback!)
  (define t (current-thread))
  (set-thread-kill-callbacks! t (cdr (thread-kill-callbacks t))))

(define (kill-thread t)
  (check 'kill-thread thread? t)
  (atomically
   (unless (thread-dead? t)
     (thread-dead! t)))
  (when (eq? t (current-thread))
    (engine-block)))

(define (run-kill-callbacks! t)
  (for ([cb (in-list (thread-kill-callbacks t))])
    (cb))
  (set-thread-kill-callbacks! t null))

;; ----------------------------------------
;; Thread status events

(define (thread-wait t)
  (check 'thread-wait thread? t)
  (semaphore-wait (get-thread-dead-sema t)))

(struct dead-evt (sema)
        #:property prop:evt (lambda (tde) (wrap-evt (dead-evt-sema tde)
                                               (lambda (s) tde)))
        #:reflection-name 'thread-dead-evt)

(define get-thread-dead-evt
  (let ([thread-dead-evt
         (lambda (t)
           (check 'thread-dead-evt thread? t)
           (atomically
            (unless (thread-dead-evt t)
              (set-thread-dead-evt! t (dead-evt (get-thread-dead-sema t)))))
           (thread-dead-evt t))])
    thread-dead-evt))

(define (get-thread-dead-sema t)
  (atomically
   (unless (thread-dead-sema t)
     (set-thread-dead-sema! t (make-semaphore 0))
     (when (eq? 'done (thread-engine t))
       (semaphore-post-all (thread-dead-sema t)))))
  (thread-dead-sema t))

;; ----------------------------------------
;; Thread suspend and resume

;; A tree mapping times (in milliseconds) to a hash table of threads
;; to wake up at that time
(define sleeping-threads empty-tree)

;; in atomic mode
(define (remove-from-sleeping-threads! t)
  (define sleep-until (thread-sleep-until t))
  (when sleep-until
    (set-thread-sleep-until! t #f)
    (define threads (tree-ref sleeping-threads sleep-until <))
    (unless threads (internal-error "thread not found among sleeping threads"))
    (define new-threads (hash-remove threads t))
    (set! sleeping-threads
          (if (zero? (hash-count new-threads))
              (tree-remove sleeping-threads sleep-until <)
              (tree-set sleeping-threads sleep-until new-threads <)))))

;; in atomic mode
(define (add-to-sleeping-threads! t timeout-at)
  (set-thread-sleep-until! t timeout-at)
  (set! sleeping-threads
        (tree-set sleeping-threads
                  timeout-at
                  (hash-set (or (tree-ref sleeping-threads timeout-at <)
                                #hasheq())
                            t
                            #t)
                  <)))

;; Removes a thread from its thread group, so it won't be scheduled,
;; and returns a thunk to be called in out of atomic mode to swap out
;; the thread, and the thunk returns `(void)`
(define (thread-internal-suspend! t timeout-at interrupt-callback retry-callback)
  (atomically
   (set-thread-interrupt-callback! t interrupt-callback)
   (thread-group-remove! (thread-parent t) t)
   (when timeout-at
     (add-to-sleeping-threads! t timeout-at))
   (when (eq? t (current-thread))
     (thread-did-work!))
   ;; It's ok if the thread gets interrupted
   ;; outside the atomic region, because we'd
   ;; swap it out anyway
   (lambda ()
     ;; In non-atomic mode:
     (when (eq? t (current-thread))
       (engine-block)
       (check-for-retrying-break t retry-callback)))))

;; Check for a break signal that woke up an operation that needs a
;; specific retry action:
(define (check-for-retrying-break t retry-callback)
  ((atomically
    (cond
     [(thread-delay-break-for-retry? t)
      ;; Since the callback wasn't cleared, the thread
      ;; was woken up to check for a break
      (set-thread-delay-break-for-retry?! t #f)
      (thread-did-work!)
      (lambda ()
        ;; In non-atomic mode:
        (check-for-break)
        (retry-callback))]
     [else void]))))

;; Add a thread back to its thread group
(define (thread-internal-resume! t #:undelay-break? [undelay-break? #t])
  (when (thread-dead? t)
    (internal-error "tried to resume a dead thread"))
  (set-thread-interrupt-callback! t #f)
  (when undelay-break?
    (set-thread-delay-break-for-retry?! t #f))
  (remove-from-sleeping-threads! t)
  (thread-group-add! (thread-parent t) t))

(define (thread-suspend t)
  (check 'thread-suspend thread? t)
  ((atomically
    (unless (thread-suspended? t)
      (set-thread-suspended?! t #t)
      (when (thread-suspended-sema t)
        (semaphore-post-all (thread-suspended-sema t))
        (set-thread-suspended-sema! t #f)))
    (cond
     [(thread-parent t) ; => not internally suspended already
      (thread-internal-suspend! t #f void void)]
     [else void]))))

;; ----------------------------------------
;; Thread yielding

;; Pause the current thread to let other threads run. If all threads
;; are paused, then `sched-info` contains information (such as a
;; timeout for the current thread's sleep) needed for a global sleep
(define (thread-yield sched-info)
  (atomically
   (cond
    [(or (not sched-info)
         (schedule-info-did-work? sched-info))
     (thread-did-work!)]
    [else (thread-did-no-work!)])
   (set-thread-sched-info! (current-thread) sched-info))
  (engine-block))

;; Sleep for a while
(define (sleep [secs 0])
  (check 'sleep
         (lambda (c) (and (real? c) (c . >=  . 0)))
         #:contract "(>=/c 0)"
         secs)
  (define until-msecs (+ (* secs 1000.0)
                         (current-inexact-milliseconds)))
  (let loop ()
    ((thread-internal-suspend! (current-thread)
                               until-msecs
                               void
                               (lambda ()
                                 ;; Woke up due to an ignored break?
                                 ;; Try again:
                                 (loop))))))

;; ----------------------------------------
;; Tracking thread progress

;; If a thread does work before it is swapped out, then we should poll
;; all threads again. Accumulate a table of threads that we don't need
;; to poll because we've tried them since the most recent thread
;; performed work:
(define poll-done-threads #hasheq())

(define (thread-did-no-work!)
  (set! poll-done-threads (hash-set poll-done-threads (current-thread) #t)))

(define (thread-did-work!)
  (set! poll-done-threads #hasheq()))

;; ----------------------------------------
;; Breaks

;; The host implementation of `dynamic-wind` is expected to cooperate
;; with the implementation of breaks in terms of `break-enabled-key`
;; and boolean-valued, preserved thread cells. That's cooperattion is
;; awkward, in the sense that it defies the intended layering of
;; subsystems, but it allows the pre and post thunks of `dynamic-wind`
;; to reliably run with breaks disabled (especially during the
;; transition from one thunk to another during a jump).

;; A continuation-mark key (not made visible to regular Racket code):
(define break-enabled-default-cell (make-thread-cell #t #t))

(define (current-break-enabled-cell)
  (continuation-mark-set-first #f
                               break-enabled-key
                               break-enabled-default-cell
                               (root-continuation-prompt-tag)))

;; When the continuation-mark mapping to `break-enabled-key` is
;; changed, or when a thread is just swapped in, then
;; `check-for-break` should be called.
(define (check-for-break)
  (define t (current-thread))
  ((atomically
    (cond
     [(and (thread-pending-break? t)
           (thread-cell-ref (current-break-enabled-cell))
           ;; If delaying for retry, then defer
           ;; break checking to the continuation (instead
           ;; of raising an asynchronous exception now)
           (not (thread-delay-break-for-retry? t)))
      (set-thread-pending-break?! t #f)
      (lambda ()
        ;; Out of atomic mode
        (call-with-escape-continuation
         (lambda (k)
           (raise (exn:break/non-engine
                   "user break"
                   (current-continuation-marks)
                   k)))))]
     [else void]))))

(define (break-thread t)
  (check 'break-thread thread? t)
  (atomically
   (unless (thread-pending-break? t)
     (set-thread-pending-break?! t #t)
     (cond
      [(thread-interrupt-callback t)
       => (lambda (interrupt-callback)
            ;; The interrupt callback might remove the thread as
            ;; a waiter on a semaphore of channel; if breaks
            ;; turn out to be disabled, the wait will be
            ;; retried through the retry callback
            (thread-internal-resume! t #:undelay-break? #f)
            (interrupt-callback))])))
  (when (eq? t (current-thread))
    (check-for-break)))
