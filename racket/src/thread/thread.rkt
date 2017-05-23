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
         "schedule-info.rkt"
         "../common/queue.rkt")

(provide (rename-out [make-thread thread])
         thread?
         current-thread
         
         thread-running?
         thread-dead?
         
         thread-wait
         thread-suspend
         (rename-out [get-thread-dead-evt thread-dead-evt])
         thread-dead-evt?
         
         break-thread
         kill-thread
         
         sleep

         break-enabled
         check-for-break
         break-enabled-key
         current-break-suspend
         
         thread-push-kill-callback!
         thread-pop-kill-callback!
         
         thread-internal-suspend!
         thread-internal-resume!
         thread-yield

         thread-ignore-break-cell!
         thread-remove-ignored-break-cell!
         
         thread-send
         thread-receive
         thread-try-receive
         thread-rewind-receive
         )

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

(struct thread node (name
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
                     [pending-break? #:mutable]
                     [ignore-break-cells #:mutable] ; => #f, a single cell, or a set of cells
                     [waiting-mail? #:mutable]
                     [mailbox #:mutable]) ;; mailbox
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
  (define t (thread #f ; node prev
                    #f ; node next
                    
                    (gensym)
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
                    #f ; pending-break?
                    #f ; ignore-thread-cells
                    #f ; waiting for mail?
                    (make-queue))) ; mailbox
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

;; set to be done here........
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

(define (thread-dead-evt? v)
  (dead-evt? v))

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

;; Removes a thread from its thread group, so it won't be scheduled;
;; returns a thunk to be called in out of atomic mode to swap out
;; the thread, where the thunk returns `(void)`; the `interrupt-callback`
;; is called if the thread receives a break signal or is killed; if
;; the break signal is supressed or resumed, then `retry-callback`
;; is called to try again --- but `retry-callback` will only be used
;; if `interrupt-callback` was previously called, and neither is called
;; if the thread is resumed normally instead of by a break signal
(define (thread-internal-suspend! t timeout-at interrupt-callback retry-callback)
  (atomically
   (set-thread-interrupt-callback! t interrupt-callback)
   (thread-group-remove! (thread-parent t) t)
   (when timeout-at
     (add-to-sleeping-threads! t timeout-at))
   (when (eq? t (current-thread))
     (thread-did-work!)
     ;; Don't let a break get handled inplicitly in `(engine-block)`,
     ;; because we want to call `retry-callback` if woken up for a
     ;; break signal:
     (set-thread-delay-break-for-retry?! t #t))
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

;; Add a thread back to its thread group; the `undelay-break?`
;; argument should only be `#f` when `break-thread` wakes up a thread,
;; instead of the normal wake-up action, and it causes the
;; `retry-callback` to be called from `thread-internal-suspend!`
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
     [else 
      void]))))

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

;; For disabling breaks, such as through `unsafe-start-atomic`:
(define break-suspend 0)
(define current-break-suspend
  (case-lambda
    [() break-suspend]
    [(v) (set! break-suspend v)]))

(define (current-break-enabled-cell)
  (continuation-mark-set-first #f
                               break-enabled-key
                               break-enabled-default-cell
                               (root-continuation-prompt-tag)))

(define break-enabled
  (case-lambda
    [() (thread-cell-ref (current-break-enabled-cell))]
    [(on?) (thread-cell-set! (current-break-enabled-cell) on?)]))

;; When the continuation-mark mapping to `break-enabled-key` is
;; changed, or when a thread is just swapped in, then
;; `check-for-break` should be called.
(define (check-for-break)
  (define t (current-thread))
  ((atomically
    (cond
     [(and (thread-pending-break? t)
           (break-enabled)
           (not (thread-ignore-break-cell? t (current-break-enabled-cell)))
           (zero? (current-break-suspend))
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

;; in atomic mode:
(define (thread-ignore-break-cell? t bc)
  (let ([ignore (thread-ignore-break-cells t)])
    (or (eq? ignore bc)
        (and (hash? ignore)
             (hash-ref ignore bc #f)))))

;; in atomic mode:
(define (thread-ignore-break-cell! t bc)
  (let ([ignore (thread-ignore-break-cells t)])
    (set-thread-ignore-break-cells! t (cond
                                        [(not ignore)
                                         ;; Singleton
                                         bc]
                                        [(hash? ignore)
                                         ;; Add to set
                                         (hash-set ignore bc #t)]
                                        [else
                                         ;; Convert to set
                                         (hasheq ignore #t bc #t)]))))

;; in atomic mode
(define (thread-remove-ignored-break-cell! t bc)
  (when (thread-ignore-break-cell? t bc)
    (let ([ignore (thread-ignore-break-cells t)])
      (set-thread-ignore-break-cells! t (cond
                                          [(eq? ignore bc) #f]
                                          [else (hash-remove ignore bc)])))))
;; ----------------------------------------
;; Thread mailboxes

;; called atomically
(define (enqueue-mail! thd v)
  (queue-add! (thread-mailbox thd) v))

;; called atomically
(define (dequeue-mail! thd)
  (define mbx (thread-mailbox thd))
  (cond
    [(queue-empty? mbx)
     (internal-error "No Mail!\n")]
    [else
     (queue-remove! mbx)]))

;; called atomically
(define (is-mail? thd)
  (not (queue-empty? (thread-mailbox thd))))

;; called atomically
(define (push-mail! thd v)
  (queue-add-front! (thread-mailbox thd) v))

(define (thread-send thd v [fail-thunk 
                            (lambda ()
                              (raise-mismatch-error 'thread-send "Thread is not running.\n"))])
  
  (check 'thread-send thread? thd)
  (check fail-thunk
         (lambda (proc)
           (or (not proc) ;; check if it is #f
               (and (procedure? proc)
                    (procedure-arity-includes? proc 0))))
         #:contract "(procedure-arity-includes?/c 0)"
         fail-thunk)
  ((atomically 
    (cond
      [(thread-running? thd)
       (enqueue-mail! thd v)
       (when (and (thread-waiting-mail? thd) ;; check so we don't resume a suspended thread. 
                  (not (thread-suspended? thd)))
         (set-thread-waiting-mail?! thd #f)
         (thread-internal-resume! thd))
       void]
      [fail-thunk
       fail-thunk]
      [else
       (lambda () #f)]))))

(define (block)
  ((thread-internal-suspend! (current-thread) #f void block)))

(define (thread-receive)
  ((atomically
    (define t (current-thread))
    (cond
      [(not (is-mail? t))
       (set-thread-waiting-mail?! t #t)
       (thread-internal-suspend! t #f void block)]
      [else
       void])))
  ;; awoken or there was already mail.
  (define t (current-thread))
  (let loop ()
    ((atomically
      (cond
        [(is-mail? t)
         (let ([msg (dequeue-mail! t)])
           (lambda () msg))]
        [else
         (let ([f (thread-internal-suspend! t #f void block)])
           (lambda ()
             (f)
             (loop)))])))))
 
(define (thread-try-receive)
  (atomically
   (define t (current-thread))
   (if (is-mail? t)
       (dequeue-mail! t)
       #f)))

(define (thread-rewind-receive lst)
  (check 'thread-rewind-receive list? lst)
  (atomically
   (define t (current-thread))
   (for-each (lambda (msg)
               (push-mail! t msg))
             lst)))

;; todo: thread-receive-evt





   
   
