#lang racket/base
(require "../common/queue.rkt"
         "check.rkt"
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
         "custodian.rkt")

(provide (rename-out [make-thread thread])
         thread/suspend-to-kill
         unsafe-thread-at-root
         thread?
         current-thread
         
         thread-running?
         thread-dead?
         
         thread-wait
         thread-suspend
         thread-resume
         thread-suspend-evt
         thread-resume-evt
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
         thread-rewind-receive)

;; Exports needed by "schedule.rkt":
(module* scheduling #f
  (provide (struct-out thread)

           make-initial-thread
           thread-dead!
           thread-did-work!
           
           thread-internal-resume!

           sleeping-threads
           poll-done-threads

           current-break-enabled-cell
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
                     [resumed-sema #:mutable]
                    
                     [needs-retry? #:mutable] ; => resuming implies an retry action
                     
                     [pending-break? #:mutable]
                     [ignore-break-cells #:mutable] ; => #f, a single cell, or a set of cells
                     [forward-break-to #:mutable] ; #f or a thread to receive ths thread's breaks
                     
                     [waiting-mail? #:mutable]
                     [mailbox #:mutable]) ;; mailbox
        #:property prop:waiter
        (make-waiter-methods 
         #:suspend! (lambda (t i-cb r-cb) (thread-internal-suspend! t #f i-cb r-cb))
         #:resume! (lambda (t v) (thread-internal-resume! t) v))
        #:property prop:evt (lambda (t) (wrap-evt (get-thread-dead-evt t)
                                             (lambda (v) t))))

(define root-thread #f)

;; ----------------------------------------
;; Thread creation

(define (do-make-thread who
                        proc
                        #:at-root? [at-root? #f]
                        #:initial? [initial? #f]
                        #:suspend-to-kill? [suspend-to-kill? #f])
  (check who (procedure-arity-includes/c 0) proc)
  (define p (if at-root?
                root-thread-group
                (current-thread-group)))
  (define e (make-engine proc
                         (if (or initial? at-root?)
                             break-enabled-default-cell
                             (current-break-enabled-cell))
                         at-root?))
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
                    #f ; resumed-sema

                    #f ; needs-retry?

                    #f ; pending-break?
                    #f ; ignore-thread-cells
                    #f; forward-break-to

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
  (let ([t (do-make-thread 'thread thunk #:initial? #t)])
    (set! root-thread t)
    t))

(define (unsafe-thread-at-root proc)
  (do-make-thread 'unsafe-thread-at-root proc #:at-root? #t))

;; ----------------------------------------
;; Thread status

(define/who (thread-running? t)
  (check who thread? t)
  (and (not (eq? 'done (thread-engine t)))
       (not (thread-suspended? t))))

(define/who (thread-dead? t)
  (check who thread? t)
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

(define/who (kill-thread t)
  (check who thread? t)
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

(define/who (thread-wait t)
  (check who thread? t)
  (semaphore-wait (get-thread-dead-sema t)))

(struct dead-evt (sema)
        #:property prop:evt (lambda (tde) (wrap-evt (dead-evt-sema tde)
                                               (lambda (s) tde)))
        #:reflection-name 'thread-dead-evt)

(define (thread-dead-evt? v)
  (dead-evt? v))

(define/who get-thread-dead-evt
  (let ([thread-dead-evt
         (lambda (t)
           (check who thread? t)
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
;; returns a thunk to be called in out of atomic mode to swap out the
;; thread, where the thunk returns `(void)`; the `interrupt-callback`
;; is called if the thread receives a break signal, is killed, or is
;; suspended; if the break signal is supressed or resumed, then
;; `retry-callback` is called to try again --- but `retry-callback`
;; will only be used if `interrupt-callback` was previously called,
;; and neither is called if the thread is "internal"-resumed normally
;; instead of by a break signal of a `thread-resume`.
(define (thread-internal-suspend! t timeout-at interrupt-callback retry-callback)
  (atomically
   (set-thread-interrupt-callback! t (lambda ()
                                       ;; If the interrupt callback gets invoked,
                                       ;; then remember that we need a retry
                                       (set-thread-needs-retry?! t #t)
                                       (interrupt-callback)))
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
     [(thread-needs-retry? t)
      ;; Since the callback wasn't cleared, the thread
      ;; was woken up to check for a break
      (set-thread-needs-retry?! t #f)
      (thread-did-work!)
      (lambda ()
        ;; In non-atomic mode:
        (check-for-break)
        (retry-callback))]
     [else void]))))

;; in atomic mode
;; Add a thread back to its thread group
(define (thread-internal-resume! t)
  (when (thread-dead? t)
    (internal-error "tried to resume a dead thread"))
  (set-thread-interrupt-callback! t #f)
  (remove-from-sleeping-threads! t)
  (thread-group-add! (thread-parent t) t))

(define/who (thread-suspend t)
  (check who thread? t)
  ((atomically
    (unless (thread-suspended? t)
      (set-thread-suspended?! t #t)
      (let ([interrupt-callback (thread-interrupt-callback t)])
        (when interrupt-callback
          ;; Suspending a thread is similar to issuing a break;
          ;; the thread should get out of any queues where it's
          ;; waiting, etc.
          (set-thread-interrupt-callback! t void)
          (interrupt-callback)))
      (when (thread-suspended-sema t)
        (semaphore-post-all (thread-suspended-sema t))
        (set-thread-suspended-sema! t #f)))
    (cond
     [(not (thread-interrupt-callback t)) ; => not internally suspended already
      (thread-internal-suspend! t #f void void)]
     [else 
      void]))))

(define/who (thread-resume t [benefactor #f])
  (check who thread? t)
  (check who (lambda (p) (or (not p) (thread? p) (custodian? p)))
         #:contract "(or/c #f thread? custodian?)"
         benefactor)
  (atomically
   (unless (thread-dead? t)
     (when (thread-suspended? t)
       (when (thread-resumed-sema t)
         (semaphore-post-all (thread-resumed-sema t))
         (set-thread-resumed-sema! t #f))
       (set-thread-suspended?! t #f)
       (thread-internal-resume! t)))))

;; ----------------------------------------
;; Suspend and resume events

(struct suspend-evt (sema)
  #:property prop:evt (lambda (se) (wrap-evt (suspend-evt-sema se)
                                             (lambda (s) se)))
  #:reflection-name 'thread-suspend-evt)

(struct resume-evt (sema)
  #:property prop:evt (lambda (re) (wrap-evt (resume-evt-sema re)
                                             (lambda (s) re)))
  #:reflection-name 'thread-resume-evt)

(define/who (thread-resume-evt t)
  (check who thread? t)
  (atomically
   (let ([s (or (thread-resumed-sema t)
                (let ([s (make-semaphore)])
                  (set-thread-resumed-sema! t s)
                  s))])
     (resume-evt s))))

(define/who (thread-suspend-evt t)
  (check who thread? t)
  (atomically
   (let ([s (or (thread-suspended-sema t)
                (let ([s (make-semaphore)])
                  (set-thread-suspended-sema! t s)
                  s))])
     (suspend-evt s))))

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
(define/who (sleep [secs 0])
  (check who
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
(define break-enabled-default-cell (make-thread-cell #t))

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
           ;; If a retry is pending, then defer
           ;; break checking to the continuation (instead
           ;; of raising an asynchronous exception now)
           (not (thread-needs-retry? t)))
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

(define/who (break-thread t)
  (check who thread? t)
  (do-break-thread t (current-thread)))

(define (do-break-thread t check-t)
  ((atomically
    (unless (thread-pending-break? t)
      (cond
        [(thread-forward-break-to t)
         => (lambda (other-t)
              (lambda () (do-break-thread other-t check-t)))]
        [else
         (set-thread-pending-break?! t #t)
         (unless (thread-suspended? t)
           (cond
             [(thread-interrupt-callback t)
              => (lambda (interrupt-callback)
                   ;; The interrupt callback might remove the thread as
                   ;; a waiter on a semaphore of channel; if breaks
                   ;; turn out to be disabled, the wait will be
                   ;; retried through the retry callback
                   (interrupt-callback)
                   (thread-internal-resume! t))]))
         void]))))
  (when (eq? t check-t)
    (check-for-break)))

(void
 (set-ctl-c-handler!
  (lambda ()
    (do-break-thread root-thread #f))))

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

;; in atomic mode
(define (enqueue-mail! thd v)
  (queue-add! (thread-mailbox thd) v))

;; in atomic mode
(define (dequeue-mail! thd)
  (define mbx (thread-mailbox thd))
  (cond
    [(queue-empty? mbx)
     (internal-error "No Mail!\n")]
    [else
     (queue-remove! mbx)]))

;; in atomic mode
(define (is-mail? thd)
  (not (queue-empty? (thread-mailbox thd))))

;; in atomic mode
(define (push-mail! thd v)
  (queue-add-front! (thread-mailbox thd) v))

(define/who (thread-send thd v [fail-thunk 
                                (lambda ()
                                  (raise-arguments-error 'thread-send "target thread is not running"))])
  (check who thread? thd)
  (check who (procedure-arity-includes/c 0) #:or-false fail-thunk)
  ((atomically
    (cond
      [(not (thread-dead? thd))
       (enqueue-mail! thd v)
       (when (thread-waiting-mail? thd)
         (set-thread-waiting-mail?! thd #f)
         (thread-internal-resume! thd))
       void]
      [fail-thunk
       fail-thunk]
      [else
       (lambda () #f)]))))

(define (thread-receive)
  ((atomically
    (define t (current-thread))
    (cond
      [(is-mail? t)
       (define v (dequeue-mail! t))
       (lambda () v)]
      [else
       (set-thread-waiting-mail?! t #t)
       (define do-yield
         (thread-internal-suspend! t
                                   #f
                                   ;; Interrupted for break => not waiting for mail
                                   (lambda ()
                                     (set-thread-waiting-mail?! t #f))
                                   ;; No retry action, because we always retry:
                                   void))
       ;; called out of atomic mode:
       (lambda ()
         (do-yield)
         (thread-receive))]))))
 
(define (thread-try-receive)
  (atomically
   (define t (current-thread))
   (if (is-mail? t)
       (dequeue-mail! t)
       #f)))

(define/who (thread-rewind-receive lst)
  (check who list? lst)
  (atomically
   (define t (current-thread))
   (for-each (lambda (msg)
               (push-mail! t msg))
             lst)))

;; todo: thread-receive-evt
