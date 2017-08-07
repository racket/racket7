#lang racket/base
(require "../common/queue.rkt"
         "check.rkt"
         "internal-error.rkt"
         "engine.rkt"
         "sandman.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "semaphore.rkt"
         "thread-group.rkt"
         "atomic.rkt"
         "schedule-info.rkt"
         "custodian.rkt"
         "exit.rkt")

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
         thread-push-suspend+resume-callbacks!
         thread-pop-suspend+resume-callbacks!
         
         thread-deschedule!
         thread-reschedule!
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
           do-make-thread
           root-thread
           thread-running?
           thread-dead!
           thread-did-work!
           
           thread-reschedule!

           poll-done-threads

           current-break-enabled-cell
           check-for-break

           break-max))

;; ----------------------------------------

(struct thread node (name
                     [engine #:mutable]
                     parent
                     [sleeping #:mutable] ; #f or sandman sleeper handle
                     [sched-info #:mutable]

                     [custodian-references #:mutable] ; list of custodian references
                     [beneficiaries #:mutable] ; a weak list for resume propagations

                     suspend-to-kill?
                     [kill-callbacks #:mutable] ; list of callbacks
                     
                     [suspend+resume-callbacks #:mutable] ; list of (cons callback callback)
                     [descheduled? #:mutable]
                     [interrupt-callback #:mutable] ; non-#f => wake up on kill
                     
                     [dead-sema #:mutable] ; created on demand
                     [dead-evt #:mutable] ; created on demand
                     [suspended? #:mutable]
                     [suspended-evt #:mutable]
                     [resumed-evt #:mutable]
                    
                     [pending-break #:mutable] ; #f, 'break, 'hang-up, or 'terminate
                     [ignore-break-cells #:mutable] ; => #f, a single cell, or a set of cells
                     [forward-break-to #:mutable] ; #f or a thread to receive this thread's breaks
                     
                     [waiting-mail? #:mutable] ; whether to wake up on `thread-send`
                     [mailbox #:mutable])
        #:property prop:waiter
        (make-waiter-methods 
         #:suspend! (lambda (t i-cb r-cb) (thread-deschedule! t #f i-cb r-cb))
         #:resume! (lambda (t v) (thread-reschedule! t) v))
        #:property prop:evt (lambda (t) (wrap-evt (get-thread-dead-evt t)
                                                  (lambda (v) t))))

(define root-thread #f)

;; ----------------------------------------
;; Thread creation

(define (do-make-thread who
                        proc
                        #:custodian [c (current-custodian)]
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
                    #f ; sleeping
                    #f ; sched-info

                    null ; custodian-references
                    null ; beneficiaries
                    
                    suspend-to-kill?
                    null ; kill-callbacks

                    null ; suspend+resume-callbacks
                    #f ; descheduled
                    #f ; interrupt-callback
                    
                    #f ; dead-sema
                    #f ; dead-evt
                    #f ; suspended?
                    #f ; suspended-evt
                    #f ; resumed-evt

                    #f ; pending-break
                    #f ; ignore-thread-cells
                    #f; forward-break-to

                    #f ; waiting for mail?
                    (make-queue))) ; mailbox
  ((atomically
    (define cref (unsafe-custodian-register c t remove-thread-custodian #f #t))
    (cond
      [cref
       (set-thread-custodian-references! t (list cref))
       (thread-group-add! p t)
       void]
      [else (lambda () (raise-custodian-is-shut-down who c))])))
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

;; In atomic mode
;; Terminating the current thread does not suspend or exit
(define (thread-dead! t)
  (set-thread-engine! t 'done)
  (when (thread-dead-sema t)
    (semaphore-post-all (thread-dead-sema t)))
  (run-interrupt-callback t)
  (unless (thread-descheduled? t)
    (thread-group-remove! (thread-parent t) t))
  (remove-from-sleeping-threads! t)
  (run-kill-callbacks! t)
  (when (thread-forward-break-to t)
    (do-break-thread (thread-forward-break-to t) 'break #f))
  (for ([cr (in-list (thread-custodian-references t))])
    (unsafe-custodian-unregister t cr))
  (set-thread-custodian-references! t null))

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
  (unless (for/and ([cr (in-list (thread-custodian-references t))])
            (custodian-manages-reference? (current-custodian) cr))
    (raise-arguments-error who
                           "the current custodian does not solely manage the specified thread"
                           "thread" t))
  (cond
    [(thread-suspend-to-kill? t)
     ((atomically
       (do-thread-suspend t)))]
    [else
     (atomically
      (do-kill-thread t))
     (when (eq? t (current-thread))
       (when (eq? t root-thread)
         (force-exit 0))
       (engine-block))
     (check-for-break-after-kill)]))

;; Called in atomic mode:
(define (do-kill-thread t)
  (unless (thread-dead? t)
    (thread-dead! t)))

;; Called in atomic mode:
(define (remove-thread-custodian t c)
  (define new-crs (for/list ([cref (in-list (thread-custodian-references t))]
                             #:unless (custodian-manages-reference? c cref))
                    cref))
  (set-thread-custodian-references! t new-crs)
  (when (null? new-crs)
    (cond
      [(thread-suspend-to-kill? t)
       (do-thread-suspend t)]
      [else
       (do-kill-thread t)])))

;; Called in atomic mode:
(define (run-kill-callbacks! t)
  (for ([cb (in-list (thread-kill-callbacks t))])
    (cb))
  (set-thread-kill-callbacks! t null))

(define (check-for-break-after-kill)
  ;; When a thread is terminated, it might be a nesting thread and
  ;; send a break to a nestee --- and the current thread might be the
  ;; nestee.
  (check-for-break))

(void (set-post-shutdown-action!
       (lambda ()
         ;; Check whether the current thread was terminated
         (let ([t (current-thread)])
           (when t ; in case custodians used (for testing) without threads
             (when (thread-dead? t)
               (engine-block))
             (check-for-break-after-kill))))))

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

;; in atomic mode
(define (remove-from-sleeping-threads! t)
  (define sleeping (thread-sleeping t))
  (when sleeping
    (set-thread-sleeping! t #f)
    (sandman-remove-sleeping-thread! t sleeping)))

;; in atomic mode
(define (add-to-sleeping-threads! t ext-events)
  (define sleeping (sandman-add-sleeping-thread! t ext-events))
  (set-thread-sleeping! t sleeping))

;; in atomic mode
;; Removes a thread from its thread group, so it won't be scheduled;
;; returns a thunk to be called in out of atomic mode to swap out the
;; thread, where the thunk returns `(void)`;
(define (do-thread-deschedule! t timeout-at)
  (set-thread-descheduled?! t #t)
  (thread-group-remove! (thread-parent t) t)
  (when timeout-at
    (add-to-sleeping-threads! t (sandman-merge-timeout #f timeout-at)))
  (when (eq? t (current-thread))
    (thread-did-work!))
  (lambda ()
    (when (eq? t (current-thread))
      (engine-block)
      (check-for-break))))

;; Extends `do-thread-deschdule!` where `t` is always `(current-thread)`.
;; The `interrupt-callback` is called if the thread receives a break
;; signal, is killed, or is suspended; if the break signal is
;; supressed or resumed, then `retry-callback` is called to try again
;; --- but `retry-callback` will only be used if `interrupt-callback`
;; was previously called, and neither is called if the thread is
;; "internal"-resumed normally instead of by a break signal of a
;; `thread-resume`.
(define (thread-deschedule! t timeout-at interrupt-callback retry-callback)
  (define needs-retry? #f)
  (atomically
   (set-thread-interrupt-callback! t (lambda ()
                                       ;; If the interrupt callback gets invoked,
                                       ;; then remember that we need a retry
                                       (set! needs-retry? #t)
                                       (interrupt-callback)))
   (define finish (do-thread-deschedule! t timeout-at))
   ;; It's ok if the thread gets interrupted
   ;; outside the atomic region, because we'd
   ;; swap it out anyway
   (lambda ()
     ;; In non-atomic mode:
     (finish)
     (when needs-retry?
       (retry-callback)))))

;; in atomic mode
;; Add a thread back to its thread group
(define (thread-reschedule! t)
  (when (thread-dead? t)
    (internal-error "tried to resume a dead thread"))
  (set-thread-descheduled?! t #f)
  (set-thread-interrupt-callback! t #f)
  (remove-from-sleeping-threads! t)
  (thread-group-add! (thread-parent t) t))

(define/who (thread-suspend t)
  (check who thread? t)
  ((atomically
    (do-thread-suspend t))))

;; in atomic mode
;; Returns a thunk to call to handle the case that
;; the current thread is suspended
(define (do-thread-suspend t)
  (unless (thread-suspended? t)
    (set-thread-suspended?! t #t)
    ;; Suspending a thread is similar to issuing a break;
    ;; the thread should get out of any queues where it's
    ;; waiting, etc.:
    (run-interrupt-callback t)
    (run-suspend/resume-callbacks t car)
    (define suspended-evt (thread-suspended-evt t))
    (when suspended-evt
      (set-suspend-resume-evt-thread! suspended-evt t)
      (semaphore-post-all (suspend-resume-evt-sema suspended-evt))
      (set-thread-suspended-evt! t #f)))
  (cond
    [(not (thread-descheduled? t))
     (do-thread-deschedule! t #f)]
    [else
     void]))

(define/who (thread-resume t [benefactor #f])
  (check who thread? t)
  (check who (lambda (p) (or (not p) (thread? p) (custodian? p)))
         #:contract "(or/c #f thread? custodian?)"
         benefactor)
  (when (and (custodian? benefactor)
             (custodian-shut-down? benefactor))
    (raise-custodian-is-shut-down who benefactor))
  (atomically
   (do-thread-resume t benefactor)))

;; in atomic mode
(define (do-thread-resume t benefactor)
  (unless (thread-dead? t)
    (cond
      [(thread? benefactor)
       (for ([cr (in-list (thread-custodian-references benefactor))])
         (add-custodian-to-thread! t (custodian-reference->custodian cr)))
       (add-beneficiary-to-thread! benefactor t)]
      [(custodian? benefactor)
       (add-custodian-to-thread! t benefactor)])
    (when (and (thread-suspended? t)
               (pair? (thread-custodian-references t)))
      (define resumed-evt (thread-resumed-evt t))
      (when resumed-evt
        (set-suspend-resume-evt-thread! resumed-evt t)
        (semaphore-post-all (suspend-resume-evt-sema resumed-evt))
        (set-thread-resumed-evt! t #f))
      (set-thread-suspended?! t #f)
      (run-suspend/resume-callbacks t cdr)
      (thread-reschedule! t)
      (do-resume-beneficiaries t #f))))

;; in atomic mode
(define (add-custodian-to-thread! t c)
  (let loop ([crs (thread-custodian-references t)]
             [accum null])
    (cond
      [(null? crs)
       (define new-crs
         (cons (unsafe-custodian-register c t remove-thread-custodian #f #t)
               accum))
       (set-thread-custodian-references! t new-crs)
       (do-resume-beneficiaries t c)]
      [else
       (define old-c (custodian-reference->custodian (car crs)))
       (cond
         [(custodian-subordinate? c old-c)
          ;; no need to add new
          (void)]
         [(custodian-subordinate? old-c c)
          ;; new one replaces old one; we can simplify forget the
          ;; old reference
          (loop (cdr crs) accum)]
         [else
          ;; keep checking
          (loop (cdr crs) (cons (car crs) accum))])])))

;; in atomic mode
(define (add-beneficiary-to-thread! t b-t)
  ;; Look for `b-t` in list, and also prune
  ;; terminated threads
  (define new-l
    (let loop ([l (thread-beneficiaries t)])
      (cond
        [(null? l) (list (make-weak-box b-t))]
        [else
         (let ([o-t (weak-box-value (car l))])
           (cond
             [(not o-t) (loop (cdr l))]
             [(thread-dead? o-t) (loop (cdr l))]
             [(eq? b-t o-t) l]
             [else (cons (car l) (loop (cdr l)))]))])))
  (set-thread-beneficiaries! t new-l))

;; in atomic mode
(define (do-resume-beneficiaries t c)
  (for ([b (in-list (thread-beneficiaries t))])
    (define b-t (weak-box-value b))
    (when b-t
      (do-thread-resume b-t c))))

;; Called in atomic mode:
;; Given callbacks are also called in atomic mode
(define (thread-push-suspend+resume-callbacks! s-cb r-cb)
  (define t (current-thread))
  (set-thread-suspend+resume-callbacks! t (cons (cons s-cb r-cb)
                                                (thread-suspend+resume-callbacks t))))

;; Called in atomic mode:
(define (thread-pop-suspend+resume-callbacks!)
  (define t (current-thread))
  (set-thread-suspend+resume-callbacks! t (cdr (thread-suspend+resume-callbacks t))))

;; Called in atomic mode:
(define (run-suspend/resume-callbacks t sel)
  (for ([cbs (in-list (thread-suspend+resume-callbacks t))])
    ((sel cbs))))

;; Called in atomic mode:
(define (run-interrupt-callback t)
  (define interrupt-callback (thread-interrupt-callback t))
  (when interrupt-callback
    ;; The interrupt callback might remove the thread as
    ;; a waiter on a semaphore of channel; if breaks
    ;; turn out to be disabled, the wait will be
    ;; retried through the retry callback
    (set-thread-interrupt-callback! t #f)
    (interrupt-callback)))

;; ----------------------------------------
;; Suspend and resume events

(struct suspend-resume-evt (sema                ; semaphore, `always-evt`, or `never-evt`
                            [thread #:mutable]) ; set lazily to avoiding retaining the thread
  #:property prop:evt (lambda (se) (wrap-evt (suspend-resume-evt-sema se)
                                             (lambda (s) (suspend-resume-evt-thread se)))))

(struct suspend-evt suspend-resume-evt ()
  #:reflection-name 'thread-suspend-evt)

(struct resume-evt suspend-resume-evt ()
  #:reflection-name 'thread-resume-evt)

(define/who (thread-resume-evt t)
  (check who thread? t)
  (atomically
   (cond
     [(thread-dead? t)
      (resume-evt never-evt #f)]
     [(thread-suspended? t)
      (or (thread-resumed-evt t)
          (let ([r (resume-evt (make-semaphore) #f)])
            (set-thread-resumed-evt! t r)
            r))]
     [else
      (resume-evt always-evt t)])))

(define/who (thread-suspend-evt t)
  (check who thread? t)
  (atomically
   (cond
     [(thread-dead? t)
      (suspend-evt never-evt #f)]
     [(thread-suspended? t)
      (suspend-evt always-evt t)]
     [else
      (or (thread-suspended-evt t)
          (let ([s (suspend-evt (make-semaphore) #f)])
            (set-thread-suspended-evt! t s)
            s))])))

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
    ((thread-deschedule! (current-thread)
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
    [(on?)
     (thread-cell-set! (current-break-enabled-cell) on?)
     (when on?
       (check-for-break))]))

;; When the continuation-mark mapping to `break-enabled-key` is
;; changed, or when a thread is just swapped in, then
;; `check-for-break` should be called.
(define (check-for-break)
  (define t (current-thread))
  ((atomically
    (cond
     [(and (thread-pending-break t)
           (break-enabled)
           (not (thread-ignore-break-cell? t (current-break-enabled-cell)))
           (zero? (current-break-suspend)))
      (define exn:break* (case (thread-pending-break t)
                           [(hang-up) exn:break:hang-up/non-engine]
                           [(terminate) exn:break:terminate/non-engine]
                           [else exn:break/non-engine]))
      (set-thread-pending-break! t #f)
      (lambda ()
        ;; Out of atomic mode
        (call-with-escape-continuation
         (lambda (k)
           (raise (exn:break*
                   "user break"
                   (current-continuation-marks)
                   k)))))]
     [else void]))))

(define/who (break-thread t [kind #f])
  (check who thread? t)
  (check who (lambda (k) (or (not k) (eq? k 'hang-up) (eq? k 'terminate)))
         #:contract "(or/c #f 'hang-up 'terminate)"
         kind)
  (do-break-thread t (or kind 'break) (current-thread)))

;; Might be called in atomic mode, but `check-t` is #f in that case
(define (do-break-thread t kind check-t)
  ((atomically
    (cond
      [(thread-forward-break-to t)
       => (lambda (other-t)
            (lambda () (do-break-thread other-t kind check-t)))]
      [else
       (when (and (thread-pending-break t)
                  (break>? kind (thread-pending-break t)))
         (set-thread-pending-break! t kind))
       (unless (thread-pending-break t)
         (set-thread-pending-break! t kind)
         (when (thread-descheduled? t)
           (unless (thread-suspended? t)
             (run-interrupt-callback t)
             (thread-reschedule! t))))
       void])))
  (when (eq? t check-t)
    (check-for-break)))

(define (break>? k1 k2)
  (cond
    [(eq? k1 'break) #f]
    [(eq? k1 'hang-up) (eq? k2 'break)]
    [else (not (eq? k2 'terminate))]))

(define (break-max k1 k2)
  (cond
    [(not (and k1 k2)) (or k1 k2)]
    [(break>? k1 k2) k1]
    [else k2]))

(void
 (set-ctl-c-handler!
  (lambda (kind)
    (do-break-thread root-thread kind #f))))

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
         (thread-reschedule! thd))
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
         (thread-deschedule! t
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
