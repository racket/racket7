#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "evt.rkt"
         "atomic.rkt"
         "semaphore.rkt"
         "thread.rkt"
         "schedule-info.rkt")

(provide sync
         sync/timeout)

(struct syncing (selected ; #f or a syncer that has been selected
                 syncers  ; linked list of `syncer`s
                 wakeup)  ; a callback for when something is selected
        #:mutable)

(struct syncer (evt   ; the evt to sync; can get updated in sync loop
                wraps ; list of wraps to apply if selected
                interrupted? ; kill/break in progerss?
                interrupts ; list of thunks to run on kill/break initiation
                abandons ; list of thunks to run on kill/break completion
                retries ; list of thunks to run on retry: returns `(values _val _ready?)`
                prev  ; previous in linked list
                next) ; next in linked list
        #:mutable)

(define (make-syncer arg last)
  (syncer arg null #f null null null last #f))

(define none-syncer (make-syncer #f #f))

(define (do-sync who timeout args)
  (check who
         (lambda (timeout) (or (not timeout)
                          (and (real? timeout) (timeout . >= . 0))
                          (and (procedure? timeout)
                               (procedure-arity-includes? timeout 0))))
         #:contract "(or/c #f (and/c real? (not/c negative?)) (-> any))"
         timeout)
  
  (define syncers
    (let loop ([args args] [first #f] [last #f])
      (cond
       [(null? args) first]
       [else
        (define arg (car args))
        (check who evt? arg)
        (define sr (make-syncer arg last))
        (when last
          (set-syncer-next! last sr))
        (loop (cdr args)
              (or first sr)
              sr)])))
  
  (define s (syncing #f ; selected
                     syncers
                     void)) ; wakeup

  (dynamic-wind
   (lambda ()
     (atomically
      (thread-push-kill-callback!
       (lambda () (syncing-abandon! s)))))
   (lambda ()
     (cond
      [(and (real? timeout) (zero? timeout))
       (sync-poll s (lambda (sched-info) #f) #:just-poll? #t)]
      [(procedure? timeout)
       (sync-poll s (lambda (sched-info) (timeout)) #:just-poll? #t)]
      [else
       ;; Loop to poll; if all events end up with asynchronous-select
       ;; callbacks, then the loop can suspend the current thread
       (define timeout-at
         (and timeout
              (+ (* timeout 1000) (current-inexact-milliseconds))))
       (let loop ([did-work? #t])
         (cond
          [(and timeout
                (timeout . >= . (current-inexact-milliseconds)))
           (syncing-done! s none-syncer)
           #f]
          [(and (all-asynchronous? s)
                (not (syncing-selected s)))
           (suspend-syncing-thread s timeout-at)
           (loop #f)]
          [else
           (sync-poll s #:did-work? did-work?
                      (lambda (sched-info)
                        (when timeout-at
                          (schedule-info-add-timeout-at! sched-info timeout-at))
                        (thread-yield sched-info)
                        (loop #f)))]))]))
   (lambda ()
     (atomically
      (thread-pop-kill-callback!)
      ;; On escape, post nacks, etc.:
      (syncing-abandon! s)))))

(define (sync . args)
  (do-sync 'sync #f args))

(define (sync/timeout timeout . args)
  (do-sync 'sync/timeout timeout args))

;; ----------------------------------------

;; Run through the events of a `sync` one time; returns a thunk to
;; call in tail position --- possibly one that calls `none-k`.
(define (sync-poll s none-k
                   #:just-poll? [just-poll? #f]
                   #:did-work? [did-work? #f])
  (define sched-info (make-schedule-info #:did-work? did-work?))
  (let loop ([sr (syncing-syncers s)])
    ((atomically
      (cond
       [(syncing-selected s)
        => (lambda (sr)
             ;; Some concurrent synchronization happened
             (make-result-thunk sr (list (syncer-evt sr))))]
       [(not sr)
        (when just-poll?
          (syncing-done! s none-syncer))
        (lambda () (none-k sched-info))]
       [else
        (define-values (results new-evt)
          (evt-poll (syncer-evt sr) (poll-ctx just-poll?
                                              ;; Call back for asynchronous selection,
                                              ;; such as by a semaphore when it's posted
                                              ;; in a different thread; this callback
                                              ;; must be invoked in atomic mode
                                              (lambda ()
                                                (syncing-done! s sr))
                                              ;; Information to propagate the to thread
                                              ;; scheduler
                                              sched-info)))
        (cond
         [results
          (syncing-done! s sr)
          (make-result-thunk sr results)]
         #;
         [(choice-evt? new-evt)
          (error "flatten")]
         [(wrap-evt? new-evt)
          (set-syncer-wraps! sr (cons (wrap-evt-wrap new-evt)
                                      (let ([l (syncer-wraps sr)])
                                        (if (and (null? l)
                                                 (not (handle-evt? new-evt)))
                                            ;; Prevent wrapper from being in tail position:
                                            (list values)
                                            ;; Allow handler in tail position:
                                            l))))
          (set-syncer-evt! sr (wrap-evt-evt new-evt))
          (lambda () (loop sr))]
         [(control-state-evt? new-evt)
          (set-syncer-interrupts! sr (cons (control-state-evt-interrupt-proc new-evt) (syncer-interrupts sr)))
          (set-syncer-abandons! sr (cons (control-state-evt-abandon-proc new-evt) (syncer-abandons sr)))
          (set-syncer-retries! sr (cons (control-state-evt-retry-proc new-evt) (syncer-retries sr)))
          (set-syncer-evt! sr (control-state-evt-evt new-evt))
          (lambda () (loop sr))]
         [(guard-evt? new-evt)
          (lambda ()
            ;; Out of atomic region:
            (define generated ((guard-evt-proc new-evt)))
            (set-syncer-evt! sr (if (evt? generated)
                                    generated
                                    (wrap-evt always-evt (lambda (a) generated))))
            (loop sr))]
         [(and (never-evt? new-evt)
               (null? (syncer-interrupts sr)))
          ;; Drop this event, since it will never get selected
          (if (syncer-prev sr)
              (set-syncer-next! (syncer-prev sr) (syncer-next sr))
              (set-syncing-syncers! s (syncer-next sr)))
          (when (syncer-next sr)
            (set-syncer-prev! (syncer-next sr) (syncer-prev sr)))
          (lambda () (loop (syncer-next sr)))]
         [else
          (set-syncer-evt! sr new-evt)
          (lambda () (loop (syncer-next sr)))])])))))

(define (make-result-thunk sr results)
  (define wraps (syncer-wraps sr))
  (lambda ()
    (let loop ([wraps wraps] [results results])
      (cond
       [(null? wraps)
        (apply values results)]
       [(null? (cdr wraps))
        ;; Call last one in tail position:
        (apply (car wraps) results)]
       [else
        (loop (cdr wraps)
              (call-with-values (lambda () (apply (car wraps) results)) list))]))))

;; ----------------------------------------

;; Called in atomic mode
;;  Finalizes a decision for the sychronization, calling
;;  interrupt+abandon (or just abandon, if already interrupted)
;;  on non-selected events to indicate that they will never be
;;  selected for this synchronization
(define (syncing-done! s selected-sr)
  (set-syncing-selected! s selected-sr)
  (let loop ([sr (syncing-syncers s)])
    (when sr
      (unless (eq? sr selected-sr)
        (unless (syncer-interrupted? sr)
          (for ([interrupt (in-list (syncer-interrupts sr))])
            (interrupt)))
        (for ([abandon (in-list (syncer-abandons sr))])
          (abandon)))
      (loop (syncer-next sr))))
  ((syncing-wakeup s)))

;; Called in atomic mode
(define (syncing-abandon! s)
  (unless (syncing-selected s)
    (syncing-done! s none-syncer)))

;; Called in atomic mode
;;  For each syncer that needs a notification (e.g., to get out of
;;  a queue of waiters), call its `interrupt` callback
(define (syncing-interrupt! s)
  (let loop ([sr (syncing-syncers s)])
    (when sr
      (when (syncer-interrupted? sr)
        (internal-error "interrupting an already-interrupted syncer"))
      (set-syncer-interrupted?! sr #t)
      (for ([interrupt (in-list (syncer-interrupts sr))])
        (interrupt))
      (loop (syncer-next sr)))))

;; Called in atomic mode
;;  For each syncer that needs a notification (e.g., to get back into
;;  a queue of waiters), call its `retry` callback; a retry might
;;  succeed immediately, moving the synchronization into "selected"
;;  state
(define (syncing-retry! s)
  (let loop ([sr (syncing-syncers s)] [done? #f])
    (when (and sr
               (not (syncing-selected s)))
      (unless (syncer-interrupted? sr)
        (internal-error "retrying a non-interrupted syncer"))
      (set-syncer-interrupted?! sr #f)
      ;; Although we keep a list of retries, we expect only
      ;; one to be relevant
      (for ([retry (in-list (syncer-retries sr))])
        (define-values (result ready?) (retry))
        (when ready?
          (set-syncer-wraps! sr (cons (lambda args result) (syncer-wraps sr)))
          (syncing-done! s sr))))))

;; ----------------------------------------

;; If everything we're waiting on is like a semaphore or channel,
;; where an asynchronous selection event is installed, then we can
;; completely suspend this thread
(define (all-asynchronous? s)
  (atomically
   (let loop ([sr (syncing-syncers s)])
    (cond
     [(not sr) #t]
     [else
      (define e (syncer-evt sr))
      (and (or (async-evt? e)
               ;; REMOVEME
               #;(never-evt? e))
           (loop (syncer-next sr)))]))))

;; Install a callback to reschedule the current thread if an
;; asynchronous selection happens, and then deschedule the thread
(define (suspend-syncing-thread s timeout-at)
  ((atomically
    (let retry ()
      (cond
       [(syncing-selected s)
        ;; don't suspend after all
        void]
       [else
        (define t (current-thread))
        (set-syncing-wakeup!
         s
         (lambda ()
           (set-syncing-wakeup! s void)
           (thread-internal-resume! t)))
        (thread-internal-suspend! t
                                  timeout-at
                                  (lambda ()
                                    ;; Break/kill signal
                                    (set-syncing-wakeup! s void)
                                    (unless (syncing-selected s)
                                      (syncing-interrupt! s)))
                                  (lambda ()
                                    ;; Continue from or ignore break...
                                    ;; In non-atomic mode and tail position:
                                    (atomically
                                     (unless (syncing-selected s)
                                       (syncing-retry! s))
                                     (retry))))])))))
