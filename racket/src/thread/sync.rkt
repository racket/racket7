#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "evt.rkt"
         "atomic.rkt"
         "semaphore.rkt"
         "channel.rkt"
         "thread.rkt"
         "schedule-info.rkt")

(provide sync
         sync/timeout
         sync/enable-break
         sync/timeout/enable-break
         sync-atomic-poll-evt?
         current-evt-pseudo-random-generator)

(struct syncing (selected ; #f or a syncer that has been selected
                 syncers  ; linked list of `syncer`s
                 wakeup   ; a callback for when something is selected
                 disable-break) ; a thunk that disables breaks
        #:mutable)

(struct syncer (evt   ; the evt to sync; can get updated in sync loop
                wraps ; list of wraps to apply if selected
                interrupted? ; kill/break in progress?
                interrupts ; list of thunks to run on kill/break initiation
                abandons ; list of thunks to run on kill/break completion
                retries ; list of thunks to run on retry: returns `(values _val _ready?)`
                prev  ; previous in linked list
                next) ; next in linked list
        #:mutable)

(define (make-syncer evt wraps)
  (syncer evt wraps #f null null null #f #f))

(define none-syncer (make-syncer #f null))

;; To support `port-commit-peeked`, the `sync/timeout` function should
;; work for polling in atomic mode for a set of constrained event
;; types:
(define (sync-atomic-poll-evt? evt)
  (or (channel-put-evt? evt)
      (channel? evt)
      (semaphore? evt)
      (semaphore-peek-evt? evt)
      (eq? always-evt evt)
      (eq? never-evt evt)))

(define (do-sync who timeout args
                  #:enable-break? [enable-break? #f])
  (check who
         (lambda (timeout) (or (not timeout)
                               (and (real? timeout) (timeout . >= . 0))
                               (and (procedure? timeout)
                                    (procedure-arity-includes? timeout 0))))
         #:contract "(or/c #f (and/c real? (not/c negative?)) (-> any))"
         timeout)

  (define local-break-cell (and enable-break?
                                (make-thread-cell #t #t)))

  (define syncers (evts->syncers who args null))
  (define s (syncing #f ; selected
                     syncers
                     void ; wakeup
                     (and local-break-cell
                          (let ([t (current-thread)])
                            (lambda ()
                              (thread-ignore-break-cell! t local-break-cell))))))

  (begin0
    (with-continuation-mark
     break-enabled-key
     (if enable-break?
         local-break-cell
         (continuation-mark-set-first #f break-enabled-key))
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
                     (timeout-at . >= . (current-inexact-milliseconds)))
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
         (when local-break-cell
           (thread-remove-ignored-break-cell! (current-thread) local-break-cell))
         ;; On escape, post nacks, etc.:
         (syncing-abandon! s)))))
    ;; In case old break cell was meanwhile enabled:
    (check-for-break)))

(define (sync . args)
  (do-sync 'sync #f args))

(define (sync/timeout timeout . args)
  (do-sync 'sync/timeout timeout args))

(define (sync/enable-break . args)
  (do-sync 'sync/enable-break #f args #:enable-break? #t))

(define (sync/timeout/enable-break timeout . args)
  (do-sync 'sync/timeout/enable-break timeout args #:enable-break? #t))

;; ----------------------------------------

(define (evts->syncers who evts wraps)
  (let loop ([evts evts] [first #f] [last #f])
    (cond
      [(null? evts) first]
      [else
       (define arg (car evts))
       (when who
         (check who evt? arg))
       (define sr (make-syncer arg wraps))
       (set-syncer-prev! sr last)
       (when last
         (set-syncer-next! last sr))
       (loop (cdr evts)
             (or first sr)
             sr)])))

;; remove a syncer from its chain in `s`
(define (syncer-remove! sr s)
  (if (syncer-prev sr)
      (set-syncer-next! (syncer-prev sr) (syncer-next sr))
      (set-syncing-syncers! s (syncer-next sr)))
  (when (syncer-next sr)
    (set-syncer-prev! (syncer-next sr) (syncer-prev sr))))

;; Replace one syncer with a new, non-empty chain of syncers in `s`
(define (syncer-replace! sr new-syncers s)
  (let ([prev (syncer-prev sr)])
    (set-syncer-prev! new-syncers prev)
    (if prev
        (set-syncer-next! prev new-syncers)
        (set-syncing-syncers! s new-syncers)))
  (let loop ([new-syncers new-syncers])
    (cond
      [(syncer-next new-syncers)
       => (lambda (next) (loop next))]
      [else
       (let ([next (syncer-next sr)])
         (set-syncer-next! new-syncers next)
         (when next
           (set-syncer-prev! next new-syncers)))])))

;; ----------------------------------------

(define MAX-SYNC-TRIES-ON-ONE-EVT 10)

;; Run through the events of a `sync` one time; returns a thunk to
;; call in tail position --- possibly one that calls `none-k`.
(define (sync-poll s none-k
                   #:just-poll? [just-poll? #f]
                   #:did-work? [did-work? #f])
  (define sched-info (make-schedule-info #:did-work? did-work?))
  (let loop ([sr (syncing-syncers s)]
             [retries 0]) ; count retries on `sr`, and advance if it's too many
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
       [(= retries MAX-SYNC-TRIES-ON-ONE-EVT)
        (schedule-info-did-work! sched-info)
        (loop (syncer-next sr) 0)]
       [else
        (define-values (results new-evt)
          (evt-poll (syncer-evt sr) (poll-ctx just-poll?
                                              ;; Call back for asynchronous selection,
                                              ;; such as by a semaphore when it's posted
                                              ;; in a different thread; this callback
                                              ;; must be invoked in atomic mode
                                              (lambda ()
                                                (syncing-done! s sr))
                                              ;; Information to propagate to the thread
                                              ;; scheduler
                                              sched-info)))
        (cond
         [results
          (syncing-done! s sr)
          (make-result-thunk sr results)]
         [(delayed-poll? new-evt)
          ;; Have to go out of atomic mode to continue:
          (lambda ()
            (let ([new-evt ((delayed-poll-resume new-evt))])
              ;; Since we left atomic mode, double-check that we're
              ;; still syncing before installing the replacement event:
              (atomically
               (unless (syncing-selected s)
                 (set-syncer-evt! sr new-evt)))
              (loop sr (add1 retries))))]
         [(choice-evt? new-evt)
          (when (or (pair? (syncer-interrupts sr))
                    (pair? (syncer-abandons sr))
                    (pair? (syncer-retries sr)))
            (internal-error "choice event discovered after interrupt/abandon/retry callbacks"))
          (let ([new-syncers (evts->syncers #f (choice-evt-evts new-evt) (syncer-wraps sr))])
            (cond
              [(not new-syncers)
               ;; Empy choice, so drop it:
               (syncer-remove! sr s)
               (lambda () (loop (syncer-next sr) 0))]
              [else
               ;; Splice in new syncers, and start there
               (syncer-replace! sr new-syncers s)
               (lambda () (loop new-syncers (add1 retries)))]))]
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
          (lambda () (loop sr (add1 retries)))]
         [(control-state-evt? new-evt)
          (set-syncer-interrupts! sr (cons (control-state-evt-interrupt-proc new-evt) (syncer-interrupts sr)))
          (set-syncer-abandons! sr (cons (control-state-evt-abandon-proc new-evt) (syncer-abandons sr)))
          (set-syncer-retries! sr (cons (control-state-evt-retry-proc new-evt) (syncer-retries sr)))
          (set-syncer-evt! sr (control-state-evt-evt new-evt))
          (lambda () (loop sr (add1 retries)))]
         [(poll-guard-evt? new-evt)
          (lambda ()
            ;; Out of atomic region:
            (define generated ((poll-guard-evt-proc new-evt) just-poll?))
            (set-syncer-evt! sr (if (evt? generated)
                                    generated
                                    (wrap-evt always-evt (lambda (a) generated))))
            (loop sr (add1 retries)))]
         [(and (never-evt? new-evt)
               (null? (syncer-interrupts sr)))
          ;; Drop this event, since it will never get selected
          (syncer-remove! sr s)
          (lambda () (loop (syncer-next sr) 0))]
         [else
          (set-syncer-evt! sr new-evt)
          (lambda () (loop (syncer-next sr) 0))])])))))

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
  (when (syncing-disable-break s)
    ((syncing-disable-break s)))
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
               (never-evt? e))
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
                                    ((atomically
                                      (unless (syncing-selected s)
                                        (syncing-retry! s))
                                      (retry)))))])))))

;; ----------------------------------------

(define/who current-evt-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)))
