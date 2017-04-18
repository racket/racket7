#lang racket/base
(require "check.rkt"
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
                nacks ; list of thunks to run if not selected
                prev  ; previous in linked list
                next) ; next in linked list
        #:mutable)

(define none-syncer (syncer #f null null #f #f))

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
        (define sr (syncer arg null null last #f))
        (when last
          (set-syncer-next! last sr))
        (loop (cdr args)
              (or first sr)
              sr)])))
  
  (define s (syncing #f ; selected
                     syncers
                     void)) ; wakeup
  
  (cond
   [(and (real? timeout) (zero? timeout))
    (sync-poll s (lambda (sched-info) #f) #:just-poll? #t)]
   [(procedure? timeout)
    (sync-poll s (lambda (sched-info) (timeout)) #:just-poll? #t)]
   [else
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
         [(nack-evt? new-evt)
          (set-syncer-nacks! sr (cons (nack-evt-nack-proc new-evt) (syncer-nacks sr)))
          (set-syncer-evt! sr (nack-evt-evt new-evt))
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
               (null? (syncer-nacks sr)))
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
(define (syncing-done! s selected-sr)
  (set-syncing-selected! s selected-sr)
  (let loop ([sr (syncing-syncers s)])
    (when sr
      (unless (eq? sr selected-sr)
        (for ([nack (in-list (syncer-nacks sr))])
          (nack)))
      (loop (syncer-next sr))))
  ((syncing-wakeup s)))

;; ----------------------------------------

;; If everything we're waiting on is like a semaphore or channel,
;; where an asynchronous selection event is installed, then we can
;; completely suspend this thread
(define (all-asynchronous? s)
  (atomically
   (let loop ([sr (syncing-syncers s)])
    (cond
     [(not sr) #t]
     [(async-evt? (syncer-evt sr))
      (loop (syncer-next sr))]
     [else #f]))))

;; Install a callback to reschedule the current thread if an
;; asynchronous selection happen, and then deschedule the thread
(define (suspend-syncing-thread s timeout-at)
  ((atomically
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
                                  (unless (syncing-selected s)
                                    (syncing-done! s none-syncer))))]))))
     
