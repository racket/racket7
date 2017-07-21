#lang racket/base
(require "atomic.rkt"
         "engine.rkt"
         "internal-error.rkt"
         "sandman.rkt"
         "parameter.rkt"
         "thread-group.rkt"
         "schedule-info.rkt"
         (submod "thread.rkt" scheduling)
         "system-idle-evt.rkt"
         "exit.rkt")

;; Many scheduler details are implemented in "thread.rkt", but this
;; module handles the thread selection, thread swapping, and
;; process sleeping.

(provide call-in-main-thread
         set-atomic-timeout-callback!)

(define TICKS 100000)

;; Initializes the thread system:
(define (call-in-main-thread thunk)
  (make-initial-thread thunk)
  (select-thread!))

;; ----------------------------------------

(define (select-thread!)
  (let loop ([g root-thread-group] [none-k maybe-done])
    (check-external-events 'fast)
    (when (and (all-threads-poll-done?)
               (maybe-future-work?))
      (or (check-external-events 'slow)
          (post-idle)
          (process-sleep)))
    (define child (thread-group-next! g))
    (cond
     [(not child) (none-k)]
     [(thread? child)
      (swap-in-thread child)]
     [else
      (loop child (lambda () (loop g none-k)))])))

(define (swap-in-thread t)
  (define e (thread-engine t))
  (set-thread-engine! t 'running)
  (set-thread-sched-info! t #f)
  (current-thread t)
  (let loop ([e e])
    (e
     TICKS
     (lambda ()
       (check-for-break)
       (when (positive? (current-atomic))
         (atomic-timeout-callback)))
     (lambda args
       (current-thread #f)
       (unless (zero? (current-atomic))
         (internal-error "terminated in atomic mode!"))
       (thread-dead! t)
       (when (eq? root-thread t)
         (force-exit 0))
       (thread-did-work!)
       (select-thread!))
     (lambda (e)
       (cond
         [(zero? (current-atomic))
          (current-thread #f)
          (set-thread-engine! t e)
          (select-thread!)]
         [else
          ;; Swap out when the atomic region ends:
          (set-end-atomic-callback! engine-block)
          (loop e)])))))

(define (maybe-done)
  (cond
   [(and (not (sandman-any-sleepers?))
         (not (any-idle-waiters?)))
    ;; all threads done or blocked
    (cond
      [(thread-running? root-thread)
       ;; we shouldn't exit, because the main thread is
       ;; blocked, but it's not going to become unblocked;
       ;; sleep forever or until a signal changes things
       (process-sleep)
       (select-thread!)]
      [else
       (void)])]
   [else
    ;; try again, which should lead to `process-sleep`
    (select-thread!)]))

;; Check for threads that have been suspended until a particular time,
;; etc., as registered with the sandman
(define (check-external-events mode)
  (define did? #f)
  (sandman-poll mode
                (lambda (t)
                  (thread-internal-resume! t)
                  (set! did? #t)))
  (when did?
    (thread-did-work!))
  did?)

;; ----------------------------------------

;; Have we tried all threads without since most recently making
;; progress on some thread?
(define (all-threads-poll-done?)
  (= (hash-count poll-done-threads)
     num-threads-in-groups))

(define (maybe-future-work?)
  (or (positive? num-threads-in-groups)
      (sandman-any-sleepers?)
      (any-idle-waiters?)))

;; Stop using the CPU for a while
(define (process-sleep)
  (define ts (thread-group-all-threads root-thread-group null))
  (define sleeping-exts
    (sandman-sleepers-external-events))
  (define exts
    (for/fold ([exts sleeping-exts]) ([t (in-list ts)])
      (define sched-info (thread-sched-info t))
      (define t-exts (and sched-info
                          (schedule-info-exts sched-info)))
      (sandman-merge-exts exts t-exts)))
  (sandman-sleep exts)
  ;; Maybe some thread can proceed:
  (thread-did-work!))

;; ----------------------------------------

(define atomic-timeout-callback void)

(define (set-atomic-timeout-callback! cb)
  (set! atomic-timeout-callback (or cb void)))
