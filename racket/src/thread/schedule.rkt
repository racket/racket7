#lang racket/base
(require "atomic.rkt"
         "internal-error.rkt"
         "tree.rkt"
         "parameter.rkt"
         "thread-group.rkt"
         "schedule-info.rkt"
         (submod "thread.rkt" scheduling)
         "system-idle-evt.rkt")

;; Many scheduler details are implemented in "thread.rkt", but this
;; module handles the thread selection, thread swapping, and
;; process sleeping.

(provide call-in-main-thread)

(define TICKS 100000)

;; Initializes the thread system:
(define (call-in-main-thread thunk)
  (make-initial-thread thunk)
  (select-thread!))

;; ----------------------------------------

(define (select-thread!)
  (let loop ([g root-thread-group] [none-k maybe-done])
    (check-timeouts)
    (when (and (all-threads-poll-done?)
               (maybe-future-work?))
      (or (post-idle)
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
       (check-for-break))
     (lambda args
       (current-thread #f)
       (unless (zero? (current-atomic))
         (internal-error "terminated in atomic mode!"))
       (thread-dead! t)
       (thread-did-work!)
       (select-thread!))
     (lambda (e)
       (cond
        [(zero? (current-atomic))
         (current-thread #f)
         (set-thread-engine! t e)
         (select-thread!)]
        [else
         (loop e)])))))

(define (maybe-done)
  (cond
   [(and (tree-empty? sleeping-threads)
         (not (any-idle-waiters?)))
    ;; all threads done
    (void)]
   [else
    ;; try again, which should lead to `process-sleep`
    (select-thread!)]))

;; Check for threads that have been suspended until a particular time
(define (check-timeouts)
  (unless (tree-empty? sleeping-threads)
    (define-values (timeout-at threads) (tree-min sleeping-threads))
    (when (timeout-at . <= . (current-inexact-milliseconds))
      (unless (null? threads)
        (for ([t (in-hash-keys threads)])
          (thread-internal-resume! t))
        (thread-did-work!)))))

;; ----------------------------------------

;; Have we tried all threads without since most recently making
;; progress on some thread?
(define (all-threads-poll-done?)
  (= (hash-count poll-done-threads)
     num-threads-in-groups))

(define (maybe-future-work?)
  (or (positive? num-threads-in-groups)
      (not (tree-empty? sleeping-threads))
      (any-idle-waiters?)))

;; Stop using the CPU for a while
(define (process-sleep)
  (define ts (thread-group-all-threads root-thread-group null))
  (define sleep-timeout (if (tree-empty? sleeping-threads)
                            (distant-future)
                            (let-values ([(timeout-at threads) (tree-min sleeping-threads)])
                              timeout-at)))
  (define timeout-at
    (for/fold ([timeout-at sleep-timeout]) ([t (in-list ts)])
      (define sched-info (thread-sched-info t))
      (define t-timeout-at (and sched-info
                                (schedule-info-timeout-at sched-info)))
      (cond
       [(not t-timeout-at) timeout-at]
       [else (min timeout-at t-timeout-at)])))
  (sleep (/ (- timeout-at (current-inexact-milliseconds)) 1000.0))
  ;; Maybe some thread can proceed:
  (thread-did-work!))

;; Compute an approximation to infinity:
(define (distant-future)
  (+ (current-inexact-milliseconds)
     (* 1000.0 60 60 24 365)))
