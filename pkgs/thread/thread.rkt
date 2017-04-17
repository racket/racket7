#lang racket/base
(require "check.rkt"
         "engine.rkt"
         "tree.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "semaphore.rkt"
         "thread-group.rkt"
         "atomic.rkt"
         "schedule-info.rkt"
         "system-idle-evt.rkt")

(provide (rename-out [-thread thread])
         thread?
         current-thread
         
         thread-running?
         thread-dead?
         
         thread-wait
         thread-dead-evt
         
         (rename-out [thread-sleep sleep])
         
         thread-internal-suspend!
         thread-internal-resume!
         
         thread-pause
         
         call-in-main-thread)

(define TICKS 1000)

;; A tree mapping times (in milliseconds) to a hash table of threads
;; to make up at that time
(define sleeping-threads empty-tree)

;; If a thread does work before it is suspended, then
;; we should poll all threads again. Accumulate a table
;; of threads that we don't need to poll because we've
;; tried them since the last thread did work:
(define poll-done-threads #hasheq())

(struct thread (name
                [engine #:mutable]
                parent
                [sleep-until #:mutable] ; non-#f => in `sleeping-threads`
                [sched-info #:mutable]
                suspend-to-kill?
                [kill-callback #:mutable] ; non-#f when suspended
                [done-sema #:mutable] ; created on demand
                [done-evt #:mutable] ; created on demand
                [suspended? #:mutable]
                [suspended-sema #:mutable])
        #:constructor-name make-thread
        #:property prop:waiter
        (make-waiter-methods 
         #:suspend! (lambda (t cb) (thread-internal-suspend! t #f cb))
         #:resume! (lambda (t v) (thread-internal-resume! t) v))
        #:property prop:evt (lambda (t) (wrap-evt (thread-dead-evt t)
                                             (lambda (v) t))))

(define (do-thread who
                   proc
                   #:suspend-to-kill? [suspend-to-kill? #f])
  (check who
         (lambda (proc)
           (and (procedure? proc)
                (procedure-arity-includes? proc 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (define p (current-thread-group))
  (define e (make-engine proc (gensym)))
  (define t (make-thread (gensym)
                         e
                         p
                         #f ; sleep-until
                         #f ; sched-info
                         suspend-to-kill?
                         #f ; kill-callback
                         #f ; done-sema
                         #f ; done-evt
                         #f ; suspended?
                         #f)) ; suspended-sema
  (thread-group-add! p t)
  t)

(define -thread
  (let ([thread (lambda (proc)
                  (do-thread 'thread proc))])
    thread))

(define (thread/suspend-to-kill proc)
  (do-thread 'thread/suspend-to-kill proc #:suspend-to-kill? #t))

(define (thread-running? t)
  (check 'thread-running? thread? t)
  (and (not (eq? 'done (thread-engine t)))
       (not (thread-suspended? t))))

(define (thread-dead? t)
  (check 'thread-dead? thread? t)
  (eq? 'done (thread-engine t)))

(define (thread-complete! t)
  (set-thread-engine! t 'done)
  (when (thread-done-sema t)
    (semaphore-post-all (thread-done-sema t)))
  (thread-group-remove! (thread-parent t) t)
  (remove-from-sleeping-threads! t))

;; ----------------------------------------

;; in atomic mode
(define (remove-from-sleeping-threads! t)
  (define sleep-until (thread-sleep-until t))
  (when sleep-until
    (set-thread-sleep-until! t #f)
    (define threads (tree-ref sleeping-threads sleep-until <))
    (unless threads (error "thread not found among sleeping threads"))
    (define new-threads (hash-remove threads t))
    (set! sleeping-threads
          (if (zero? (hash-count new-threads))
              (tree-remove sleeping-threads sleep-until <)
              (tree-set sleeping-threads sleep-until new-threads)))))

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

;; ----------------------------------------

(define (thread-internal-suspend! t timeout-at kill-callback)
  (atomically
   (set-thread-kill-callback! t kill-callback)
   (thread-group-remove! (thread-parent t) t)
   (when timeout-at
     (add-to-sleeping-threads! t timeout-at))
   (when (eq? t (current-thread))
     (thread-did-work!))
   ;; It's ok if the thread gets interrupted
   ;; outside the atomic region, because we'd
   ;; swap it out anyway
   (lambda ()
     (when (eq? t (current-thread))
       (engine-block)))))

(define (thread-internal-resume! t)
  (set-thread-kill-callback! t #f)
  (remove-from-sleeping-threads! t)
  (thread-group-add! (thread-parent t) t))

;; ----------------------------------------

(define (thread-run t)
  (define e (thread-engine t))
  (set-thread-engine! t 'running)
  (set-thread-sched-info! t #f)
  (current-thread t)
  (let loop ([e e])
    (e
     TICKS
     (lambda args
       (current-thread #f)
       (unless (zero? (current-atomic))
         (error 'thread-run "thread terminated in atomic mode!"))
       (thread-complete! t)
       (thread-did-work!)
       (thread-select!))
     (lambda (e)
       (cond
        [(zero? (current-atomic))
         (current-thread #f)
         (set-thread-engine! t e)
         (thread-select!)]
        [else
         (loop e)])))))

(define (thread-select!)
  (let loop ([g root-thread-group] [none-k maybe-done])
    (check-timeouts)
    (when (all-threads-poll-done?)
      (or (post-idle)
          (process-sleep)))
    (define child (thread-group-next! g))
    (cond
     [(not child) (none-k)]
     [(thread? child)
      (thread-run child)]
     [else
      (loop child (lambda () (loop g none-k)))])))

(define (maybe-done)
  (cond
   [(tree-empty? sleeping-threads)
    "all threads done"]
   [else (thread-select!)]))

;; Pause the current thread to let other threads run. If all threads
;; are paused, then `sched-info` contains information needed for a
;; global sleep, such as a timeout for the current thread's sleep:
(define (thread-pause sched-info)
  (atomically
   (cond
    [(or (not sched-info)
         (schedule-info-did-work? sched-info))
     (thread-did-work!)]
    [else (thread-did-no-work!)])
   (set-thread-sched-info! (current-thread) sched-info))
  (engine-block))

;; ----------------------------------------

(define thread-sleep
  (let ([sleep (lambda ([secs 0])
                 (check 'sleep
                        (lambda (c) (and (real? c) (c . >=  . 0)))
                        #:contract "(>=/c 0)"
                        secs)
                 ((thread-internal-suspend! (current-thread)
                                            (+ (* secs 1000.0)
                                               (current-inexact-milliseconds))
                                            void)))])
    sleep))

(define (check-timeouts)
  (unless (tree-empty? sleeping-threads)
    (define-values (timeout-at threads) (tree-min sleeping-threads))
    (when (timeout-at . <= . (current-inexact-milliseconds))
      (unless (null? threads)
        (for ([t (in-hash-keys threads)])
          (thread-internal-resume! t))
        (thread-did-work!)))))

;; ----------------------------------------

(define (thread-did-no-work!)
  (set! poll-done-threads (hash-set poll-done-threads (current-thread))))

(define (thread-did-work!)
  (set! poll-done-threads #hasheq()))

(define (all-threads-poll-done?)
  (and (= (hash-count poll-done-threads)
          num-threads-in-groups)
       (or (positive? num-threads-in-groups)
           (not (tree-empty? sleeping-threads)))))

(define (distant-future)
  (+ (current-inexact-milliseconds)
     (* 1000.0 60 60 24 365)))

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

;; ----------------------------------------

(define (thread-wait t)
  (check 'thread-wait thread? t)
  (atomically
   (unless (thread-done-sema t)
     (set-thread-done-sema! t (make-semaphore 0))
     (when (eq? 'done (thread-engine t))
       (semaphore-post-all (thread-done-sema t)))))
  (semaphore-wait (thread-done-sema t)))

;; ----------------------------------------

(define (thread-suspend t)
  (check 'thread-suspend thread? t)
  (atomically
   (unless (thread-suspended? t)
     (set-thread-suspended?! t #t)
     (when (thread-suspended-sema t)
       (semaphore-post-all (thread-suspended-sema t))
       (set-thread-suspended-sema! t #f)))
   (when (thread-parent t) ; might be internally suspended
     (thread-group-remove! (thread-parent t) t))
   (when (eq? t (current-thread))
     (thread-did-work!)))
  ;; Ok to be out of the atomic block; see
  ;; `thread-internal-suspend!`
  (when (eq? t (current-thread))
    (engine-block)))

(struct -thread-dead-evt (sema)
        #:property
        prop:evt
        (lambda (tde) (wrap-evt (-thread-dead-evt-sema tde) (lambda (s) tde)))
        #:reflection-name 'thread-dead-evt)

(define (thread-dead-evt t)
  (check 'thread-wait thread? t)
  (atomically
   (unless (thread-done-sema t)
     (set-thread-done-sema! t (make-semaphore 0))
     (when (and (eq? 'done (thread-engine t))
                (thread-done-sema t))
       (semaphore-post-all (thread-done-sema t)))
     (set-thread-done-evt! t (-thread-dead-evt (thread-done-sema t)))))
  (thread-done-evt t))

;; ----------------------------------------

(define (call-in-main-thread thunk)
  (-thread thunk)
  (thread-select!))
