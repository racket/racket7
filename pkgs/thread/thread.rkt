#lang racket/base
(require "check.rkt"
         "engine.rkt"
         "parameter.rkt"
         "waiter.rkt"
         "semaphore.rkt"
         "thread-group.rkt"
         "atomic.rkt")

(provide (rename-out [-thread thread])
         thread?
         current-thread
         
         thread-running?
         thread-dead?
         
         thread-wait
         
         thread-internal-suspend!
         thread-internal-resume!
         
         thread-pause
         
         call-in-main-thread)

(define TICKS 1000)

(struct thread (name
                [engine #:mutable]
                parent
                suspend-to-kill?
                [kill-callback #:mutable] ; non-#f when suspended
                [done-sema #:mutable] ; created on demand
                [suspended? #:mutable]
                [suspended-sema #:mutable])
        #:constructor-name make-thread
        #:property prop:waiter
        (make-waiter-methods 
         #:suspend! (lambda (t cb) (thread-internal-suspend! t cb))
         #:resume! (lambda (t v) (thread-internal-resume! t) v)))

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
                         suspend-to-kill?
                         #f ; kill-callback
                         #f ; done-sema
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
  (thread-group-remove! (thread-parent t) t))

;; ----------------------------------------

(define (thread-internal-suspend! t kill-callback)
  (atomically
   (set-thread-kill-callback! t kill-callback)
   (thread-group-remove! (thread-parent t) t)
   ;; It's ok if the thread gets interrupted
   ;; outside the atomic region, because we'd
   ;; swap it out anyway
   (lambda ()
     (when (eq? t (current-thread))
       (engine-block)))))

(define (thread-internal-resume! t)
  (set-thread-kill-callback! t #f)
  (thread-group-add! (thread-parent t) t))

;; ----------------------------------------

(define (thread-run t)
  (define e (thread-engine t))
  (set-thread-engine! t 'running)
  (current-thread t)
  (let loop ([e e])
    (e
     TICKS
     (lambda args
       (current-thread #f)
       (unless (zero? (current-atomic))
         (error 'thread-run "thread terminated in atomic mode!"))
       (thread-complete! t)
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
  (let loop ([g root-thread-group] [none-k (lambda () "all threads done")])
    (define child (thread-group-next! g))
    (cond
     [(not child) (none-k)]
     [(thread? child)
      (thread-run child)]
     [else
      (loop child (lambda () (loop g none-k)))])))

(define (thread-pause sched-info)
  (engine-block))

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
     (thread-group-remove! (thread-parent t) t)))
  ;; Ok to be out of the atomic block; see
  ;; `thread-internal-suspend!`
  (when (eq? t (current-thread))
    (engine-block)))

(define (thread-suspend-evt t)
  (check 'thread-wait thread? t)
  (atomically
   (unless (thread-done-sema t)
     (set-thread-done-sema! t (make-semaphore 0))
     (when (and (eq? 'done (thread-engine t))
                (thread-done-sema t))
       (semaphore-post-all (thread-done-sema t)))))
  (thread-done-sema t))

;; ----------------------------------------

(define (call-in-main-thread thunk)
  (-thread thunk)
  (thread-select!))
