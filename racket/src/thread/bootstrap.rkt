#lang racket/base
(require '#%linklet)

;; Simulate engines by using the host system's threads.

;; This simulation doesn't provide a `dynamic-wind` that cooperates
;; with `break-enabled-key`, and it does not support using an
;; exception handler in an engine.

(define (make-engine thunk init-break-enabled-cell empty-config?)
  (define ready-s (make-semaphore))
  (define s (make-semaphore))
  (define prefix void)
  (define results (list (void)))
  (define t (thread (lambda ()
                      (define orig (uncaught-exception-handler))
                      (define (run-prefix)
                        (prefix)
                        (set! prefix void))
                      (call-with-exception-handler
                       (lambda (exn)
                         (if (and (exn:break? exn)
                                  (not (exn:break/non-engine? exn)))
                             (with-handlers ([exn:break/non-engine?
                                              (lambda (exn)
                                                ;; Avoid exception-during-exception
                                                ;; error by propagating the original,
                                                ;; even though it's a different kind
                                                ;; of break exn:
                                                exn)])
                               (run-prefix)
                               ((exn:break-continuation exn)))
                             (abort-current-continuation
                              the-root-continuation-prompt-tag
                              exn)))
                       (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (with-continuation-mark
                                break-enabled-key
                                init-break-enabled-cell
                              (begin
                                (semaphore-post ready-s)
                                (semaphore-wait s)
                                (run-prefix)
                                (set! results
                                      (call-with-values thunk list)))))
                          the-root-continuation-prompt-tag
                          (lambda (exn)
                            ((error-display-handler) (exn-message exn) exn))))))))
  (semaphore-wait ready-s)
  (thread-suspend t)
  (semaphore-post s)
  (define (go ticks next-prefix complete expire)
    (set! prefix next-prefix)
    (break-thread t)
    (thread-resume t)
    (define t2
      (thread (lambda ()
                (sleep (/ ticks 1000000.0))
                (thread-suspend t))))
    ;; Limited break propagation while syncing:
    (call-with-exception-handler
     (lambda (exn)
       (if (and (exn:break? exn)
                ctl-c-handler)
           (begin
             (ctl-c-handler 'break)
             ((exn:break-continuation exn)))
           exn))
     (lambda ()
       (sync t t2 (thread-suspend-evt t))))
    (cond
     [(thread-dead? t)
      (apply complete 0 results)]
     [else
      (expire go)]))
  go)

(define (engine-block)
  (thread-suspend (current-thread)))

(define ctl-c-handler #f)

(define (set-ctl-c-handler! proc)
  (set! ctl-c-handler proc))

(define the-root-continuation-prompt-tag (make-continuation-prompt-tag 'root))
(define (root-continuation-prompt-tag) the-root-continuation-prompt-tag)
(define break-enabled-key (gensym 'break-enabled))

(struct exn:break/non-engine exn:break ())
(struct exn:break:hang-up/non-engine exn:break/non-engine ())
(struct exn:break:terminate/non-engine exn:break/non-engine ())

(define (internal-make-thread-parameter v)
  (define x v)
  (case-lambda
    [() x]
    [(v) (set! x v)]))

(primitive-table '#%engine
                 (hash 
                  'make-engine
                  make-engine
                  'engine-block
                  engine-block
                  'engine-return
                  (lambda args
                    (error "engine-return: not ready"))
                  'current-process-milliseconds
                  current-process-milliseconds
                  'set-ctl-c-handler!
                  set-ctl-c-handler!
                  'root-continuation-prompt-tag
                  root-continuation-prompt-tag
                  'break-enabled-key
                  break-enabled-key
                  'set-break-enabled-transition-hook!
                  void
                  'exn:break/non-engine
                  exn:break/non-engine
                  'exn:break:hang-up/non-engine
                  exn:break:hang-up/non-engine
                  'exn:break:terminate/non-engine
                  exn:break:terminate/non-engine
                  'internal-make-thread-parameter
                  internal-make-thread-parameter
                  'fork-pthread
                  (lambda args
                    (error "fork-pthread: not ready"))
                  'pthread?
                  (lambda args
                    (error "thread?: not ready"))
                  'get-thread-id
                  (lambda args
                    (error "get-thread-id: not ready"))
                  'make-condition
                  (lambda () 'condition)
                  'condition-wait
                  (lambda args
                    (error "condition-wait: not ready"))
                  'condition-signal 
                  (lambda args
                    (error "condition-signal: not ready"))
                  'condition-broadcast
                  (lambda args
                    (error "condition-broadcast: not ready"))
                  'threaded?
                  (lambda args
                    (error "threaded?: not ready"))
                  'current-engine-state
                  (lambda args
                    (error "current-engine state: not ready"))
                  'make-mutex
                  (lambda () 'mutex)
                  'mutex-acquire
                  (lambda args
                    (error "mutex-acquire: not ready"))
                  'mutex-release
                  (lambda args
                    (error "mutex-release: not ready"))
                  'active-pthreads
                  (lambda () 0)
                  'collect-garbage-pending-major?
                  (box '())
                  'collect-garbage-pending-minor?
                  (box '())))

;; add dummy definitions that implement pthreads and conditions etc.
;; dummy definitions that error

