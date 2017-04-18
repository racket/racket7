#lang racket/base
(require '#%linklet)

;; Simulate engines by using the host system's threads.

(define (make-engine thunk)
  (define ready-s (make-semaphore))
  (define s (make-semaphore))
  (define prefix void)
  (define results (list (void)))
  (define t (thread (lambda ()
                      (define orig (uncaught-exception-handler))
                      (define (run-prefix)
                        (prefix)
                        (set! prefix void))
                      (uncaught-exception-handler
                       (lambda (exn)
                         (if (exn:break? exn)
                             (begin
                               (run-prefix)
                               ((exn:break-continuation exn)))
                             (orig exn))))
                      (call-with-continuation-prompt
                       (lambda ()
                         (semaphore-post ready-s)
                         (semaphore-wait s)
                         (run-prefix)
                         (set! results
                               (call-with-values thunk list)))
                       the-root-continuation-prompt-tag))))
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
    (sync t t2 (thread-suspend-evt t))
    (cond
     [(thread-dead? t)
      (apply complete 0 results)]
     [else
      (expire go)]))
  go)

(define (engine-block)
  (thread-suspend (current-thread)))

(define the-root-continuation-prompt-tag (make-continuation-prompt-tag 'root))
(define (root-continuation-prompt-tag) the-root-continuation-prompt-tag)

(primitive-table '#%engine
                 (hash 
                  'make-engine
                  make-engine
                  'engine-block
                  engine-block
                  'engine-return
                  (lambda args
                    (error "engine-return: not ready"))
                  'root-continuation-prompt-tag
                  root-continuation-prompt-tag))
