#lang racket/base
(require '#%linklet)

;; Simulate engines by using the host system's threads.

;; This simulation does not support using an exception
;; handler in an engine.

(define (make-engine thunk init-break-enabled-cell)
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
                         (with-continuation-mark
                             break-enabled-key
                           init-break-enabled-cell
                           (call-with-continuation-prompt
                            (lambda ()
                              (semaphore-post ready-s)
                              (semaphore-wait s)
                              (run-prefix)
                              (set! results
                                    (call-with-values thunk list)))
                            the-root-continuation-prompt-tag
                            (lambda (exn)
                              ((error-display-handler) (exn-message exn) exn)))))))))
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
(define break-enabled-key (gensym 'break-enabled))

(struct exn:break/non-engine exn:break ())

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
                  root-continuation-prompt-tag
                  'break-enabled-key
                  break-enabled-key
                  'exn:break/non-engine
                  exn:break/non-engine))
