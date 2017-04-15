#lang racket/base
(require '#%linklet)

;; Simulate engines by using the host system's threads.

(define (make-engine thunk identity)
  (define s (make-semaphore))
  (define results #f)
  (define t (thread (lambda ()
                      (semaphore-wait s)
                      (set! results
                            (call-with-values thunk list)))))
  (thread-suspend t)
  (semaphore-post s)
  (define (go ticks complete expire)
    (thread-resume t)
    (define t2
      (thread (lambda ()
                (sleep (/ ticks 10000.0))
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

(primitive-table '#%engine
                 (hash 
                  'make-engine
                  make-engine
                  'engine-block
                  engine-block
                  'engine-return
                  (lambda args
                    (error "engine-return: not ready"))))
