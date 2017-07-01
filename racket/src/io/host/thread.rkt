#lang racket/base
(require (only-in '#%linklet primitive-table))

(provide atomically
         non-atomically
         check-current-custodian)

(define table
  (or (primitive-table '#%thread)
      (error '#%thread "scheduler cooperation not supported by host")))

(define-syntax bounce
  (syntax-rules ()
    [(_ id)
     (begin
       (provide id)
       (define id (hash-ref table 'id)))]
    [(_ id ...)
     (begin (bounce id) ...)]))

(bounce make-semaphore
        semaphore-post
        semaphore-wait
        semaphore-peek-evt
        wrap-evt
        sync
        sync/timeout
        evt?
        sync-atomic-poll-evt?
        prop:evt
        poller
        poll-ctx-poll?
        poll-ctx-select-proc
        poll-ctx-sched-info
        control-state-evt
        async-evt
        schedule-info-current-exts
        current-sandman
        start-atomic
        end-atomic
        current-custodian
        custodian-shut-down?
        unsafe-custodian-register
        unsafe-custodian-unregister
        thread-push-kill-callback!
        thread-pop-kill-callback!)

(define-syntax-rule (atomically e ...)
  (begin
    (start-atomic)
    (begin0
      (let () e ...)
      (end-atomic))))

(define-syntax-rule (non-atomically e ...)
  (begin
    (end-atomic)
    (begin0
      (let () e ...)
      (start-atomic))))

;; in atomic mode
(define (check-current-custodian who)
  (when (custodian-shut-down? (current-custodian))
    (end-atomic)
    (raise
     (exn:fail
      (string-append (symbol->string who) ": the current custodian has been shut down")
      (current-continuation-marks)))))
