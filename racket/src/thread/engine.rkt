#lang racket/base
(require "internal-error.rkt"
         (only-in '#%linklet primitive-table)
         (for-syntax racket/base))

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ bind ...)
     (with-syntax ([([orig-id here-id] ...)
                    (for/list ([bind (in-list (syntax->list #'(bind ...)))])
                      (if (identifier? bind)
                          (list bind bind)
                          bind))])
       #'(begin
           (provide here-id ...)
           (define-values (here-id ...)
             (let ([ht (primitive-table '#%engine)])
               (unless ht
                 (internal-error "engines not provided by host"))
               (values
                (hash-ref ht 'orig-id)
                ...)))))]))

(bounce make-engine
        engine-block
        engine-return
        current-engine-state
        current-process-milliseconds
        set-ctl-c-handler!
        root-continuation-prompt-tag
        break-enabled-key
        set-break-enabled-transition-hook!
        [continuation-marks host:continuation-marks]

        [poll-will-executors host:poll-will-executors]
        [make-will-executor host:make-will-executor]
        [make-stubborn-will-executor host:make-stubborn-will-executor]
        [will-executor? host:will-executor?]
        [will-register host:will-register]
        [will-try-execute host:will-try-execute]

        ;; Just `exn:break`, etc., but the host may need
        ;; to distinguish breaks raised by the thread
        ;; implementation:
        exn:break/non-engine
        exn:break:hang-up/non-engine
        exn:break:terminate/non-engine

        internal-make-thread-parameter
        fork-pthread
        pthread?
        [get-thread-id get-pthread-id]
        [make-condition chez:make-condition]
        [condition-wait chez:condition-wait]
        [condition-signal chez:condition-signal]
        [condition-broadcast chez:condition-broadcast]
        [make-mutex chez:make-mutex]
        [mutex-acquire chez:mutex-acquire]
        [mutex-release chez:mutex-release]
        [active-pthreads chez:active-threads]
        [collect-garbage-pending-major? chez:collect-garbage-pending-major?]
        [collect-garbage-pending-minor? chez:collect-garbage-pending-minor?]
        threaded?)
