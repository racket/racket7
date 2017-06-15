#lang racket/base
(require "internal-error.rkt"
         (only-in '#%linklet primitive-table))

(provide make-engine
         engine-block
         engine-return
         current-engine-state
         current-process-milliseconds
         set-ctl-c-handler!
         root-continuation-prompt-tag
         break-enabled-key
         set-break-enabled-transition-hook!
         ;; Just `exn:break`, but the host may need
         ;; to distinguish breaks raised by the thread
         ;; implementation:
         exn:break/non-engine
         exn:break:hang-up/non-engine
         exn:break:terminate/non-engine

         internal-make-thread-parameter
         fork-pthread
         pthread?
         get-pthread-id
         chez:make-condition
         chez:condition-wait
         chez:condition-signal
         chez:condition-broadcast
         chez:make-mutex
         chez:mutex-acquire
         chez:mutex-release
         threaded?)

(define-values (make-engine engine-block engine-return current-engine-state
                            current-process-milliseconds
                            set-ctl-c-handler!
                            root-continuation-prompt-tag
                            break-enabled-key
                            set-break-enabled-transition-hook!
                            exn:break/non-engine
                            exn:break:hang-up/non-engine
                            exn:break:terminate/non-engine
                            internal-make-thread-parameter
                            fork-pthread
                            pthread?
                            get-pthread-id
                            chez:make-condition
                            chez:condition-wait
                            chez:condition-signal
                            chez:condition-broadcast
                            chez:make-mutex
                            chez:mutex-acquire
                            chez:mutex-release
                            threaded?)
  (let ([ht (primitive-table '#%engine)])
    (unless ht
      (internal-error "engines not provided by host"))
    (values
     (hash-ref ht 'make-engine)
     (hash-ref ht 'engine-block)
     (hash-ref ht 'engine-return)
     (hash-ref ht 'current-engine-state)
     (hash-ref ht 'current-process-milliseconds)
     (hash-ref ht 'set-ctl-c-handler!)
     (hash-ref ht 'root-continuation-prompt-tag)
     (hash-ref ht 'break-enabled-key)
     (hash-ref ht 'set-break-enabled-transition-hook!)
     (hash-ref ht 'exn:break/non-engine)
     (hash-ref ht 'exn:break:hang-up/non-engine)
     (hash-ref ht 'exn:break:terminate/non-engine)
     (hash-ref ht 'internal-make-thread-parameter)
     (hash-ref ht 'fork-pthread)
     (hash-ref ht 'pthread?)
     (hash-ref ht 'get-thread-id)
     (hash-ref ht 'make-condition)
     (hash-ref ht 'condition-wait)
     (hash-ref ht 'condition-signal)
     (hash-ref ht 'condition-broadcast)
     (hash-ref ht 'make-mutex)
     (hash-ref ht 'mutex-acquire)
     (hash-ref ht 'mutex-release)
     (hash-ref ht 'threaded?)
     )))
