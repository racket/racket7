#lang racket/base
(require "internal-error.rkt"
         (only-in '#%linklet primitive-table))

(provide make-engine
         engine-block
         engine-return
         set-ctl-c-handler!
         root-continuation-prompt-tag
         break-enabled-key
         ;; Just `exn:break`, but the host may need
         ;; to distinguish breaks raised by the thread
         ;; implementation:
         exn:break/non-engine)

(define-values (make-engine engine-block engine-return
                            set-ctl-c-handler!
                            root-continuation-prompt-tag break-enabled-key
                            exn:break/non-engine)
  (let ([ht (primitive-table '#%engine)])
    (unless ht
      (internal-error "engines not provided by host"))
    (values
     (hash-ref ht 'make-engine)
     (hash-ref ht 'engine-block)
     (hash-ref ht 'engine-return)
     (hash-ref ht 'set-ctl-c-handler!)
     (hash-ref ht 'root-continuation-prompt-tag)
     (hash-ref ht 'break-enabled-key)
     (hash-ref ht 'exn:break/non-engine))))
