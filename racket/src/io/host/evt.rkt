#lang racket/base
(require (only-in '#%linklet primitive-table))

(provide poller
         poll-ctx-poll?
         poll-ctx-select-proc
         control-state-evt
         async-evt
         start-atomic
         end-atomic
         thread-push-kill-callback!
         thread-pop-kill-callback!)

(define-values (poller
                poll-ctx-poll?
                poll-ctx-select-proc
                control-state-evt
                async-evt
                start-atomic
                end-atomic
                thread-push-kill-callback!
                thread-pop-kill-callback!)
  (let ([ht (primitive-table '#%evt)])
    (unless ht
      (error '#%evt "scheduler cooperation not supported by host"))
    (values
     (hash-ref ht 'poller)
     (hash-ref ht 'poll-ctx-poll?)
     (hash-ref ht 'poll-ctx-select-proc)
     (hash-ref ht 'control-state-evt)
     (hash-ref ht 'async-evt)
     
     (hash-ref ht 'start-atomic)
     (hash-ref ht 'end-atomic)
     (hash-ref ht 'thread-push-kill-callback!)
     (hash-ref ht 'thread-pop-kill-callback!))))
