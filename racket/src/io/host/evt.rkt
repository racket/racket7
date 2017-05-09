#lang racket/base
(require (only-in '#%linklet primitive-table))

(provide poller
         poll-ctx-poll?
         poll-ctx-select-proc
         control-state-evt
         async-evt)

(define-values (poller
                poll-ctx-poll?
                poll-ctx-select-proc
                control-state-evt
                async-evt)
  (let ([ht (primitive-table '#%evt)])
    (unless ht
      (error '#%evt "scheduler cooperation not supported by host"))
    (values
     (hash-ref ht 'poller)
     (hash-ref ht 'poll-ctx-poll?)
     (hash-ref ht 'poll-ctx-select-proc)
     (hash-ref ht 'control-state-evt)
     (hash-ref ht 'async-evt))))
