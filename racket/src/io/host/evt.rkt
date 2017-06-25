#lang racket/base
(require (only-in '#%linklet primitive-table))

(define table
  (or (primitive-table '#%evt)
      (error '#%evt "scheduler cooperation not supported by host")))

(define-syntax bounce
  (syntax-rules ()
    [(_ id)
     (begin
       (provide id)
       (define id (hash-ref table 'id)))]
    [(_ id ...)
     (begin (bounce id) ...)]))

(bounce sync
        evt?
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
        thread-push-kill-callback!
        thread-pop-kill-callback!)
