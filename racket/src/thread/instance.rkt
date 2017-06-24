#lang racket/base
(require "evt.rkt"
         "atomic.rkt"
         "thread.rkt")

;; Unsafe scheduler-cooperation functions are made available to
;; clients through a `#%evt` primitive linklet instance:

(provide #%evt-instance)

(define #%evt-instance
  (hasheq 'poller poller
          'poll-ctx-poll? poll-ctx-poll?
          'poll-ctx-select-proc poll-ctx-select-proc
          'control-state-evt control-state-evt
          'async-evt async-evt
          
          'start-atomic start-atomic
          'end-atomic end-atomic
          'thread-push-kill-callback! thread-push-kill-callback!
          'thread-pop-kill-callback! thread-pop-kill-callback!))
