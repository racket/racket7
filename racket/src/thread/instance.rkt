#lang racket/base
(require "evt.rkt"
         "sync.rkt"
         "schedule-info.rkt"
         "sandman.rkt"
         "atomic.rkt"
         "thread.rkt")

;; Unsafe scheduler-cooperation functions are made available to
;; clients through a `#%evt` primitive linklet instance:

(provide #%evt-instance)

(define #%evt-instance
  (hasheq 'sync sync
          'evt? evt?
          'prop:evt prop:evt
          'poller poller
          'poll-ctx-poll? poll-ctx-poll?
          'poll-ctx-select-proc poll-ctx-select-proc
          'poll-ctx-sched-info poll-ctx-sched-info
          'control-state-evt control-state-evt
          'async-evt async-evt
          'current-sandman current-sandman
          'schedule-info-current-exts schedule-info-current-exts
          'schedule-info-did-work! schedule-info-did-work!
          'start-atomic start-atomic
          'end-atomic end-atomic
          'thread-push-kill-callback! thread-push-kill-callback!
          'thread-pop-kill-callback! thread-pop-kill-callback!))
