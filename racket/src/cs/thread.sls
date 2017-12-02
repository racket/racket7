(library (thread)
  (export)
  (import (chezpart)
          (rename (only (chezscheme)
                        sleep
                        printf)
                  [sleep chez:sleep])
          (rename (rumble)
                  [rumble:break-enabled-key break-enabled-key]
                  ;; These are extracted via `#%linklet`:
                  [make-engine rumble:make-engine]
                  [engine-block rumble:engine-block]
                  [engine-return rumble:engine-return]
                  [current-engine-state rumble:current-engine-state]
                  [make-condition rumble:make-condition]
                  [condition-wait rumble:condition-wait]
                  [condition-signal rumble:condition-signal]
                  [condition-broadcast rumble:condition-broadcast]
                  [make-mutex rumble:make-mutex]
                  [mutex-acquire rumble:mutex-acquire]
                  [mutex-release rumble:mutex-release]
                  [pthread? rumble:thread?]
                  [fork-pthread rumble:fork-thread]
                  [threaded? rumble:threaded?]
                  [get-thread-id rumble:get-thread-id]
                  [internal-make-thread-parameter rumble:make-thread-parameter]
                  [set-ctl-c-handler! rumble:set-ctl-c-handler!]
                  [root-continuation-prompt-tag rumble:root-continuation-prompt-tag]
                  [set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!]))

  (define (exit n)
    (chez:exit n))

  (define (sleep secs)
    (define isecs (inexact->exact (floor secs)))
    (chez:sleep (make-time 'time-duration
                           (inexact->exact (floor (* (- secs isecs) 1e9)))
                           isecs)))

  (define (primitive-table key)
    (case key
      [(|#%engine|) (hash
                     'make-engine rumble:make-engine
                     'engine-block rumble:engine-block
                     'engine-return rumble:engine-return
                     'current-engine-state rumble:current-engine-state
                     'set-ctl-c-handler! rumble:set-ctl-c-handler!
                     'root-continuation-prompt-tag rumble:root-continuation-prompt-tag
                     'break-enabled-key break-enabled-key
                     'set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!
                     'exn:break/non-engine exn:break
                     'exn:break:hang-up/non-engine exn:break:hang-up
                     'exn:break:terminate/non-engine exn:break:terminate
                     'current-process-milliseconds cpu-time
                     'internal-make-thread-parameter rumble:make-thread-parameter
                     'fork-pthread rumble:fork-thread
                     'pthread? rumble:thread?
                     'get-thread-id rumble:get-thread-id
                     'make-condition rumble:make-condition
                     'condition-wait rumble:condition-wait
                     'condition-signal rumble:condition-signal
                     'condition-broadcast rumble:condition-broadcast
                     'make-mutex rumble:make-mutex
                     'mutex-acquire rumble:mutex-acquire
                     'mutex-release rumble:mutex-release
                     'active-pthreads active-pthreads
                     'collect-garbage-pending-major? collect-garbage-pending-major?
                     'collect-garbage-pending-minor? collect-garbage-pending-minor?
                     'threaded? rumble:threaded?
                     )]
      [else #f]))


  ;; Tie knots:
  (define (check-for-break) (1/check-for-break))
  (define (break-enabled) (1/break-enabled))

  (include "compiled/thread.scm")

  (set-engine-exit-handler!
   (lambda (v)
     (|#%app| (|#%app| 1/exit-handler) v)))

  (set-scheduler-lock-callbacks! (lambda () (1/make-semaphore 1))
                                 1/semaphore-wait
                                 1/semaphore-post)

  (set-future-callbacks! 1/future? 1/current-future
                         future-block future-wait current-future-prompt
			 halt-workers resume-workers))
