#lang racket/base
(require (only-in '#%linklet primitive-table))

;; Approximate scheduler cooperation where `async-evt` can be used
;; within the dynamic extent of a `poller` callback to mean that the
;; poller is selected. Since `nack` propagation is based on a thread,
;; this approximation won't work right if an event is actually
;; contented.

(struct poller (proc)
  #:property prop:procedure
  (lambda (p s)
    (define async-sema (make-semaphore))
    (poll-guard-evt
     (lambda (poll?)
       (parameterize ([current-async-semaphore async-sema])
         (define-values (results new-evt)
           ((poller-proc p) s (poll-ctx poll? (lambda () (semaphore-post async-sema)))))
         (if results
             (wrap-evt always-evt (lambda (v) (apply values results)))
             new-evt))))))

(struct poll-ctx (poll? select-proc))

(struct control-state-evt (evt interrupt abandon retry)
  #:property prop:evt (lambda (cse)
                        (nack-guard-evt
                         (lambda (nack)
                           (thread (lambda () (sync nack) ((control-state-evt-abandon cse))))
                           (control-state-evt-evt cse)))))

(define current-async-semaphore (make-parameter #f))

(define (async-evt)
  (or (current-async-semaphore)
      (error 'async-evt "not in a `poller` callback")))
  
(primitive-table '#%evt
                 (hasheq 'poller poller
                         'poll-ctx-poll? poll-ctx-poll?
                         'poll-ctx-select-proc poll-ctx-select-proc
                         'control-state-evt control-state-evt
                         'async-evt async-evt))
