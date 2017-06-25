#lang racket/base
(require (only-in '#%linklet primitive-table)
         "../../thread/sandman.rkt"
         ffi/unsafe/atomic
         "bootstrap-rktio.rkt")

;; Approximate scheduler cooperation where `async-evt` can be used
;; within the dynamic extent of a `poller` callback to mean that the
;; poller is selected. Since `nack` propagation is based on a thread,
;; this approximation won't work right if an event is actually
;; contended.

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

(define (poll-ctx-sched-info ctx) #f)

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

(define current-kill-callbacks (make-parameter '()))

(define (thread-push-kill-callback! p)
  (current-kill-callbacks (cons p (current-kill-callbacks))))

(define (thread-pop-kill-callback!)
  (current-kill-callbacks (cdr (current-kill-callbacks))))

(define schedule-info-current-exts
  (case-lambda
    [() #f]
    [(v) (void)]))

(primitive-table '#%evt
                 (hasheq 'sync sync
                         'evt? evt?
                         'prop:evt prop:evt
                         'poller poller
                         'poll-ctx-poll? poll-ctx-poll?
                         'poll-ctx-select-proc poll-ctx-select-proc
                         'poll-ctx-sched-info poll-ctx-sched-info
                         'control-state-evt control-state-evt
                         'async-evt async-evt
                         'schedule-info-current-exts schedule-info-current-exts
                         'current-sandman current-sandman
                         'start-atomic start-atomic
                         'end-atomic end-atomic
                         'thread-push-kill-callback! thread-push-kill-callback!
                         'thread-pop-kill-callback! thread-pop-kill-callback!))
