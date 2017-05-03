#lang racket/base
(require racket/unsafe/ops)

(provide prop:evt
         evt?
         evt-poll

         (rename-out [the-never-evt never-evt]
                     [the-always-evt always-evt]
                     [the-async-evt async-evt])
         never-evt?
         async-evt?

         (struct-out wrap-evt)
         (struct-out handle-evt)
         (struct-out control-state-evt)
         (struct-out guard-evt)

         (struct-out poller)
         (struct-out poll-ctx))

(define-values (prop:evt evt? evt-ref)
  (make-struct-type-property 'evt))

;; A poller as a `prop:evt` value wraps a procedure
;;   evt poll-ctx -> (values results-or-#f replacing-evt-or-#f)
;; where either a list of results is returned, indicating
;; that the event is selected, or a replacement event
;; is returned (possibly unchanged).
;; If a poller does any work that can allow some thread to
;; become unblocked, then it must tell the scheduler via
;; `sechedule-info-did-work!`.
(struct poller (proc))

(struct poll-ctx (poll?         ; whether events are being polled once (i.e., 0 timeout)
                  select-proc   ; callback to asynchornously select the event being polled
                  sched-info))  ; instructions to the scheduler, such as timeouts

(struct never-evt ()
        #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))
(define the-never-evt (never-evt))

(struct always-evt ()
        #:property prop:evt (poller (lambda (self poll-ctx) (values (list self) #f))))
(define the-always-evt (always-evt))

;; A placeholder for an event that will be selected through a callback
;; instead of polling:
(struct async-evt ()
        #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))
(define the-async-evt (async-evt))

(struct wrap-evt (evt wrap)
        #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))
(struct handle-evt wrap-evt ())

(struct control-state-evt (evt
                           interrupt-proc ; thunk for break/kill initiated
                           abandon-proc ; thunk for break/kill complete
                           retry-proc) ; thunk for resume from break; return `(values _val _ready?)`
        #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))

(struct guard-evt (proc)
        #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))

;; Check whether an event is ready; returns the same results
;; as a poller
(define (evt-poll evt poll-ctx)
  (let* ([v (evt-ref evt)]
         [v (if (fixnum? v)
                (unsafe-struct-ref evt v)
                v)]
         [v (if (procedure? v)
                (v evt)
                v)])
    (cond
     [(evt? v) (values #f v)]
     [(poller? v) ((poller-proc v) evt poll-ctx)]
     [else (values #f never-evt)])))
