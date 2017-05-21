#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt")

(provide (rename-out [progress-evt?* progress-evt?])
         port-provides-progress-evts?
         port-progress-evt
         port-commit-peeked

         check-progress-evt)

(struct progress-evt (port evt)
  #:property prop:evt 1)

(define progress-evt?*
  (let ([progress-evt?
         (case-lambda
           [(v) (progress-evt? v)]
           [(v port)
            (and (progress-evt? v)
                 (eq? port (progress-evt-port port)))])])
    progress-evt?))

;; ----------------------------------------

(define (port-provides-progress-evts? in)
  (check 'port-provides-progress-evts? input-port? in)
  (let ([in (->core-input-port in)])
    (and (core-input-port-get-progress-evt in) #t)))

(define (port-progress-evt orig-in)
  (check 'port-progress-evts? input-port? orig-in)
  (let ([in (->core-input-port orig-in)])
    (define get-progress-evt (core-input-port-get-progress-evt in))
    (if get-progress-evt
        (progress-evt orig-in (get-progress-evt))
        (raise-arguments-error 'port-progress-evt
                               "port does not provide progress evts"
                               "port" orig-in))))

(define (port-commit-peeked amt progress-evt evt [in (current-input-port)])
  (check 'port-commit-peeked exact-nonnegative-integer? amt)
  (check 'port-commit-peeked progress-evt? progress-evt)
  (check 'port-commit-peeked (lambda (p)
                               (or (channel-put-evt? evt)
                                   (channel? evt)
                                   (semaphore? evt)
                                   (semaphore-peek-evt? evt)
                                   (eq? always-evt evt)
                                   (eq? never-evt evt)))
         #:contract "(or/c channel-put-evt? channel? semaphore? semaphore-peek-evt? always-evt never-evt)"
         evt)
  (check 'port-commit-peeked input-port? in)
  (check-progress-evt 'port-commit-peeked progress-evt in)
  (let ([in (->core-input-port in)])
    (define commit (core-input-port-commit in))
    (commit amt (progress-evt-evt progress-evt) evt)))

(define (check-progress-evt who progress-evt in)
  (unless (progress-evt?* progress-evt in)
    (raise-arguments-error who "evt is not a progress evt for the given port"
                           "evt" progress-evt
                           "port" in)))
