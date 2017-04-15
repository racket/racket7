#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "queue.rkt")

(provide make-channel
         channel?
         channel-put
         channel-get
         
         channel-put-evt)

(struct channel (get-queue
                 put-queue)
        #:property
        prop:evt
        (poller (lambda (ch poll-ctx)
                  (channel-get/poll ch poll-ctx))))

(struct channel-put-evt (ch v)
        #:property
        prop:evt
        (poller (lambda (cp poll-ctx)
                  (channel-put/poll (channel-put-evt-ch cp)
                                    (channel-put-evt-v cp)
                                    cp
                                    poll-ctx))))

;; A channel must not match get and put from the same thread, which is
;; a danger when `sync` queues up multiple events at a time:
(struct channel-select-waiter select-waiter (thread))

(define (make-channel)
  (channel (make-queue) (make-queue)))

;; ----------------------------------------

(define (channel-get ch)
  (check 'channel-get channel? ch)
  (define b (box #f))
  ((atomically
    (define pw+v (queue-remove! (channel-put-queue ch)))
    (define gw (current-thread))
    (cond
     [(not pw+v)
      (define gq (channel-get-queue ch))
      (define n (queue-add! gq (cons gw b)))
      (waiter-suspend! gw (lambda () (queue-remove-node! gq n)))]
     [else
      (set-box! b (cdr pw+v))
      (waiter-resume! (car pw+v) (void))
      void])))
  (unbox b))

(define (channel-get/poll ch poll-ctx)
  ;; Similar to `channel-get`, but works in terms of a
  ;; `select-waiter` instead of a thread
  (define pw+v (queue-fremove! (channel-put-queue ch)
                               not-matching-select-waiter))
  (cond
   [pw+v
    (waiter-resume! (car pw+v) (void))
    (values (list (cdr pw+v)) #f)]
   [(poll-ctx-poll? poll-ctx)
    (values #f never-evt)]
   [else
    (define b (box #f))
    (define gq (channel-get-queue ch))
    (define gw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread)))
    (define n (queue-add! gq (cons gw b)))
    (values #f
            (wrap-evt
             (nack-evt async-evt
                       (lambda () (queue-remove-node! gq n)))
             (lambda (v) (unbox b))))]))

;; ----------------------------------------


(define (channel-put ch v)
  (check 'channel-put channel? ch)
  ((atomically
    (define gw+b (queue-remove! (channel-get-queue ch)))
    (define pw (current-thread))
    (cond
     [(not gw+b)
      (define pq (channel-put-queue ch))
      (define n (queue-add! pq (cons pw v)))
      (waiter-suspend! pw (lambda () (queue-remove-node! pq n)))]
     [else
      (set-box! (cdr gw+b) v)
      (waiter-resume! (car gw+b) v)
      void])))
  (void))

(define (channel-put/poll ch v result poll-ctx)
  ;; Similar to `channel-put`, but works in terms of a
  ;; `select-waiter` instead of a thread
  (define gw+b (queue-fremove! (channel-get-queue ch)
                               not-matching-select-waiter))
  (cond
   [(not gw+b)
    (define pq (channel-put-queue ch))
    (define pw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread)))
    (define n (queue-add! pq (cons pw v)))
    (values #f
            (wrap-evt
             (nack-evt async-evt
                       (lambda () (queue-remove-node! pq n)))
             (lambda (v) result)))]
   [(poll-ctx-poll? poll-ctx)
    (values #f async-evt)]
   [else
    (set-box! (cdr gw+b) v)
    (waiter-resume! (car gw+b) v)
    (values (list result) #f)]))

;; ----------------------------------------

(define (not-matching-select-waiter w+b/v)
  (define w (car w+b/v))
  (or (not (channel-select-waiter? w))
      (not (eq? (current-thread)
                (channel-select-waiter-thread w)))))
