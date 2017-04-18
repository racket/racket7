#lang racket/base

(provide make-schedule-info

         schedule-info-did-work?
         schedule-info-timeout-at

         schedule-info-add-timeout-at!
         schedule-info-did-work!)

;; A `schedule-info` record allows an event poller to communicate
;; extra information to the scheduler when an even is not ready.

(struct schedule-info (did-work? timeout-at) #:mutable)

(define (make-schedule-info #:did-work? [did-work? #t]
                            #:timeout-at [timeout-at #f])
  (schedule-info did-work?
                 timeout-at))

(define (schedule-info-add-timeout-at! sched-info timeout-at)
  (define tm (schedule-info-timeout-at sched-info))
  (set-schedule-info-timeout-at! sched-info
                                 (if tm
                                     (min tm timeout-at)
                                     timeout-at)))

(define (schedule-info-did-work! sched-info)
  (set-schedule-info-did-work?! sched-info #t))
