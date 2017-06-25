#lang racket/base
(require "sandman.rkt")

(provide make-schedule-info

         schedule-info-did-work?
         schedule-info-exts

         schedule-info-add-timeout-at!
         schedule-info-did-work!)

;; A `schedule-info` record allows an event poller to communicate
;; extra information to the scheduler when an even is not ready.

(struct schedule-info (did-work?
                       exts) ; <ext-event-set> for the sandman
  #:mutable)

(define (make-schedule-info #:did-work? [did-work? #t])
  (schedule-info did-work?
                 #f))

(define (schedule-info-add-timeout-at! sched-info timeout-at)
  (define exts (schedule-info-exts sched-info))
  (set-schedule-info-exts! sched-info
                           (sandman-merge-timeout exts timeout-at)))

(define (schedule-info-did-work! sched-info)
  (set-schedule-info-did-work?! sched-info #t))
