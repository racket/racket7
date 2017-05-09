#lang racket/base
(require "atomic.rkt"
         "thread.rkt")

(provide unsafe-start-atomic
         unsafe-end-atomic
         unsafe-start-breakable-atomic
         unsafe-end-breakable-atomic
         unsafe-in-atomic?)

(define (unsafe-start-breakable-atomic)
  (start-atomic))

(define (unsafe-end-breakable-atomic)
  (start-atomic))

(define (unsafe-start-atomic)
  (start-atomic)
  (current-break-suspend (add1 (current-break-suspend))))

(define (unsafe-end-atomic)
  (define bs (sub1 (current-break-suspend)))
  (current-break-suspend bs)
  (end-atomic)
  (when (zero? bs)
    (check-for-break)))

(define (unsafe-in-atomic?)
  (positive? (current-atomic)))
