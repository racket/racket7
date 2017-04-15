#lang racket/base

(provide prop:waiter
         make-waiter-methods
         waiter-resume!
         waiter-suspend!
         
         select-waiter)

;; A waiter can be in the queue for a semaphore or
;; channel
(define-values (prop:waiter waiter? waiter-ref)
  (make-struct-type-property 'waiter))

(struct waiter-methods (suspend resume))

(define (make-waiter-methods #:suspend! suspend
                             #:resume! resume)
  (waiter-methods suspend resume))
                             

(define (waiter-resume! w s)
  ((waiter-methods-resume (waiter-ref w)) w s))

(define (waiter-suspend! w cb)
  ((waiter-methods-suspend (waiter-ref w)) w cb))

;; Used for semaphores and channels to run a "just selected" callback
;; when synchronized:
(struct select-waiter (proc)
        #:property prop:waiter
        (make-waiter-methods #:suspend! void
                             #:resume! (lambda (w s)
                                         ((select-waiter-proc w)))))

