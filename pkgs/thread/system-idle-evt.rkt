#lang racket/base
(require "evt.rkt"
         "semaphore.rkt")

(provide system-idle-evt
         post-idle)

(define idle-sema (make-semaphore))
(define wrapped-idle-sema (wrap-evt idle-sema void))
(struct -system-idle-evt ()
        #:property prop:evt (lambda (i) wrapped-idle-sema)
        #:reflection-name 'system-idle-evt)

(define the-idle-evt (-system-idle-evt))

(define (system-idle-evt)
  the-idle-evt)

;; In atomic mode:
(define (post-idle)
  (and (semaphore-any-waiters? idle-sema)
       (begin
         (semaphore-post idle-sema)
         #t)))


