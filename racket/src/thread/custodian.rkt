#lang racket/base
(require "check.rkt")

(provide current-custodian
         make-custodian
         custodian?
         custodian-shutdown-all
         custodian-managed-list
         make-custodian-box
         custodian-box?
         custodian-box-value
         custodian-memory-accounting-available?
         custodian-require-memory
         custodian-limit-memory

         unsafe-make-custodian-at-root
         unsafe-custodian-register
         unsafe-custodian-unregister)

(struct custodian ([parent])
  #:authentic)
(struct custodian-box (v)
  #:authentic)

(define root-custodian (custodian #f))

(define/who current-custodian
  (make-parameter root-custodian
                  (lambda (v)
                    (check who custodian? v)
                    v)))

(define/who (make-custodian [parent (current-custodian)])
  (check who custodian? parent)
  (custodian parent))

(define (unsafe-make-custodian-at-root)
  (make-custodian root-custodian))

(define/who (custodian-shutdown-all c)
  (check who custodian? c)
  (void))

(define/who (custodian-managed-list c super-c)
  (check who custodian? c)
  (check who custodian? super-c)
  '())

(define (custodian-memory-accounting-available?)
  #f)

(define/who (custodian-require-memory limit-cust need-amt stop-cust)
  (check who custodian? limit-cust)
  (check who exact-nonnegative-integer? need-amt)
  (check who custodian? stop-cust)
  (raise (exn:fail:unsupported
          "custodian-require-memory: unsupported"
          (current-continuation-marks))))

(define/who (custodian-limit-memory limit-cust need-amt [stop-cust limit-cust])
  (check who custodian? limit-cust)
  (check who exact-nonnegative-integer? need-amt)
  (check who custodian? stop-cust)
  (raise (exn:fail:unsupported
          "custodian-limit-memory: unsupported"
          (current-continuation-marks))))

;; ----------------------------------------

(define (make-custodian-box v)
  (custodian-box v))
  
(define/who (custodian-box-value cb)
  (check who custodian-box? cb)
  (custodian-box-v cb))

;; ----------------------------------------

(define (unsafe-custodian-register cust obj callback at-exit? init-weak?)
  (void))

(define (unsafe-custodian-unregister mref obj)
  (void))
