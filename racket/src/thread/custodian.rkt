#lang racket/base
(require "check.rkt")

(provide current-custodian
         make-custodian
         make-custodian-from-main
         custodian?
         custodian-shutdown-all
         custodian-managed-list
         make-custodian-box
         custodian-box?
         custodian-box-value
         custodian-memory-accounting-available?
         custodian-require-memory
         custodian-limit-memory)

(struct custodian ([parent])
  #:authentic)
(struct custodian-box (v)
  #:authentic)

(define root-custodian (custodian #f))

(define current-custodian
  (make-parameter root-custodian
                  (lambda (v)
                    (check 'current-custodian custodian? v)
                    v)))

(define (make-custodian [parent (current-custodian)])
  (check 'make-custodian custodian? parent)
  (custodian parent))

(define (make-custodian-from-main)
  (make-custodian root-custodian))

(define (custodian-shutdown-all c)
  (check 'custodian-shutdown-all custodian? c)
  (void))

(define (custodian-managed-list c super-c)
  (check 'custodian-managed-list custodian? c)
  (check 'custodian-managed-list custodian? super-c)
  '())

(define (custodian-memory-accounting-available?)
  #f)

(define (custodian-require-memory limit-cust need-amt stop-cust)
  (check 'custodian-require-memory custodian? limit-cust)
  (check 'custodian-require-memory exact-nonnegative-integer? need-amt)
  (check 'custodian-require-memory custodian? stop-cust)
  (raise (exn:fail:unsupported
          "custodian-require-memory: unsupported"
          (current-continuation-marks))))

(define (custodian-limit-memory limit-cust need-amt [stop-cust limit-cust])
  (check 'custodian-limit-memory custodian? limit-cust)
  (check 'custodian-limit-memory exact-nonnegative-integer? need-amt)
  (check 'custodian-limit-memory custodian? stop-cust)
  (raise (exn:fail:unsupported
          "custodian-limit-memory: unsupported"
          (current-continuation-marks))))

;; ----------------------------------------

(define (make-custodian-box v)
  (custodian-box v))
  
(define (custodian-box-value cb)
  (check 'custodian-box-value custodian-box? cb)
  (custodian-box-v cb))
