#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt")

;; Compilation generates a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide instance-imports
         make-instance-instance
         make-module-body-instance-instance)

(define instance-imports
  `(,ns-id
    ,phase-shift-id
    ,self-id
    ,inspector-id               ; declaration-time inspector to grant to syntax objects
    ,bulk-binding-registry-id   ; declaration-time registry to connect to bulk bindings
    ,set-transformer!-id))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:inspector inspector
                                #:bulk-binding-registry bulk-binding-registry
                                #:set-transformer! set-transformer!)
  (make-instance 'instance #f
                 ns-id ns
                 phase-shift-id phase-shift
                 self-id self
                 inspector-id inspector
                 bulk-binding-registry-id bulk-binding-registry
                 set-transformer!-id set-transformer!))

(define (make-module-body-instance-instance #:set-transformer! set-transformer!)
  (make-instance 'body-instance #f
                 set-transformer!-id set-transformer!))
