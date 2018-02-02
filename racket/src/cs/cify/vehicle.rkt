#lang racket/base
(require "arg.rkt"
         "sort.rkt"
         "id.rkt"
         "union-find.rkt")

(provide (struct-out lam)
         (struct-out vehicle)
         make-lam
         lam-add-transitive-tail-apply!
         merge-vehicles!)

(struct lam (id e
                [free-var-refs #:mutable]
                [env #:mutable]
                loop-targets
                [max-jump-argc #:mutable]
                [need-entry? #:mutable]
                [vehicle #:mutable]
                [index #:mutable]
                [under-lambda? #:mutable] ; inside another function?
                [moved-to-top? #:mutable]
                [unused? #:mutable]       ; in case a `lambda` gets dropped completely
                [can-tail-apply? #:mutable]   ; if any in vehicle can tail apply
                [transitive-tail-applies #:mutable] ; table used for fixpoint calculation of `can-tail-apply?`
                [can-leaf? #:mutable]))   ; never syncs runstack => GC-independent leaf

(struct vehicle ([id #:mutable]
                 [lams #:mutable]
                 [closure? #:mutable]
                 [min-argc #:mutable]
                 [max-jump-argc #:mutable]
                 [max-runstack-depth #:mutable]
                 [overflow-check? #:mutable])) ; if the vehicle can be called directly

(define (make-lam id e #:can-call-direct? [can-call-direct? #f])
  (define-values (min-argc max-argc) (lambda-arity e))
  (define a-vehicle (vehicle id '() #f min-argc 0 0 can-call-direct?))
  (define a-lam (lam id e #f #f (make-hasheqv) 0 #f a-vehicle 0 #f #f #f #f #hasheq() #f))
  (set-vehicle-lams! a-vehicle (list a-lam))
  a-lam)

(define (lam-add-transitive-tail-apply! lam target-lam)
  (set-lam-transitive-tail-applies!
   lam
   (hash-set (lam-transitive-tail-applies lam) target-lam #t)))

(define (merge-vehicles! lambdas state orig-out)
  (define vehicles
    (for/fold ([vehicles #hash()]) ([lam (in-sorted-hash-values lambdas (compare symbol<? lam-id))]
                                    #:unless (lam-unused? lam))
      (define vehicle-lam (find! state lam))
      (define vehicle (lam-vehicle vehicle-lam))
      (define old-vehicle (lam-vehicle lam))
      (set-vehicle-max-jump-argc! vehicle (max (vehicle-max-jump-argc vehicle)
                                               (lam-max-jump-argc lam)))
      (unless (null? (lam-free-var-refs lam))
        (set-vehicle-closure?! vehicle #t))
      (unless (eq? vehicle old-vehicle)
        (define lams (vehicle-lams vehicle))
        (when (null? (cdr lams))
          (set-vehicle-id! vehicle (genid '__vehicle)))
        (set-lam-index! lam (length lams))
        (set-vehicle-lams! vehicle (cons lam lams))
        (set-vehicle-min-argc! vehicle (min (vehicle-min-argc vehicle)
                                            (vehicle-min-argc old-vehicle)))
        (set-vehicle-max-runstack-depth! vehicle (max (vehicle-max-runstack-depth vehicle)
                                                      (vehicle-max-runstack-depth old-vehicle)))
        (set-lam-vehicle! lam vehicle)
        (set-vehicle-closure?! vehicle #t))
      (hash-set vehicles vehicle (add1 (hash-ref vehicles vehicle 0)))))
  (fprintf orig-out "vehicles: ~a\n" (hash-count vehicles))
  (fprintf orig-out "max vehicle size ~a\n" (for/fold ([n 0]) ([m (in-hash-values vehicles)])
                                              (max n m)))
  ;; Fixpoint to determine tail-call behavior:
  (let loop ()
    (when (for*/fold ([changed? #f]) ([vehicle (in-hash-keys vehicles)]
                                      [lam (in-list (vehicle-lams vehicle))])
            (or (and (not (lam-can-tail-apply? lam))
                     (for/or ([other-lam (in-hash-keys (lam-transitive-tail-applies lam))])
                       (and (lam-can-tail-apply? other-lam)
                            (begin
                              (set-lam-can-tail-apply?! lam #t)
                              #t))))
                changed?))
      (loop)))
  ;; Reverse accumulated lams:
  (for/list ([vehicle (in-sorted-hash-keys vehicles (compare symbol<? vehicle-id))])
    (set-vehicle-lams! vehicle (reverse (vehicle-lams vehicle)))
    vehicle))