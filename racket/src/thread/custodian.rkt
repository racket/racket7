#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         (only-in '#%foreign make-stubborn-will-executor))

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

         custodian-shut-down?
         unsafe-make-custodian-at-root
         unsafe-custodian-register
         unsafe-custodian-unregister
         raise-custodian-is-shut-down)

(struct custodian (children     ; weakly maps maps object to callback
                   [shut-down? #:mutable]
                   [parent-reference #:mutable])
  #:authentic)

(struct custodian-box ([v #:mutable])
  #:authentic)

(struct willed-callback (proc will)
  #:property prop:procedure (struct-field-index proc)
  #:authentic)

(struct at-exit-callback willed-callback ()
  #:authentic)

;; Reporting registration in a custodian through this indirection
;; enables GCing custodians that aren't directly referenced, merging
;; the managed objects into the parent
(struct custodian-reference (c)
  #:authentic)

(define (create-custodian)
  (custodian (make-weak-hasheq)
             #f
             #f))
  
(define root-custodian (create-custodian))

(define/who current-custodian
  (make-parameter root-custodian
                  (lambda (v)
                    (check who custodian? v)
                    v)))

(define/who (make-custodian [parent (current-custodian)])
  (check who custodian? parent)
  (define c (create-custodian))
  (define cref (unsafe-custodian-register parent c custodian-shutdown-all #f #t))
  (set-custodian-parent-reference! c cref)
  (unless cref (raise-custodian-is-shut-down who parent))
  c)

(define (unsafe-make-custodian-at-root)
  (make-custodian root-custodian))

;; The given `callback` will be run in atomic mode.
;; Unless `weak?` is true, the given `obj` is registered with an ordered
;; finalizer, so don't supply an `obj` that is exposed to safe code
;; that might see `obj` after finalization through a weak reference
;; (and detect that `obj` is thereafter retained strongly).
(define (unsafe-custodian-register cust obj callback at-exit? weak?)
  (atomically
   (cond
     [(custodian-shut-down? cust) #f]
     [else
      (define we (and (not weak?)
                      (make-stubborn-will-executor)))
      (hash-set! (custodian-children cust)
                 obj
                 (cond
                   [weak? callback]
                   [at-exit? (at-exit-callback callback we)]
                   [else (willed-callback callback we)]))
      (when we
        ;; Registering with a will executor that we never poll has the
        ;; effect of turning a weak reference into a strong one when
        ;; there are no other references:
        (will-register we obj void))
      (custodian-reference cust)])))

(define (unsafe-custodian-unregister obj cref)
  (when cref
    (atomically
     (define c (custodian-reference-c cref))
     (unless (custodian-shut-down? c)
       (define cb (hash-ref (custodian-children c) obj #f))
       (hash-remove! (custodian-children c) obj)))))

(define/who (custodian-shutdown-all c)
  (check who custodian? c)
  (atomically
   (unless (custodian-shut-down? c)
     (set-custodian-shut-down?! c #t)
     (for ([(child callback) (in-hash (custodian-children c))])
       (callback child))
     (hash-clear! (custodian-children c)))))

(define (subordinate? c super-c)
  (let loop ([p (custodian-reference-c (custodian-parent-reference c))])
    (cond
      [(eq? p super-c) #t]
      [(not p) #f]
      [else (loop (custodian-reference-c (custodian-parent-reference p)))])))

(define/who (custodian-managed-list c super-c)
  (check who custodian? c)
  (check who custodian? super-c)
  (unless (subordinate? c super-c)
    (raise-arguments-error who "the second custodian does not manage the first custodian"
                           "first custodian" c
                           "second custodian" super-c))
  (hash-keys (custodian-children c)))

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

(define/who (make-custodian-box c v)
  (check who custodian? c)
  (define b (custodian-box v))
  (unless (unsafe-custodian-register c b (lambda (b) (set-custodian-box-v! b #f)) #f #t)
    (raise-custodian-is-shut-down who c))
  b)
  
(define/who (custodian-box-value cb)
  (check who custodian-box? cb)
  (custodian-box-v cb))

;; ----------------------------------------

(define (raise-custodian-is-shut-down who c)
  (raise-arguments-error who "the custodian has been shut down"
                         "custodian" c))
