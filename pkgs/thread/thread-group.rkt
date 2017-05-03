#lang racket/base
(require "engine.rkt"
         "check.rkt"
         "internal-error.rkt"
         "atomic.rkt")

(provide thread-group?
         make-thread-group
         current-thread-group

         ;; Used by scheduler
         root-thread-group
         thread-group-next!
         
         ;; Called by thread creation and termination:
         thread-group-add!
         thread-group-remove!
         
         thread-group-all-threads
         num-threads-in-groups)

(struct thread-group (parent
                      children ; maps children to nodes
                      [chain-start #:mutable] ; all children
                      [chain #:mutable] ; children remaining to be scheduled round-robin
                      [chain-end #:mutable])
        #:transparent)

(struct node (child
              [prev #:mutable]
              [next #:mutable])
        #:transparent)

(define root-thread-group (thread-group #f (make-hasheq) #f #f #f))

(define num-threads-in-groups 0)

(define current-thread-group
  (make-parameter root-thread-group
                  (lambda (v)
                    (check 'current-thread-group thread-group? v)
                    v)))

(define (make-thread-group [parent (current-thread-group)])
  (check 'make-thread-group thread-group? parent)
  (define tg (thread-group parent (make-hasheq) #f #f #f))
  (thread-group-add! parent tg)
  tg)

;; Called atomically in scheduler:
(define (thread-group-next! tg)
  (define n (thread-group-chain tg))
  (cond
   [(not n)
    (define n (thread-group-chain-start tg))
    (cond
     [(not n)
      ;; No children
      #f]
     [else
      (set-thread-group-chain! tg (node-next n))
      (node-child n)])]
   [else
    (set-thread-group-chain! tg (node-next n))
    (node-child n)]))

(define (thread-group-add! parent child)
  (atomically
   (when (hash-ref (thread-group-children parent) child #f)
     (internal-error "adding a thread that is already added"))
   (define t (thread-group-chain-end parent))
   (define n (node child t #f))
   (if t
       (set-node-next! t n)
       (set-thread-group-chain-start! parent n))
   (set-thread-group-chain-end! parent n)
   (hash-set! (thread-group-children parent) child n)
   (unless (thread-group? child)
     (set! num-threads-in-groups (add1 num-threads-in-groups)))))

(define (thread-group-remove! parent child)
  (atomically
   (define children (thread-group-children parent))
   (define n (hash-ref children child))
   (hash-remove! children child)
   (if (node-next n)
       (set-node-prev! (node-next n) (node-prev n))
       (set-thread-group-chain-end! parent (node-prev n)))
   (if (node-prev n)
       (set-node-next! (node-prev n) (node-next n))
       (set-thread-group-chain-start! parent (node-next n)))
   (when (eq? n (thread-group-chain parent))
     (set-thread-group-chain! parent (node-next n)))
   (unless (thread-group? child)
     (set! num-threads-in-groups (sub1 num-threads-in-groups)))))

(define (thread-group-all-threads parent accum)
  (cond
   [(not (thread-group? parent)) (cons parent accum)]
   [else
    (let loop ([n (thread-group-chain-start parent)] [accum accum])
      (cond
       [(not n) accum]
       [else (loop (node-next n)
                   (thread-group-all-threads (node-child n) accum))]))]))

