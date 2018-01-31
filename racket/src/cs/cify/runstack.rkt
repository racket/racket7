#lang racket/base
(require "out.rkt"
         "id.rkt")

(provide make-runstack
         runstack-push!
         runstack-pop!
         runstack-ref
         runstack-assign
         make-runstack-assign
         runstack-stack-ref
         runstack-ref-pos
         runstack-sync!
         runstack-synced!
         runstack-max-depth
         runstack-branch-before
         runstack-branch-other!
         runstack-branch-merge!)

(struct runstack (depth max-depth sync-depth vars var-depths need-inits)
  #:mutable)

(define (make-runstack)
  (runstack 0 0 #f '() (make-hasheq) (make-hasheq)))

(define (runstack-push! rs id #:referenced? [referenced? #t])
  (define depth (add1 (runstack-depth rs)))
  (set-runstack-depth! rs depth)
  (set-runstack-max-depth! rs (max depth (runstack-max-depth rs)))
  (set-runstack-vars! rs (cons id (runstack-vars rs)))
  (hash-set! (runstack-need-inits rs) id depth)
  (hash-set! (runstack-var-depths rs) id depth)
  (out "~aconst int ~a = -~a;~a"
       (if referenced? "" "/* ")
       (cify id) depth
       (if referenced? "" " */")))

(define (runstack-pop! rs [n 1])
  (set-runstack-depth! rs (- (runstack-depth rs) n))
  (let loop ([n n])
    (unless (zero? n)
      (define var (car (runstack-vars rs)))
      (hash-remove! (runstack-need-inits rs) var)
      (set-runstack-vars! rs (cdr (runstack-vars rs)))
      (loop (sub1 n)))))

(define (runstack-ref rs id)
  (format "__runbase[~a]" (cify id)))

(define (runstack-assign rs id)
  (hash-remove! (runstack-var-depths rs) id)
  (hash-remove! (runstack-need-inits rs) id)
  (runstack-ref rs id))

(define (make-runstack-assign rs id)
  (lambda (s) (out "~a = ~a;" (runstack-assign rs id) s)))

(define (runstack-stack-ref rs)
  (format "(__runbase-~a)" (runstack-depth rs)))

(define (runstack-ref-pos rs id)
  (hash-ref (runstack-var-depths rs) id #f))

(define (runstack-sync! rs)
  (define vars (hash-keys (runstack-need-inits rs)))
  (for ([var (in-list vars)])
    (out "~a = __RUNSTACK_INIT_VAL;" (runstack-assign rs var)))
  (unless (eqv? (runstack-depth rs) (runstack-sync-depth rs))
    (out "*__runstack_ptr = ~a;" (runstack-stack-ref rs))
    (set-runstack-sync-depth! rs (runstack-depth rs))))

(define (runstack-synced! rs)
  (hash-clear! (runstack-need-inits rs)))

(struct runstack-branch-state (need-inits sync-depth))

(define (runstack-branch-before rs)
  (runstack-branch-state (hash-copy (runstack-need-inits rs))
                         (runstack-sync-depth rs)))

(define (runstack-branch-other! rs pre)
  (begin0
    (runstack-branch-state (hash-copy (runstack-need-inits rs))
                           (runstack-sync-depth rs))
    (set-runstack-need-inits! rs (runstack-branch-state-need-inits pre))
    (set-runstack-sync-depth! rs (runstack-branch-state-sync-depth pre))))

(define (runstack-branch-merge! rs pre post)
  (for ([(k v) (in-hash (runstack-branch-state-need-inits post))])
    (hash-set! (runstack-need-inits rs) k v))
  (unless (eqv? (runstack-branch-state-sync-depth post) (runstack-sync-depth rs))
    (set-runstack-sync-depth! rs #f)))

