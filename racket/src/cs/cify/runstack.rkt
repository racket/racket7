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

(struct runstack (rs-state    ; shared state table
                  depth       ; curent stack depth
                  max-depth   ; max reached stack depth
                  sync-depth  ; depth that MZ_RUNSTACK currently has, `#f` if unknown
                  vars        ; list of pushed vars, newest first
                  var-depths  ; pushed var -> 'local or distance from stack start
                  need-inits  ; set of pushed vars that are not yet initialized
                  unsynced)   ; set of pushed vars that haven't yet lived through a GC boundary
  #:mutable)

(define (make-runstack state)
  (define rs-state (or (hash-ref state '#:runstack #f)
                       (let ([ht (make-hasheq)])
                         (hash-set! state '#:runstack ht)
                         ht)))
  (runstack rs-state 0 0 #f '() (make-hasheq) (make-hasheq) (make-hasheq)))

(define (runstack-push! rs id
                        #:referenced? [referenced? #t]
                        #:track-local? [track-local? #f])
  (set-runstack-vars! rs (cons id (runstack-vars rs)))
  (cond
    [(and track-local?
          referenced?
          (eq? 'local (hash-ref (runstack-rs-state rs) id #f)))
     ;; A previous pass determined that this variable will not
     ;; live across a GC boundary, so it can be stored in a C local
     (hash-set! (runstack-var-depths rs) id 'local)
     (out "Scheme_Object *~a;" (cify id))]
    [else
     (define depth (add1 (runstack-depth rs)))
     (set-runstack-depth! rs depth)
     (set-runstack-max-depth! rs (max depth (runstack-max-depth rs)))
     (hash-set! (runstack-var-depths rs) id depth)
     (hash-set! (runstack-need-inits rs) id #t)
     (hash-set! (runstack-unsynced rs) id #t)
     (out "~aconst int ~a = -~a;~a"
          (if referenced? "" "/* ")
          (cify id) depth
          (if referenced? "" " */"))]))

(define (runstack-pop! rs [n 1])
  (define var-depths (runstack-var-depths rs))
  (let loop ([n n])
    (unless (zero? n)
      (define var (car (runstack-vars rs)))
      (unless (eq? 'local (hash-ref var-depths var #f))
        (set-runstack-depth! rs (- (runstack-depth rs) 1))
        (hash-remove! (runstack-need-inits rs) var)
        (when (hash-ref (runstack-unsynced rs) var #f)
          (hash-set! (runstack-rs-state rs) var 'local)
          (hash-remove! (runstack-unsynced rs) var)))
      (set-runstack-vars! rs (cdr (runstack-vars rs)))
      (hash-remove! var-depths var)
      (loop (sub1 n)))))

(define (runstack-ref rs id #:last-use? [last-use? #f])
  (cond
    [(eq? 'local (hash-ref (runstack-var-depths rs) id #f))
     (format "~a" (cify id))]
    [last-use?
     (format "__last_use(__runbase, ~a)" (cify id))]
    [else
     (format "__runbase[~a]" (cify id))]))

(define (runstack-assign rs id)
  (hash-remove! (runstack-need-inits rs) id)
  (runstack-ref rs id))

(define (make-runstack-assign rs id)
  (lambda (s) (out "~a = ~a;" (runstack-assign rs id) s)))

(define (runstack-stack-ref rs)
  (format "(__runbase-~a)" (runstack-depth rs)))

(define (runstack-ref-pos rs id)
  (hash-ref (runstack-var-depths rs) id #f))

(define (runstack-sync! rs)
  (hash-clear! (runstack-unsynced rs))
  (define vars (sort (hash-keys (runstack-need-inits rs)) symbol<?))
  (for ([var (in-list vars)])
    (out "~a = __RUNSTACK_INIT_VAL;" (runstack-assign rs var)))
  (unless (eqv? (runstack-depth rs) (runstack-sync-depth rs))
    (out "*__runstack_ptr = ~a;" (runstack-stack-ref rs))
    (set-runstack-sync-depth! rs (runstack-depth rs))))

(define (runstack-synced! rs)
  (hash-clear! (runstack-need-inits rs))
  (hash-clear! (runstack-unsynced rs)))

(struct runstack-branch-state (need-inits sync-depth unsynced))

(define (runstack-branch-before rs)
  (runstack-branch-state (hash-copy (runstack-need-inits rs))
                         (runstack-sync-depth rs)
                         (hash-copy (runstack-unsynced rs))))

(define (runstack-branch-other! rs pre)
  (begin0
    (runstack-branch-state (hash-copy (runstack-need-inits rs))
                           (runstack-sync-depth rs)
                           (hash-copy (runstack-unsynced rs)))
    (set-runstack-need-inits! rs (runstack-branch-state-need-inits pre))
    (set-runstack-sync-depth! rs (runstack-branch-state-sync-depth pre))
    (set-runstack-unsynced! rs (runstack-branch-state-unsynced pre))))

(define (runstack-branch-merge! rs pre post)
  (for ([(k v) (in-hash (runstack-branch-state-need-inits post))])
    (hash-set! (runstack-need-inits rs) k v))
  (unless (eqv? (runstack-branch-state-sync-depth post) (runstack-sync-depth rs))
    (set-runstack-sync-depth! rs #f))
  (define unsynced (runstack-unsynced rs))
  (define post-unsynced (runstack-branch-state-unsynced post))
  (for ([var (in-list (hash-keys unsynced))])
    (unless (hash-ref post-unsynced var #f)
      (hash-remove! unsynced var))))
