#lang racket/base
(require "out.rkt"
         "id.rkt"
         "ref.rkt")

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
         runstack-branch-before!
         runstack-branch-other!
         runstack-branch-merge!)

(struct runstack (rs-state    ; shared state table
                  depth       ; curent stack depth
                  max-depth   ; max reached stack depth
                  sync-depth  ; depth that MZ_RUNSTACK currently has, `#f` if unknown
                  vars        ; list of pushed vars, newest first
                  var-depths  ; pushed var -> 'local or distance from stack start
                  need-inits  ; set of pushed vars that are not yet initialized
                  unsynced    ; pushed vars that haven't yet lived through a GC boundary
                  unsynced-refs) ; per-var refs that haven't yet lived through a GC boundary
  #:mutable)

(define (make-runstack state)
  (define rs-state (or (hash-ref state '#:runstack #f)
                       (let ([ht (make-hasheq)])
                         (hash-set! state '#:runstack ht)
                         ht)))
  (runstack rs-state 0 0 #f '() (make-hasheq) (make-hasheq) (make-hasheq) (make-hasheq)))

(define (runstack-push! rs id
                        #:referenced? [referenced? #t]
                        #:track-local? [track-local? #f])
  (set-runstack-vars! rs (cons id (runstack-vars rs)))
  (cond
    [(and track-local?
          referenced?
          (eq? 'local (hash-ref (runstack-rs-state rs) id #f)))
     ;; A previous pass determined that this variable will not
     ;; live across a GC boundary, so it can be stored in a C local.
     ;; Note that we're sharing a global table, even though an id
     ;; can have different extents due to closures; but only `let`
     ;; bindings are "tracked", and each of those is unique.
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

(define (runstack-pop! rs [n 1]
                       #:track-local? [track-local? #f])
  (define var-depths (runstack-var-depths rs))
  (let loop ([n n])
    (unless (zero? n)
      (define var (car (runstack-vars rs)))
      (unless (eq? 'local (hash-ref var-depths var #f))
        (set-runstack-depth! rs (- (runstack-depth rs) 1))
        (hash-remove! (runstack-need-inits rs) var)
        (when (hash-ref (runstack-unsynced rs) var #f)
          (when track-local?
            (hash-set! (runstack-rs-state rs) var 'local))
          (hash-remove! (runstack-unsynced rs) var))
        (let ([refs (hash-ref (runstack-unsynced-refs rs) var '())])
          (hash-remove! (runstack-unsynced-refs rs) var)
          (for ([ref (in-list refs)])
            (set-ref-last-use?! ref #f))))
      (set-runstack-vars! rs (cdr (runstack-vars rs)))
      (hash-remove! var-depths var)
      (loop (sub1 n)))))

(define (runstack-ref rs id #:assign? [assign? #f] #:ref [ref #f])
  (when ref
    ;; Remember the rest, so we can clear its `last-use?` if no sync
    ;; happends before the variable is popped
    (hash-set! (runstack-unsynced-refs rs) id
               (cons ref (hash-ref (runstack-unsynced-refs rs) id '()))))
  (cond
    [(eq? 'local (hash-ref (runstack-var-depths rs) id #f))
     (format "~a" (cify id))]
    [(and ref (ref-last-use? ref))
     (format "__last_use(__runbase, ~a)" (cify id))]
    [else
     (format "__runbase[~a]"  (cify id))]))

(define (runstack-assign rs id)
  (hash-remove! (runstack-need-inits rs) id)
  (runstack-ref rs id #:assign? #t))

(define (make-runstack-assign rs id)
  (lambda (s) (out "~a = ~a;" (runstack-assign rs id) s)))

(define (runstack-stack-ref rs)
  (format "(__runbase-~a)" (runstack-depth rs)))

(define (runstack-ref-pos rs id)
  (hash-ref (runstack-var-depths rs) id #f))

(define (runstack-sync! rs)
  (hash-clear! (runstack-unsynced rs))
  (hash-clear! (runstack-unsynced-refs rs))
  (define vars (sort (hash-keys (runstack-need-inits rs)) symbol<?))
  (for ([var (in-list vars)])
    (out "~a = __RUNSTACK_INIT_VAL;" (runstack-assign rs var)))
  (unless (eqv? (runstack-depth rs) (runstack-sync-depth rs))
    (out "*__runstack_ptr = ~a;" (runstack-stack-ref rs))
    (set-runstack-sync-depth! rs (runstack-depth rs))))

(define (runstack-synced! rs)
  (hash-clear! (runstack-need-inits rs))
  (hash-clear! (runstack-unsynced rs))
  (hash-clear! (runstack-unsynced-refs rs)))

(struct runstack-branch-state (need-inits sync-depth unsynced-refs))

(define (runstack-branch-before! rs)
  (define unsynced-refs (runstack-unsynced-refs rs))
  (set-runstack-unsynced-refs! rs (make-hasheq))
  (runstack-branch-state (hash-copy (runstack-need-inits rs))
                         (runstack-sync-depth rs)
                         unsynced-refs))

(define (runstack-branch-other! rs pre)
  (begin0
    (runstack-branch-state (hash-copy (runstack-need-inits rs))
                           (runstack-sync-depth rs)
                           (runstack-unsynced-refs rs))
    (set-runstack-need-inits! rs (runstack-branch-state-need-inits pre))
    (set-runstack-sync-depth! rs (runstack-branch-state-sync-depth pre))
    (set-runstack-unsynced-refs! rs (make-hasheq))))

(define (runstack-branch-merge! rs pre post)
  (for ([(k v) (in-hash (runstack-branch-state-need-inits post))])
    (hash-set! (runstack-need-inits rs) k v))
  (unless (eqv? (runstack-branch-state-sync-depth post) (runstack-sync-depth rs))
    (set-runstack-sync-depth! rs #f))
  (set-runstack-unsynced-refs! rs (union-unsynced-refs! (runstack-unsynced-refs rs)
                                                        (runstack-branch-state-unsynced-refs pre)
                                                        (runstack-branch-state-unsynced-refs post))))

(define union-unsynced-refs!
  (case-lambda
    [(a b c)
     (cond
       [((hash-count b) . > . (hash-count a))
        (union-unsynced-refs! b a c)]
       [((hash-count c) . > . (hash-count b))
        (union-unsynced-refs! a c b)]
       [else
        (union-unsynced-refs! a b)
        (union-unsynced-refs! a c)])]
    [(a b)
     (for ([(k l) (in-hash b)])
       (hash-set! a k (append l (hash-ref a k '()))))
     a]))

