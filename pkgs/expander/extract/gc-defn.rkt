#lang racket/base
(require racket/list
         "../host/correlate.rkt"
         "../common/set.rkt"
         "../compile/side-effect.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "symbol.rkt"
         "utils.rkt")

(provide garbage-collect-definitions)

(define (garbage-collect-definitions linklet-expr)
  (log-status "Removing unused definitions...")

  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define used-syms (make-hasheq))

  ;; maps known definitions to one of:
  ;; #s(def) -- all we know is that it's defined
  ;; #s(lam n pure?) -- function of arity n, may be pure (b/c constructor), pure must return 1 val
  ;; #s(property) -- struct property with no guard
  ;; #s(struct-ty n) -- struct type with n fields
  (define seen-defns (make-hasheq))
  (hash-set! seen-defns 'struct:exn:fail (struct-op 'struct-type 2))
  (hash-set! seen-defns 'make-thread-cell (struct-op 'constructor 1))
  (hash-set! seen-defns 'make-continuation-prompt-tag (struct-op 'constructor 1))
  (hash-set! seen-defns 'make-weak-hash (struct-op 'constructor 0))
  (hash-set! seen-defns 'gensym (struct-op 'constructor 0))
  (hash-set! seen-defns 'string (struct-op 'constructor 2))
  (hash-set! seen-defns 'cons (struct-op 'constructor 2))


  ;; Map symbols to definition right-hand sides
  (define sym-to-rhs (make-hasheq))
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (for ([sym (in-list (defn-syms e))])
        (hash-set! sym-to-rhs sym (defn-rhs e)))]))

  ;; A "mark"-like traversal of an expression:
  (define (set-all-used! init-sym e)
    (for ([sym (in-set (all-used-symbols e))])
      (unless (hash-ref used-syms sym #f)
        (hash-set! used-syms sym #t)
        (set-all-used! sym (hash-ref sym-to-rhs sym #f)))))

  (define (ref? s) (hash-ref seen-defns s (lambda () #f)))

  (define (safe-defn? e)
    (and (defn? e)
         (or (not (any-side-effects? (defn-rhs e) (length (defn-syms e)) ref?
                                     #:known-defns seen-defns)))))

  ;; Mark each body form, delaying the righthand side of definitions
  ;; if the definition has no side-effect
  (let loop ([body body])
    (cond [(null? body) (void)]
          [(defn? (car body))
           (for* ([d (in-list body)]
                    #:break (not (safe-defn? d))
                    [s (in-list (defn-syms d))])
             (unless (hash-ref seen-defns s #f)
               (hash-set! seen-defns s #s(def))))
           (define e (car body))
           (define s
             (cond [(any-side-effects? (defn-rhs e) (length (defn-syms e)) ref? #:known-defns seen-defns)
                    (for ([sym (in-list (defn-syms e))])
                      (unless (hash-ref used-syms sym #f)
                        (hash-set! used-syms sym #t)))
                    (set-all-used! (defn-syms e) (defn-rhs e))]
                   [else (seteq)]))
           (add-defn-types! seen-defns (defn-syms e) (defn-rhs e))
           (loop (cdr body))]
          [else
           (set-all-used! '<toplevel> (car body))
           (loop (cdr body))]))

  ;; Mark each export:
  (for ([ex+sym (in-list (bootstrap:s-expr-linklet-exports+locals linklet-expr))])
    (set-all-used! '<export> (cdr ex+sym)))

  (define can-remove-count
    (for/sum ([e (in-list body)])
      (cond
       [(defn? e)
        (if (for/or ([sym (in-list (defn-syms e))])
              (hash-ref used-syms sym #f))
            0
            (length (defn-syms e)))]
       [else 0])))
  (log-status "Can remove ~s of ~s defined names, keeping ~s"
              can-remove-count
              (hash-count sym-to-rhs)
              (- (hash-count sym-to-rhs) can-remove-count))

  (define new-body
    (for/list ([e (in-list body)]
               #:when (or (not (defn? e))
                          (for/or ([sym (in-list (defn-syms e))])
                            (hash-ref used-syms sym #f))))
      e))

  (append (take linklet-expr 3)
          new-body))
