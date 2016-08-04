#lang racket/base
(require racket/list
         "../common/set.rkt"
         "../compile/side-effect.rkt"
         "../run/status.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "symbol.rkt")

(provide garbage-collect-definitions)

(define (garbage-collect-definitions linklet-expr)
  (log-status "Removing unused definitions...")
    
  (define body (bootstrap:s-expr-linklet-body linklet-expr))

  (define used-syms (make-hasheq))

  ;; Map symbols to definition right-hand sides
  (define sym-to-rhs (make-hasheq))
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (for ([sym (in-list (defn-syms e))])
        (hash-set! sym-to-rhs sym (defn-rhs e)))]))
  
  ;; A "mark"-like traversal of an expression:
  (define (set-all-used! e)
    (for ([sym (in-set (all-used-symbols e))])
      (unless (hash-ref used-syms sym #f)
        (hash-set! used-syms sym #t)
        (set-all-used! (hash-ref sym-to-rhs sym #f)))))
  
  ;; Mark each body form, delaying the righthand side of definitions
  ;; if the definition has no side-effect
  (for ([e (in-list body)])
    (cond
     [(defn? e)
      (if (any-side-effects? (defn-rhs e) (length (defn-syms e)) (lambda (s) #f))
          (set-all-used! (defn-rhs e))
          (seteq))]
     [else (set-all-used! e)]))
  
  ;; Mark each export:
  (for ([ex+sym (in-list (bootstrap:s-expr-linklet-exports+locals linklet-expr))])
    (set-all-used! (cdr ex+sym)))
  
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

;; ----------------------------------------
  
(define (defn? e)
  (and (pair? e)
       (eq? (car e) 'define-values)))

(define defn-syms cadr)
(define defn-rhs caddr)
