#lang racket/base
(require (only-in '#%linklet primitive->compiled-position)
         racket/match
         racket/set
         compiler/zo-structs
         "run.rkt"
         "name.rkt"
         "import.rkt")

(provide merge-linklets)

(define (merge-linklets runs names internals lifts imports)
  (define (syntax-literals-import? path/submod+phase)
    (eq? (cdr path/submod+phase) 'syntax-literals))

  ;; Pick an order for the remaining imports:
  (define import-keys (for/list ([path/submod+phase (in-hash-keys imports)]
                                 ;; References to a 'syntax-literals "phase" are
                                 ;; references to the implicit syntax-literals
                                 ;; module; drop those:
                                 #:unless (syntax-literals-import? path/submod+phase))
                        path/submod+phase))

  (define any-syntax-literals?
    (for/or ([path/submod+phase (in-hash-keys imports)])
      (syntax-literals-import? path/submod+phase)))
  (define syntax-literals-pos (and any-syntax-literals? 1))
  (define import-counter (if any-syntax-literals? 2 1))

  ;; Map each remaining import to its position
  (define ordered-importss
    (for/list ([key (in-list import-keys)])
      (define ordered-imports (hash-ref imports key))
      (for ([name (in-list ordered-imports)])
        (define i (hash-ref names (cons key name)))
        (set-import-pos! i import-counter)
        (set! import-counter (add1 import-counter)))
      ordered-imports))
  ;; Keep all the same import shapes
  (define import-shapess
    (for/list ([key (in-list import-keys)])
      (for/list ([name (in-list (hash-ref imports key))])
        (import-shape (hash-ref names (cons key name))))))

  ;; Map all syntax-literal references to the same import.
  ;; We'll update a call to the access to use a suitable
  ;; vector index.
  (for ([(path/submod+phase imports) (in-hash imports)]
        #:when (syntax-literals-import? path/submod+phase)
        [name (in-list imports)])
    (define i (hash-ref names (cons path/submod+phase name)))
    (set-import-pos! i syntax-literals-pos))

  ;; Map internals and lifts to positions
  (define positions
    (for/hash ([name (in-list (append internals lifts))]
               [i (in-naturals import-counter)])
      (values name i)))

  ;; For each linklet that we merge, make a mapping from
  ;; the linklet's old position to new names (which can
  ;; then be mapped to new positions):
  (define (make-position-mapping r)
    (define h (make-hasheqv))
    (define linkl (run-linkl r))
    (define importss (linkl-importss linkl))
    (define pos 1)
    (for ([imports (in-list importss)]
          [use (in-list (run-uses r))])
      (for ([name (in-list imports)])
        (hash-set! h pos (find-name names use name))
        (set! pos (add1 pos))))
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
    (for ([name (in-list (append (linkl-exports linkl)
                                 (linkl-internals linkl)
                                 (linkl-lifts linkl)))]
          [pos (in-naturals pos)])
      (hash-set! h pos (find-name names path/submod+phase name)))
    h)

  ;; Do we need the implicit initial variable for `(#%variable-reference)`?
  ;; The slot will be reserved whether we use it or not, but the
  ;; slot is not necessarily initialized if we don't need it.
  (define saw-zero-pos-toplevel? #f)

  (define body
    (apply
     append
     (for/list ([r (in-list runs)])
       (define pos-to-name/import (make-position-mapping r))
       (define (remap-pos pos)
         (cond
           [(zero? pos)
            ;; Implicit variable for `(#%variable-reference)` stays in place:
            (set! saw-zero-pos-toplevel? #t)
            0]
           [else
            (define new-name/import (hash-ref pos-to-name/import pos))
            (if (import? new-name/import)
                (import-pos new-name/import)
                (hash-ref positions new-name/import))]))
       (define graph (make-hasheq))
       (make-reader-graph
        (for/list ([b (in-list (linkl-body (run-linkl r)))])
          (let remap ([b b])
            (match b
              [(toplevel depth pos const? ready?)
               (define new-pos (remap-pos pos))
               (toplevel depth new-pos const? ready?)]
              [(def-values ids rhs)
               (def-values (map remap ids) (remap rhs))]
              [(inline-variant direct inline)
               (inline-variant (remap direct) (remap inline))]
              [(closure code gen-id)
               (cond
                 [(hash-ref graph gen-id #f)
                  => (lambda (ph) ph)]
                 [else
                  (define ph (make-placeholder #f))
                  (hash-set! graph gen-id ph)
                  (define cl (closure (remap code) gen-id))
                  (placeholder-set! ph cl)
                  cl])]
              [(let-one rhs body type unused?)
               (let-one (remap rhs) (remap body) type unused?)]
              [(let-void count boxes? body)
               (let-void count boxes? (remap body))]
              [(install-value count pos boxes? rhs body)
               (install-value count pos boxes? (remap rhs) (remap body))]
              [(let-rec procs body)
               (let-rec (map remap procs) (remap body))]
              [(boxenv pos body)
               (boxenv pos (remap body))]
              [(application rator rands)
               ;; Check for a `(.get-syntax-literal! '<pos>)` call
               (cond
                 [(and (toplevel? rator)
                       (let ([i (hash-ref pos-to-name/import (toplevel-pos rator))])
                         (and (import? i)
                              (eqv? syntax-literals-pos (import-pos i)))))
                  ;; This is a `(.get-syntax-literal! '<pos>)` call
                  (application (remap rator)
                               ;; FIXME: change the offset
                               rands)]
                 [else
                  ;; Any other application
                  (application (remap rator) (map remap rands))])]
              [(branch tst thn els)
               (branch (remap tst) (remap thn) (remap els))]
              [(with-cont-mark key val body)
               (with-cont-mark (remap key) (remap val) (remap body))]
              [(beg0 forms)
               (beg0 (map remap forms))]
              [(seq forms)
               (seq (map remap forms))]
              [(varref constant? toplevel dummy)
               (varref constant? (remap toplevel) (remap dummy))]
              [(assign id rhs undef-ok?)
               (assign (remap id) (remap rhs) undef-ok?)]
              [(apply-values proc args-expr)
               (apply-values (remap proc) (remap args-expr))]
              [(with-immed-mark key def-val body)
               (with-immed-mark (remap key) (remap def-val) (remap body))]
              [(case-lam name clauses)
               (case-lam name (map remap clauses))]
              [_
               (cond
                 [(lam? b)
                  (define tl-map (lam-toplevel-map b))
                  (define new-tl-map
                    (and tl-map
                         (for/set ([pos (in-set tl-map)])
                           (remap-pos pos))))
                  (struct-copy lam b
                               [body (remap (lam-body b))]
                               [toplevel-map new-tl-map])]
                 [else b])])))))))

  (define module-name 'demodularized)
  (define (primitive v)
    (primval (or (primitive->compiled-position v)
                 (error "cannot find primitive" v))))
  
  (define new-linkl
    (linkl module-name
           (list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                  '()
                  ordered-importss)
           (list* (if any-syntax-literals? (list (function-shape 1 #f)) '())
                  '()
                  import-shapess)
           '() ; exports
           internals
           lifts
           #hasheq()
           body
           (for/fold ([m 0]) ([r (in-list runs)])
             (max m (linkl-max-let-depth (run-linkl r))))
           saw-zero-pos-toplevel?))

  (define data-linkl
    (linkl 'data
           '((deserialize-module-path-indexes))
           '((#f))
           '(.mpi-vector)
           '()
           '()
           #hasheq()
           (list
            (def-values (list (toplevel 0 2 #f #f)) ; .mpi-vector
              (application (toplevel 2 1 #f #f) ; deserialize-module-path-indexes
                           (list
                            (list->vector
                             (cons
                              (box module-name)
                              (for/list ([path/submod+phase (in-list import-keys)])
                                (vector `(quote ,(car path/submod+phase))))))
                            (for/vector ([i (in-range (add1 (length import-keys)))])
                              i)))))
           16
           #f))

  (define decl-linkl
    (let ([deserialize-pos 1]
          [module-use-pos 2]
          [mpi-vector-pos 3]
          [exports-pos 4])
      (linkl 'decl
             '((deserialize
                module-use)
               (.mpi-vector))
             '((#f)
               (#f))
             '(self-mpi requires provides phase-to-link-modules)
             '()
             '()
             #hasheq()
             (list
              (def-values (list (toplevel 0 (+ exports-pos 0) #f #f)) ; .self-mpi
                (application (primitive vector-ref)
                             (list (toplevel 2 mpi-vector-pos #f #f)
                                   '0)))
              (def-values (list (toplevel 0 (+ exports-pos 1) #f #f)) ; requires
                (let ([arg-count 9])
                  (application (toplevel arg-count deserialize-pos #f #f)
                               (list
                                (toplevel arg-count mpi-vector-pos #f #f)
                                #f #f 0 '#() 0 '#() '#()
                                (list->vector
                                 (append
                                  `(#:cons #:list ,(add1 (length import-keys)) 0)
                                  (apply
                                   append
                                   (for/list ([k (in-list import-keys)]
                                              [i (in-naturals 1)])
                                     `(#:mpi ,i)))
                                  '(())))))))
              (def-values (list (toplevel 0 (+ exports-pos 2) #f #f)) ; provides
                (application (primitive hasheqv) null))
              (def-values (list (toplevel 0 (+ exports-pos 3) #f #f)) ; phase-to-link-modules
                (let ([depth 2])
                  (application (primitive hasheqv)
                               (list 0
                                     (let ([depth (+ depth (length import-keys))])
                                       (application (primitive list)
                                                    (for/list ([k (in-list import-keys)]
                                                               [i (in-naturals 1)])
                                                      (let ([depth (+ depth 2)])
                                                        (application (toplevel depth module-use-pos #f #f)
                                                                     (list
                                                                      (let ([depth (+ depth 2)])
                                                                        (application (primitive vector-ref)
                                                                                     (list
                                                                                      (toplevel depth mpi-vector-pos #f #f)
                                                                                      i)))
                                                                      '0)))))))))))
             (+ 32 (length import-keys))
             #f)))

  ;; By not including a 'stx-data linklet, we get a default
  ;; linklet that supplies #f for any syntax-literal reference.

  (linkl-bundle (hasheq 0 new-linkl
                        'data data-linkl
                        'decl decl-linkl)))
