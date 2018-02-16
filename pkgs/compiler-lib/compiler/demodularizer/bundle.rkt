#lang racket/base
(require (only-in '#%linklet primitive->compiled-position)
         compiler/zo-structs
         "run.rkt"
         "name.rkt")

(provide wrap-bundle)

(define (wrap-bundle body internals lifts get-merge-info)
  (define-values (runs
                  import-keys
                  ordered-importss
                  import-shapess
                  any-syntax-literals?
                  any-transformer-registers?
                  saw-zero-pos-toplevel?)
    (get-merge-info))

  (define module-name 'demodularized)
  (define (primitive v)
    (primval (or (primitive->compiled-position v)
                 (error "cannot find primitive" v))))
  
  (define new-linkl
    (linkl module-name
           (list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                  (if any-transformer-registers? '(.set-transformer!) '())
                  ordered-importss)
           (list* (if any-syntax-literals? (list (function-shape 1 #f)) '())
                  (if any-transformer-registers? (list (function-shape 2 #f)) '())
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
