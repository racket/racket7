#lang racket/base
(require syntax/modresolve)

(provide module-path-index->path)

(define (module-path-index->path req path submod)
  (define mpi
    (let loop ([req req])
      (define-values (mod-path base) (module-path-index-split req))
      (cond
        [(not mod-path) (if (null? submod)
                            (module-path-index-join #f #f)
                            (module-path-index-join `(submod "." ,@submod)
                                                    (module-path-index-join #f #f)))]
        [else
         (module-path-index-join mod-path
                                 (and base (loop base)))])))
  (define p (resolve-module-path-index mpi path))

  ;; Make sure a path name is normalized
  (define p-path (if (pair? p) (cadr p) p))
  (define p-submod (if (pair? p) (cddr p) '()))
  (define p-simple-path (if (path? p-path)
                            (normal-case-path (simplify-path p-path))
                            p-path))

  ;; Combine path back with submod
  (if (null? p-submod)
      p-simple-path
      (cons p-simple-path p-submod)))
