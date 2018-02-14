#lang racket/base
(require syntax/modresolve
         (prefix-in expander: expander/common/module-path))

(provide module-path-index->path)

(define (module-path-index->path req path submod)
  (define mpi
    (let loop ([req req])
      (define-values (mod-path base) (expander:module-path-index-split req))
      (cond
        [(not mod-path) (if (null? submod)
                            (module-path-index-join #f #f)
                            (module-path-index-join `(submod "." ,@submod)
                                                    (module-path-index-join #f #f)))]
        [else
         (module-path-index-join mod-path
                                 (and base (loop base)))])))
  (resolve-module-path-index mpi path))
