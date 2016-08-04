#lang racket/base
(require "../eval/dynamic-require.rkt"
         "../namespace/namespace.rkt"
         "reader-syntax-to-syntax.rkt")

(provide dynamic-require-reader)

(define (dynamic-require-reader mod-path sym [fail-thunk default-dynamic-require-fail-thunk])
  (define root-ns (namespace-root-namespace (current-namespace)))
  (define proc (if root-ns
                   ;; Switch to the root namespace:
                   (parameterize ([current-namespace root-ns])
                     (dynamic-require mod-path sym fail-thunk))
                   ;; Current namespace is a root namespace:
                   (dynamic-require mod-path sym fail-thunk)))
  (cond
   [(and (eq? sym 'read-syntax)
         (procedure? proc)
         (procedure-arity-includes? proc 6))
    ;; Need to convert syntax object for module name
    (lambda (name input mod-s line column position)
      ;; We don't try to convert the result; if it's syntax, it will get
      ;; wrapped again as a reader syntax object by the reader, but that
      ;; extra wrapper is harmlessly removed by our `read-syntax`
      (proc name input (reader-syntax->syntax mod-s) line column position))]
   [else proc]))
