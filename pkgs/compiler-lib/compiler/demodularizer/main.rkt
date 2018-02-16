#lang racket/base
(require compiler/cm
         "find.rkt"
         "name.rkt"
         "merge.rkt"
         "write.rkt")

(provide current-excluded-modules
         garbage-collect-toplevels-enabled
         recompile-enabled
         demodularize)

(define current-excluded-modules (make-parameter null))
(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize input-file [given-output-file #f])
  (parameterize ([current-logger logger])

    (log-info "Compiling module")
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (managed-compile-zo input-file))

    (log-info "Finding modules")
    (define runs (find-modules input-file))

    (log-info "Selecting names")
    (define-values (names internals lifts imports) (select-names runs))

    (log-info "Merging linklets")
    (define bundle (merge-linklets runs names internals lifts imports))

    (log-info "Writing bytecode")
    (define output-file (or given-output-file
                            (path-add-suffix input-file #"_merged.zo")))
    (write-module output-file bundle)))
