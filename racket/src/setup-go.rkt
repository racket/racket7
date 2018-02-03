#lang racket/base

;; This module is loaded via `setup/main` with a `--boot` argument
;; that select this module and sets the compile-file root directory to
;; be within the build directory. The first argument should be a
;; module to load, and the rest of the arguments are delivered
;; to that module.

(provide go)

(define (go orig-compile-file-paths)
  (define mod-file (vector-ref (current-command-line-arguments) 3))
  (parameterize ([current-command-line-arguments
                  ;; Discard --boot, this mod, compiled-file dir, and name of file to load:
                  (list->vector (list-tail (vector->list (current-command-line-arguments)) 4))])
    ;; In case multiple xforms run in parallel, use a lock file
    ;;  so that only one is building.
    (let retry ()
      (let ([lock-file (build-path (car (current-compiled-file-roots)) "XFORM-LOCK")])
        ((call-with-escape-continuation
          (lambda (escape)
            (parameterize ([uncaught-exception-handler
                            (lambda (exn)
                              (escape
                               (lambda ()
                                 (if (exn:fail:filesystem:exists? exn)
                                     (begin
                                       (printf "Lock file exists: ~a\n"
                                               (path->complete-path lock-file))
                                       (printf " (If this isn't a parallel make, then delete it.)\n")
                                       (printf " Waiting until the lock file disappears...\n")
                                       (let loop ()
                                         (flush-output)
                                         (sleep 0.1)
                                         (if (file-exists? lock-file)
                                             (loop)
                                             (printf " ... continuing\n")))
                                       (retry))
                                     (raise exn)))))])
              (dynamic-wind
               (lambda ()
                 (close-output-port (open-output-file lock-file #:exists 'error)))
               (lambda ()
                 ;; Make sure `compiler/cm` gets built, so that we don't
                 ;; have to bootstrap again if we did this time:
                 (dynamic-require 'compiler/cm #f)
                 ;; Load the requested module:
                 (dynamic-require mod-file #f))
               (lambda ()
                 (delete-file lock-file))))
            void)))))))
