#lang racket/base
(require compiler/cm)

;; This module is loaded via `setup/main` with a `--boot` argument
;; that selects this module and sets the compile-file root directory
;; to be within the build directory. The first argument should be a
;; module to load, and the rest of the arguments are delivered to that
;; module.
;;
;; The point of going through `setup/main` is that the Racket module
;; gets compiled as needed, so that it doesn't have to be loaded from
;; source every time. At the same time `setup/main` detects when files
;; need to be recompiled, either becuase the underlying Racket's
;; version changed or because a dependency changed.

(provide go)

(define (go orig-compile-file-paths)
  (define mod-file (vector-ref (current-command-line-arguments) 3))
  (parameterize ([current-command-line-arguments
                  ;; Discard --boot, this mod, compiled-file dir, and name of file to load:
                  (list->vector (list-tail (vector->list (current-command-line-arguments)) 4))])
    ;; In case multiple xforms run in parallel, use a lock file so
    ;;  that only one is building.
    (define lock-file (build-path (car (current-compiled-file-roots)) "SETUP-LOCK"))
    (define lock-port (open-output-file #:exists 'truncate/replace lock-file))
    (let loop ([n 0])
      (when (= n 3)
        (printf "Waiting on lock: ~a" lock-file)
        (unless (port-try-file-lock? lock-port 'exclusive)
          (sleep 0.1)
          (loop (add1 n)))))

    (dynamic-wind
     void
     (lambda ()
       ;; Load the requested module, but don't instantiate:
       (dynamic-require mod-file (void)))
     (lambda ()
       (port-file-unlock lock-port)))

    ;; Now that the lock is released, instantiate:
    (dynamic-require mod-file #f)))
