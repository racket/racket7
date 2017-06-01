#lang racket/base
(require "../error/abort.rkt"
         "path.rkt"
         "check.rkt"
         "check-path.rkt"
         "relativity.rkt"
         "build.rkt")

(provide path->complete-path)

(define/who (path->complete-path p-in wrt #:wrt-given? [wrt-given? #t])
  (check-path-argument who p-in)
  (when wrt-given?
    (check who (lambda (p) (and (or (path-string? p) (path-for-some-system? p))
                                (complete-path? p)))
           #:contract "(and/c (or/c path-string? path-for-some-system?) complete-path?)"
           wrt))
  (unless (eq? (convention-of-path p-in)
               (convention-of-path wrt))
    (if wrt-given?
        (raise-arguments-error who
                               "convention of first path incompatible with convention of second path"
                               "first path" p-in
                               "second path" wrt)
        (raise-arguments-error who
                               "no second path supplied, and given path is not for the current platform"
                               "given path" p-in)))
  (define p (->path p-in))
  (cond
   [(complete-path? p) p]
   [(relative-path? p)
    (build-path wrt p)]
   [else
    ;; FIXME
    (abort "non-complete absolute path on Windows")]))

(define (convention-of-path p)
  (if (path? p)
      (path-convention p)
      (system-path-convention-type)))
