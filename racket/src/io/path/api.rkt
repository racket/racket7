#lang racket/base
(require "parameter.rkt"
         (rename-in "complete.rkt"
                    [path->complete-path raw:path->complete-path])
         "path.rkt")

(provide path->complete-path
         current-drive)

(define path->complete-path
  (case-lambda
    [(p) (raw:path->complete-path p (current-directory) #:wrt-given? #f)]
    [(p wrt) (raw:path->complete-path p wrt #:wrt-given? #t)]))

(define (current-drive)
  (if (eq? (system-path-convention-type) 'unix)
      (string->path "/")
      (error 'current-drive "not yet ready")))

