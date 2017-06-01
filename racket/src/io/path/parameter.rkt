#lang racket/base
(require "path.rkt"
         "check.rkt"
         "complete.rkt")

(provide current-directory)

(define/who current-directory
  (make-parameter (path #"/" (system-path-convention-type))
                  (lambda (v)
                    (check who path-string? v)
                    (path->complete-path v (current-directory)))))
