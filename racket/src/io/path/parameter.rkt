#lang racket/base
(require "path.rkt"
         "check.rkt")

(provide current-directory)

(define current-directory
  (make-parameter (path #"/" (system-path-convention-type))
                  (lambda (v)
                    (check 'current-directory path-string? v)
                    (path->complete-path v))))
