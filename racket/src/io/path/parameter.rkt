#lang racket/base
(require "path.rkt"
         "check.rkt"
         "complete.rkt")

(provide current-directory
         current-directory-for-user)

;; Note: the publicly available versions of these functions are
;; wrapped to include security-guard checks.

(define/who current-directory
  (make-parameter (path #"/" (system-path-convention-type))
                  (lambda (v)
                    (check-directory-path who v))))

(define/who current-directory-for-user
  (make-parameter (current-directory)
                  (lambda (v)
                    (check-directory-path who v))))

(define (check-directory-path who v)
  (check who path-string? v)
  (path->complete-path v (current-directory)))
