#lang racket/base
(require "../common/check.rkt")

(provide make-security-guard
         security-guard?
         current-security-guard)

(struct security-guard (parent
                        file-guard
                        network-guard
                        link-guard))

(define root-security-guard
  (security-guard #f void void void))

(define current-security-guard
  (make-parameter root-security-guard
                  (lambda (v)
                    (check 'guard-for-current-security-guard
                           security-guard?
                           v)
                    v)))

(define (make-security-guard parent
                             file-guard
                             network-guard
                             [link-guard #f])
  (check 'make-security-guard security-guard? parent)
  (check 'make-security-guard (lambda (p) (and (procedure? file-guard)
                                               (procedure-arity-includes? file-guard 3)))
         #:contract "(procedure-arity-includes/c 4)"
         file-guard)
  (check 'make-security-guard (lambda (p) (and (procedure? network-guard)
                                               (procedure-arity-includes? network-guard 4)))
         #:contract "(procedure-arity-includes/c 4)"
         network-guard)
  (check 'make-security-guard (lambda (p) (or (not p)
                                              (and (procedure? link-guard)
                                                   (procedure-arity-includes? link-guard 3))))
         #:contract "(or/c #f (procedure-arity-includes/c 3))"
         link-guard)
  (security-guard parent file-guard network-guard link-guard))
