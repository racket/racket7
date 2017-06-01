#lang racket/base
(require "../common/check.rkt")

(provide make-security-guard
         security-guard?
         current-security-guard

         security-guard-check-file
         security-guard-check-file-link
         security-guard-check-network)

(struct security-guard (parent
                        file-guard
                        network-guard
                        link-guard))

(define root-security-guard
  (security-guard #f void void void))

(define/who current-security-guard
  (make-parameter root-security-guard
                  (lambda (v)
                    (check who security-guard? v)
                    v)))

(define/who (make-security-guard parent
                                 file-guard
                                 network-guard
                                 [link-guard #f])
  (check who security-guard? parent)
  (check who (procedure-arity-includes/c 3) file-guard)
  (check who (procedure-arity-includes/c 4) network-guard)
  (check who #:or-false (procedure-arity-includes/c 3) link-guard)
  (security-guard parent file-guard network-guard link-guard))

(define (security-guard-check-file who path guards)
  (void))

(define (security-guard-check-file-link who path dest)
  (void))

(define (security-guard-check-network who host port client?)
  (void))

