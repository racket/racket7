#lang racket/base
(require "engine.rkt")

(provide atomically
         current-atomic
         
         end-atomic)

(define current-atomic (make-global-parameter 0))

(define-syntax-rule (atomically expr ...)
  (begin
    (start-atomic)
    (begin0
     (let () expr ...)
     (end-atomic))))

(define (start-atomic)
  (current-atomic (add1 (current-atomic))))

(define (end-atomic)
  (current-atomic (sub1 (current-atomic))))
