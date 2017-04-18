#lang racket/base

(provide atomically
         current-atomic
         
         end-atomic)

(define atomic 0)
(define current-atomic
  (case-lambda
    [() atomic]
    [(v) (set! atomic v)]))

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
