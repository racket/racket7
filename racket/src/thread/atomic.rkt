#lang racket/base

(require "engine.rkt"
         "internal-error.rkt")

(provide atomically
         current-atomic

         start-atomic
         end-atomic

         set-end-atomic-callback!)

;; This definition is specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register:
(define current-atomic (make-pthread-parameter 0))

(define-syntax-rule (atomically expr ...)
  (begin
    (start-atomic)
    (begin0
     (let () expr ...)
     (end-atomic))))

(define (start-atomic)
  (current-atomic (add1 (current-atomic))))

(define (end-atomic)
  (define n (sub1 (current-atomic)))
  (cond
    [(and end-atomic-callback
          (zero? n))
     (define cb end-atomic-callback)
     (set! end-atomic-callback #f)
     (current-atomic n)
     (cb)]
    [else
     (current-atomic n)]))

;; ----------------------------------------

(define end-atomic-callback #f)

(define (set-end-atomic-callback! cb)
  (set! end-atomic-callback cb))
