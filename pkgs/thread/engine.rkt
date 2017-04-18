#lang racket/base
(require (only-in '#%linklet primitive-table))

(provide make-engine
         engine-block
         engine-return
         root-continuation-prompt-tag

         make-global-parameter)

(define-values (make-engine engine-block engine-return root-continuation-prompt-tag)
  (let ([ht (primitive-table '#%engine)])
    (unless ht
      (error "engines not provided by host"))
    (values
     (hash-ref ht 'make-engine)
     (hash-ref ht 'engine-block)
     (hash-ref ht 'engine-return)
     (hash-ref ht 'root-continuation-prompt-tag))))

(define (make-global-parameter v)
  (case-lambda
    [() v]
    [(new-v) (set! v new-v)]))
