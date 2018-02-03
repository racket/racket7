#lang racket/base
(require (only-in '#%linklet primitive-in-category?))

(provide direct-call-primitive?)

(define (direct-call-primitive? rator e prim-knowns)
  (primitive-in-category? rator 'noncm))
