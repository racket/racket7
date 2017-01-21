#lang racket/base
(require "../compile/side-effect.rkt")

(provide register-known-primitives!)

(define (register-known-primitives! seen-defns)
  ;; Register some core primitives that have specific properties:
  (hash-set! seen-defns 'struct:exn:fail (known-struct-op 'struct-type 2))
  (hash-set! seen-defns 'make-thread-cell (known-struct-op 'constructor 1))
  (hash-set! seen-defns 'make-continuation-prompt-tag (known-struct-op 'constructor 1))
  (hash-set! seen-defns 'make-weak-hash (known-struct-op 'constructor 0))
  (hash-set! seen-defns 'gensym (known-struct-op 'constructor 0))
  (hash-set! seen-defns 'string (known-struct-op 'constructor 2))
  (hash-set! seen-defns 'cons (known-struct-op 'constructor 2)))
