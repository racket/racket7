(library (schemify)
  (export schemify-linklet
          schemify-body
          prim-knowns)
  (import (chezpart)
          (rename (core)
                  [correlated? core:correlated?]
                  [correlated-e core:correlated-e])
          (regexp)
          (port)
          (known-primitive))
  
  ;; Bridge for flattened "schemify/wrap.rkt":
  (define (primitive-table kernel)
    (hash 'syntax? core:correlated?
          'syntax-e core:correlated-e))

  (include "compiled/schemify.scm")

  (define (add-known ht syms extract known)
    (let loop ([ht ht] [syms syms])
      (cond
       [(null? syms) ht]
       [else (loop (hash-set ht
                             (extract (car syms))
                             (if (procedure? known)
                                 (known (car syms))
                                 known))
                   (cdr syms))])))

  (define prim-knowns
    (add-known
     (add-known
      (add-known
       (add-known (hasheq) known-procedures (lambda (s) s) a-known-procedure)
       known-struct-type-property/immediate-guards (lambda (s) s)
       a-known-struct-type-property/immediate-guard)
      known-constructors car
      (lambda (s) (known-constructor (car s) (cadr s))))
     known-constants (lambda (x) x)
     a-known-constant)))
