(library (schemify)
  (export schemify-linklet
          lift-in-schemified-linklet
          jitify-schemified-linklet
          interpretable-jitified-linklet
          interpret-linklet
          linklet-bigger-than?
          prim-knowns
          a-known-procedure
          a-known-constant)
  (import (chezpart)
          (rename (rumble)
                  [correlated? rumble:correlated?]
                  [correlated-e rumble:correlated-e])
          (regexp)
          (io)
          (known-primitive))

  ;; Bridge for flattened "schemify/wrap.rkt"
  ;; and "schemify/wrap-annotation.rkt"
  (define (primitive-table name)
    (case name
      [(|#%annotation|)
       ;; Normally, schemify is schemified so that these are accessed
       ;; directly as prefixed versions:
       (hash 'syntax? annotation?
             'syntax-e annotation-expression)]
      [else #f]))

  ;; Prefiex names for direct access by schemified schemify:
  (define annotation:syntax? annotation?)
  (define annotation:syntax-e annotation-expression)

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
