
;; Exports that are not exposed to Racket, but
;; can be used in a linklet:

(define internal-table
  (make-primitive-table
   call/cm
   extract-procedure
   set-ctl-c-handler!
   register-linklet-instantiate-continuation!
   impersonator-val
   impersonate-ref
   impersonate-set!
   struct-type-install-properties!
   structure-type-lookup-prefab-uid
   register-struct-constructor!
   register-struct-predicate!
   register-struct-field-accessor!
   register-struct-field-mutator!
   struct-property-set!))
