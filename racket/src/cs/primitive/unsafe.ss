
(define unsafe-table
  (make-primitive-table
   
   unsafe-car
   unsafe-cdr
   unsafe-list-tail
   unsafe-list-ref
   unsafe-cons-list

   unsafe-fx+
   unsafe-fx-
   unsafe-fx*
   unsafe-fxquotient
   unsafe-fxremainder
   unsafe-fxmodulo
   unsafe-fxabs
   unsafe-fxand
   unsafe-fxior
   unsafe-fxxor
   unsafe-fxnot
   unsafe-fxrshift
   unsafe-fxlshift

   unsafe-fx=
   unsafe-fx<
   unsafe-fx>
   unsafe-fx>=
   unsafe-fx<=
   unsafe-fxmin
   unsafe-fxmax

   unsafe-fl+
   unsafe-fl-
   unsafe-fl*
   unsafe-fl/
   unsafe-flabs

   unsafe-fl=
   unsafe-fl<
   unsafe-fl>
   unsafe-fl>=
   unsafe-fl<=
   unsafe-flmin
   unsafe-flmax

   unsafe-fx->fl
   unsafe-fl->fx
   
   unsafe-flrandom

   unsafe-flsqrt

   unsafe-make-flrectangular
   unsafe-flreal-part
   unsafe-flimag-part

   ;; Currently omitted since regular Racket's #%unsafe does not provide them

   ;; unsafe-flround
   ;; unsafe-flfloor
   ;; unsafe-flceiling
   ;; unsafe-fltruncate

   ;; unsafe-flsin
   ;; unsafe-flcos
   ;; unsafe-fltan
   ;; unsafe-flasin
   ;; unsafe-flacos
   ;; unsafe-flatan
   ;; unsafe-fllog
   ;; unsafe-flexp
   ;; unsafe-flexpt

   unsafe-extfl*
   unsafe-extfl+
   unsafe-extfl-
   unsafe-extfl/
   unsafe-extfl<
   unsafe-extfl<=
   unsafe-extfl=
   unsafe-extfl>
   unsafe-extfl>=
   unsafe-extflabs
   unsafe-extflmax
   unsafe-extflmin
   unsafe-extflsqrt
   unsafe-extfl->fx
   unsafe-fx->extfl

   unsafe-extflvector-length
   unsafe-extflvector-ref
   unsafe-extflvector-set!

   unsafe-unbox*
   unsafe-set-box*!
   unsafe-set-box!
   unsafe-unbox
   unsafe-box*-cas!

   unsafe-mcar
   unsafe-mcdr
   unsafe-set-mcar!
   unsafe-set-mcdr!

   unsafe-vector-ref
   unsafe-vector-set!
   unsafe-vector*-ref
   unsafe-vector*-set!
   unsafe-vector*-cas!
   unsafe-vector-length
   unsafe-vector*-length
   
   unsafe-fxvector-length
   unsafe-fxvector-ref
   unsafe-fxvector-set!

   unsafe-flvector-length
   unsafe-flvector-ref
   unsafe-flvector-set!

   unsafe-s16vector-ref
   unsafe-s16vector-set!
   unsafe-u16vector-ref
   unsafe-u16vector-set!
   unsafe-f64vector-ref
   unsafe-f64vector-set!
   unsafe-f80vector-set!
   unsafe-f80vector-ref

   unsafe-bytes-length
   unsafe-bytes-ref
   unsafe-bytes-set!

   unsafe-string-length
   unsafe-string-set!
   unsafe-string-ref
   
   unsafe-struct-ref
   unsafe-struct-set!
   unsafe-struct*-ref
   unsafe-struct*-set!
   
   unsafe-immutable-hash-iterate-key+value
   unsafe-immutable-hash-iterate-pair
   unsafe-immutable-hash-iterate-value
   unsafe-immutable-hash-iterate-key
   unsafe-immutable-hash-iterate-first
   unsafe-immutable-hash-iterate-next
   unsafe-mutable-hash-iterate-key+value
   unsafe-mutable-hash-iterate-pair
   unsafe-mutable-hash-iterate-value
   unsafe-mutable-hash-iterate-key
   unsafe-mutable-hash-iterate-first
   unsafe-mutable-hash-iterate-next
   unsafe-weak-hash-iterate-key+value
   unsafe-weak-hash-iterate-pair
   unsafe-weak-hash-iterate-value
   unsafe-weak-hash-iterate-key
   unsafe-weak-hash-iterate-first
   unsafe-weak-hash-iterate-next

   unsafe-chaperone-procedure
   unsafe-impersonate-procedure
   unsafe-impersonate-vector
   unsafe-chaperone-vector

   unsafe-undefined
   check-not-unsafe-undefined
   check-not-unsafe-undefined/assign
   prop:chaperone-unsafe-undefined
   chaperone-struct-unsafe-undefined

   unsafe-start-atomic
   unsafe-end-atomic
   unsafe-start-breakable-atomic
   unsafe-end-breakable-atomic
   unsafe-in-atomic?
   unsafe-set-on-atomic-timeout!

   unsafe-thread-at-root
   unsafe-make-custodian-at-root
   unsafe-custodian-register
   unsafe-custodian-unregister
   unsafe-register-process-global

   unsafe-make-security-guard-at-root

   unsafe-abort-current-continuation/no-wind
   unsafe-call-with-composable-continuation/no-wind

   unsafe-poller
   unsafe-poll-ctx-fd-wakeup
   unsafe-poll-ctx-eventmask-wakeup
   unsafe-poll-ctx-milliseconds-wakeup
   unsafe-signal-received
   unsafe-set-sleep-in-thread!))
