(library (core)
  (export version
          banner

          null eof void void?

          begin0

          dynamic-wind
          call-with-current-continuation
          call-with-composable-continuation
          call-with-escape-continuation
          continuation?

          make-continuation-prompt-tag
          continuation-prompt-tag?
          default-continuation-prompt-tag
          root-continuation-prompt-tag
          call-with-continuation-prompt
          call-with-continuation-barrier
          abort-current-continuation
          continuation-prompt-available?
          impersonate-prompt-tag
          chaperone-prompt-tag
          (rename [break-enabled-key core:break-enabled-key])
          unsafe-abort-current-continuation/no-wind
          unsafe-call-with-composable-continuation/no-wind

          with-continuation-mark
          call/cm ; not exported to Racket
          call-with-immediate-continuation-mark
          continuation-mark-set-first
          continuation-mark-set->list
          continuation-mark-set->list*
          continuation-mark-set->context
          current-continuation-marks
          continuation-marks
          continuation-mark-set?
          make-continuation-mark-key
          continuation-mark-key?
          impersonate-continuation-mark-key
          chaperone-continuation-mark-key
          $current-mark-stack

          make-engine
          engine-block
          engine-return
          set-ctl-c-handler! ; not exported to Racket
          set-scheduler-lock-callbacks! ; not exported to Racket

          make-thread-cell
          thread-cell?
          thread-cell-ref
          thread-cell-set!
          current-preserved-thread-cell-values
          thread-cell-values?

          parameterization-key
          make-parameter
          make-derived-parameter
          parameter?
          extend-parameterization
          parameterization?
          parameter-procedure=?

          raise
          error-print-width
          error-value->string-handler
          error-print-context-length
          exception-handler-key
          uncaught-exception-handler
          error-display-handler
          error-escape-handler
          register-linklet-instantiate-continuation! ; not exported to Racket

          current-inspector
          make-inspector
          make-sibling-inspector

          struct:exn exn exn? exn-message exn-continuation-marks
          struct:exn:break exn:break exn:break? exn:break-continuation
          struct:exn:break:hang-up exn:break:hang-up exn:break:hang-up?
          struct:exn:break:terminate exn:break:terminate exn:break:terminate?
          struct:exn:fail exn:fail exn:fail?
          struct:exn:fail:contract exn:fail:contract exn:fail:contract?
          struct:exn:fail:contract:arity exn:fail:contract:arity exn:fail:contract:arity?
          struct:exn:fail:contract:divide-by-zero exn:fail:contract:divide-by-zero exn:fail:contract:divide-by-zero?
          struct:exn:fail:contract:non-fixnum-result exn:fail:contract:non-fixnum-result exn:fail:contract:non-fixnum-result?
          struct:exn:fail:contract:continuation exn:fail:contract:continuation exn:fail:contract:continuation?
          struct:exn:fail:contract:variable exn:fail:contract:variable exn:fail:contract:variable? exn:fail:contract:variable-id
          struct:exn:fail:read exn:fail:read exn:fail:read? exn:fail:read-srclocs
          struct:exn:fail:read:eof exn:fail:read:eof exn:fail:read:eof?
          struct:exn:fail:read:non-char exn:fail:read:non-char exn:fail:read:non-char?
          struct:exn:fail:filesystem exn:fail:filesystem exn:fail:filesystem?
          struct:exn:fail:filesystem:exists exn:fail:filesystem:exists exn:fail:filesystem:exists?
          struct:exn:fail:filesystem:version exn:fail:filesystem:version exn:fail:filesystem:version?
          struct:exn:fail:filesystem:errno exn:fail:filesystem:errno exn:fail:filesystem:errno? exn:fail:filesystem:errno-errno
          struct:exn:fail:network exn:fail:network exn:fail:network?
          struct:exn:fail:network:errno exn:fail:network:errno exn:fail:network:errno? exn:fail:network:errno-errno
          struct:exn:fail:out-of-memory exn:fail:out-of-memory exn:fail:out-of-memory?
          struct:exn:fail:unsupported exn:fail:unsupported exn:fail:unsupported?
          struct:exn:fail:user exn:fail:user exn:fail:user?

          struct:srcloc srcloc srcloc?
          srcloc-source srcloc-line srcloc-column srcloc-position srcloc-span

          struct:date date? date make-date
          date-second date-minute date-hour date-day date-month date-year
          date-week-day date-year-day date-dst? date-time-zone-offset

          struct:date* date*? date* make-date*
          date*-nanosecond date*-time-zone-name

          struct:arity-at-least arity-at-least arity-at-least?
          arity-at-least-value

          prop:procedure
          prop:incomplete-arity
          prop:method-arity-error
          apply
          procedure?
          procedure-specialize
          |#%app|
          extract-procedure ; not exported to Racket
          procedure-arity-includes?
          procedure-arity
          procedure-result-arity
          procedure-extract-target
          procedure-closure-contents-eq?
          procedure-reduce-arity
          procedure-rename
          procedure-arity?

          equal?
          equal?/recur

          impersonator?
          chaperone?
          impersonator-of?
          chaperone-of?
          impersonator-val ; not exported to Racket
          impersonate-ref ; not exported to Racket
          impersonate-set! ; not exported to Racket
          impersonator-property?
          make-impersonator-property
          impersonator-property-accessor-procedure?
          impersonator-ephemeron

          impersonate-procedure
          chaperone-procedure
          impersonate-procedure*
          chaperone-procedure*
          procedure-impersonator*?
          impersonator-prop:application-mark
          unsafe-impersonate-procedure
          unsafe-chaperone-procedure

          raise-argument-error
          raise-arguments-error
          raise-result-error
          raise-mismatch-error
          raise-range-error
          raise-arity-error
          raise-type-error
          raise-binding-result-arity-error

          make-struct-type-property
          struct-type-property?
          struct-type-property-accessor-procedure?
          make-struct-type
          struct-type-install-properties! ; not exported to Racket
          structure-type-lookup-prefab-uid ; not exported to Racket
          make-struct-field-accessor
          make-struct-field-mutator
          struct-type-constructor-add-guards ; not exported to Racket
          register-struct-constructor! ; not exported to Racket
          register-struct-predicate! ; not exported to Racket
          register-struct-field-accessor! ; not exported to Racket
          register-struct-field-mutator! ; not exported to Racket
          struct-property-set!  ; not exported to Racket
          struct-constructor-procedure?
          struct-predicate-procedure?
          struct-accessor-procedure?
          struct-mutator-procedure?
          struct?
          struct-type?
          procedure-struct-type?
          struct-type-info
          struct-info
          struct-type-make-constructor
          struct-type-make-predicate
          unsafe-struct-ref
          unsafe-struct-set!
          struct->vector
          prefab-key?
          prefab-struct-key
          prefab-key->struct-type
          make-prefab-struct
          prop:authentic
          prop:equal+hash
          inspector?
          inspector-superior?
          impersonate-struct
          chaperone-struct

          prop:object-name
          object-name

          eq-hash-code
          eqv-hash-code
          equal-hash-code

          hash hasheqv hasheq
          make-hash make-hasheqv make-hasheq
          make-immutable-hash make-immutable-hasheqv make-immutable-hasheq
          make-weak-hash make-weak-hasheq make-weak-hasheqv
          hash-ref hash-set hash-set! hash-remove hash-remove!
          hash-for-each hash-map hash-copy hash-clear hash-clear!
          hash-iterate-first hash-iterate-next
          hash-iterate-key hash-iterate-value
          hash-iterate-key+value hash-iterate-pair
          unsafe-immutable-hash-iterate-first unsafe-immutable-hash-iterate-next
          unsafe-immutable-hash-iterate-key unsafe-immutable-hash-iterate-value
          unsafe-immutable-hash-iterate-key+value unsafe-immutable-hash-iterate-pair
          unsafe-mutable-hash-iterate-first unsafe-mutable-hash-iterate-next
          unsafe-mutable-hash-iterate-key unsafe-mutable-hash-iterate-value
          unsafe-mutable-hash-iterate-key+value unsafe-mutable-hash-iterate-pair
          unsafe-weak-hash-iterate-first unsafe-weak-hash-iterate-next
          unsafe-weak-hash-iterate-key unsafe-weak-hash-iterate-value
          unsafe-weak-hash-iterate-key+value unsafe-weak-hash-iterate-pair

          hash? hash-eq? hash-equal? hash-eqv? hash-weak? immutable-hash?
          hash-count
          hash-keys-subset?
          ;; For intern tables:
          weak-hash-ref-key

          impersonate-hash
          chaperone-hash

          bytes shared-bytes
          bytes?
          bytes-length
          make-bytes make-shared-bytes
          bytes-ref bytes-set!
          bytes->list list->bytes
          bytes->immutable-bytes
          bytes-copy! bytes-copy bytes-fill!
          bytes=? bytes<? bytes>? bytes<=? bytes>=?
          bytes-append
          subbytes

          string-copy!
          substring

          gensym
          symbol-interned?
          symbol-unreadable?
          string->uninterned-symbol
          string->unreadable-symbol
          symbol->string

          list?

          vector?
          mutable-vector?
          vector-length
          vector-ref
          vector-set!
          vector-copy
          vector-copy!
          vector-immutable
          vector->values
          vector-fill!
          vector->immutable-vector

          impersonate-vector
          impersonate-vector*
          chaperone-vector
          chaperone-vector*

          box? unbox set-box!
          box-cas!
          make-weak-box weak-box? weak-box-value
          impersonate-box
          chaperone-box

          immutable?

          keyword?
          keyword->string
          string->keyword
          keyword<?

          symbol<?

          even? odd?

          exact-integer?
          exact-nonnegative-integer?
          exact-positive-integer?
          inexact-real?
          byte?
          double-flonum?
          single-flonum?
          real->double-flonum
          real->single-flonum
          arithmetic-shift
          integer-sqrt
          integer-sqrt/remainder
          integer->integer-bytes
          integer-bytes->integer
          real->floating-point-bytes
          floating-point-bytes->real
          system-big-endian?
          string->number
          quotient/remainder

          random
          random-seed
          pseudo-random-generator?
          make-pseudo-random-generator
          current-pseudo-random-generator
          vector->pseudo-random-generator
          vector->pseudo-random-generator!
          pseudo-random-generator->vector
          pseudo-random-generator-vector?

          mpair? mcons mcar mcdr set-mcar! set-mcdr!

          flvector?
          (rename [new-flvector flvector])
          make-flvector
          flvector-length
          flvector-ref
          flvector-set!
          flvector-copy
          shared-flvector
          make-shared-flvector
          unsafe-flvector-length
          unsafe-flvector-set!
          unsafe-flvector-ref

          correlated?
          correlated-source
          correlated-line
          correlated-column
          correlated-position
          correlated-span
          correlated-e
          correlated->datum
          datum->correlated
          correlated-property
          correlated-property-symbol-keys

          make-reader-graph
          make-placeholder
          placeholder?
          placeholder-set!
          placeholder-get
          hash-placeholder?
          make-hash-placeholder
          make-hasheq-placeholder
          make-hasheqv-placeholder

          time-apply
          current-inexact-milliseconds
          current-milliseconds
          current-process-milliseconds
          current-gc-milliseconds
          current-seconds
          seconds->date

          collect-garbage
          current-memory-use
          dump-memory-stats
          phantom-bytes?
          make-phantom-bytes
          set-phantom-bytes!
          set-garbage-collect-notify! ; not exported to Racket

          make-will-executor
          will-executor?
          will-register
          will-try-execute

          make-ephemeron
          ephemeron?
          ephemeron-value

          system-type

          unsafe-car
          unsafe-cdr
          unsafe-list-tail
          unsafe-list-ref

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

          unsafe-flround
          unsafe-flfloor
          unsafe-flceiling
          unsafe-fltruncate

          unsafe-flsin
          unsafe-flcos
          unsafe-fltan
          unsafe-flasin
          unsafe-flacos
          unsafe-flatan
          unsafe-fllog
          unsafe-flexp
          unsafe-flsqrt
          unsafe-flexpt

          extfl* extfl+ extfl- ->extfl
          extfl->exact extfl->exact-integer
          extfl->floating-point-bytes extfl->fx
          extfl->inexact
          extfl/ extfl< extfl<= extfl= extfl> extfl>=
          extflabs extflacos extflasin extflatan extflceiling
          extflcos extflexp extflexpt floating-point-bytes->extfl
          extflfloor fx->extfl extfllog make-shared-extflvector
          make-extflvector extflmax extflmin extflonum-available?
          extflonum? real->extfl extflround shared-extflvector
          extflsin extflsqrt extfltan extfltruncate extflvector
          extflvector-length extflvector-ref extflvector-set! extflvector?

          tcp-listener? tcp-port?
          tcp-abandon-port tcp-accept tcp-accept-evt tcp-accept-ready?
          tcp-accept/enable-break tcp-addresses tcp-close tcp-connect
          tcp-connect/enable-break tcp-listen
          udp? udp-bind! udp-bound? udp-close udp-connect! udp-connected?
          udp-multicast-interface udp-multicast-join-group! udp-multicast-leave-group!
          udp-multicast-loopback? udp-multicast-set-interface! udp-multicast-set-loopback!
          udp-multicast-set-ttl! udp-multicast-ttl udp-open-socket udp-receive! udp-receive!*
          udp-receive!-evt udp-receive!/enable-break udp-receive-ready-evt udp-send
          udp-send* udp-send-evt udp-send-ready-evt udp-send-to udp-send-to* udp-send-to-evt
          udp-send-to/enable-break udp-send/enable-break

          place-enabled? place? place-channel? place-break
          place-channel-get place-channel-put place-sleep
          place-channel place-dead-evt place-kill place-message-allowed?
          dynamic-place place-wait place-pumper-threads place-shared?

          _bool _bytes _double _double* _fixint _fixnum _float _fpointer _gcpointer
          _int16 _int32 _int64 _int8 _longdouble _path _pointer _scheme _stdbool _void
          _string/ucs-4 _string/utf-16 _symbol _ufixint _ufixnum _uint16 _uint32 _uint64 _uint8
          compiler-sizeof cpointer-gcable? cpointer-tag cpointer?
          ctype-alignof ctype-basetype ctype-c->scheme ctype-scheme->c ctype-sizeof ctype?
          end-stubborn-change extflvector->cpointer
          ffi-call ffi-callback ffi-callback? ffi-lib ffi-lib-name ffi-lib? ffi-obj ffi-obj-lib
          ffi-obj-name  ffi-obj? flvector->cpointer free free-immobile-cell lookup-errno
          make-array-type make-cstruct-type make-ctype make-late-weak-box make-late-weak-hasheq
          make-sized-byte-string make-stubborn-will-executor make-union-type malloc malloc-immobile-cell
          memcpy memmove memset offset-ptr? prop:cpointer ptr-add ptr-add! ptr-equal? ptr-offset ptr-ref
          ptr-set! saved-errno set-cpointer-tag! set-ptr-offset! vector->cpointer
          unsafe-register-process-global

          unsafe-unbox
          unsafe-unbox*
          unsafe-set-box!
          unsafe-set-box*!

          unsafe-vector-ref
          unsafe-vector-set!
          unsafe-vector*-ref
          unsafe-vector*-set!
          unsafe-vector-length
          unsafe-vector*-length

          unsafe-fxvector-length
          unsafe-fxvector-ref
          unsafe-fxvector-set!

          unsafe-bytes-length
          unsafe-bytes-ref
          unsafe-bytes-set!

          unsafe-undefined
          check-not-unsafe-undefined
          check-not-unsafe-undefined/assign

          unsafe-string-length)
  (import (chezpart)
          (only (chezscheme)
                format
                fprintf
                current-error-port
                error)
          (only (chezscheme csv7)
                record-field-accessor
                record-field-mutator))

  (define (version) "6.9.1.5")
  (define (banner) (string-append "Welcome to Racket " (version) "\n"))

  (define none (chez:gensym "none"))
  (define none2 (chez:gensym "none2"))

  (include "core/check.ss")
  (include "core/constant.ss")
  (include "core/hash-code.ss")
  (include "core/symbol.ss")
  (include "core/struct.ss")
  (include "core/prefab.ss")
  (include "core/chaperone.ss")
  (include "core/equal.ss")
  (include "core/number.ss")
  (include "core/procedure.ss")
  (include "core/object-name.ss")
  (include "core/arity.ss")
;;  (include "core/hamt.ss")
  (include "core/patricia.ss")
  (include "core/hash.ss")
  (include "core/lock.ss")
  (include "core/thread-cell.ss")
  (include "core/begin0.ss")
  (include "core/control.ss")
  (include "core/interrupt.ss")
  (include "core/parameter.ss")
  (include "core/engine.ss")
  (include "core/error.ss")
  (include "core/srcloc.ss")
  (include "core/bytes.ss")
  (include "core/string.ss")
  (include "core/list.ss")
  (include "core/vector.ss")
  (include "core/box.ss")
  (include "core/immutable.ss")
  (include "core/keyword.ss")
  (include "core/mpair.ss")
  (include "core/flvector.ss")
  (include "core/correlated.ss")
  (include "core/graph.ss")
  (include "core/time.ss")
  (include "core/random.ss")
  (include "core/memory.ss")
  (include "core/ephemeron.ss")
  (include "core/will-executor.ss")
  (include "core/system.ss")
  (include "core/unsafe.ss")
  (include "core/extfl.ss")
  (include "core/network.ss")
  (include "core/place.ss")
  (include "core/foreign.ss")

  (set-base-exception-handler!)
  (set-collect-handler!)
  (set-primitive-applicables!)
  (set-continuation-applicables!)
  (set-impersonator-applicables!)
  (set-hash-hash!)
  (set-impersonator-hash!)
  (set-procedure-impersonator-hash!)
  (set-vector-impersonator-hash!)
  (set-box-impersonator-hash!))
