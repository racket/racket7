(library (core)
  (export null eof void void?

          begin0

          dynamic-wind
          call-with-current-continuation
          call-with-composable-continuation
          call-with-escape-continuation

          make-continuation-prompt-tag
          default-continuation-prompt-tag
          root-continuation-prompt-tag
          call-with-continuation-prompt
          call-with-continuation-barrier
          abort-current-continuation
          continuation-prompt-available?
          (rename [break-enabled-key core:break-enabled-key])
          
          with-continuation-mark
          call-with-immediate-continuation-mark
          continuation-mark-set-first
          continuation-mark-set->list
          continuation-mark-set->list*
          current-continuation-marks
          continuation-marks
          continuation-mark-set?

          make-engine
          engine-block
          engine-return

          make-thread-cell
          thread-cell?
          thread-cell-ref
          thread-cell-set!

          parameterization-key
          make-parameter
          extend-parameterization
          parameterization?

          raise
          error-print-width
          error-value->string-handler
          error-print-context-length
          exception-handler-key
          uncaught-exception-handler

          current-inspector
          make-inspector

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
          struct:exn:fail:contract:variable exn:fail:contract:variable exn:fail:contract:variable?
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
          prop:method-arity-error
          apply
          procedure?
          |#%app|
          procedure-arity-includes?
          procedure-arity
          procedure-extract-target
          
          raise-argument-error
          raise-arguments-error
          raise-result-error
          raise-mismatch-error
          raise-range-error
          raise-arity-error
          raise-type-error
          raise-result-arity-error

          make-struct-type-property
          make-struct-type
          struct-type-install-properties!
          make-struct-field-accessor
          make-struct-field-mutator
          struct?
          struct-type?
          struct-type
          struct-type-field-count
          unsafe-struct-ref
          unsafe-struct-set!
          struct->vector
          struct-type-transparent?
          struct-transparent-type
          prefab-key?
          prefab-struct-key
          prefab-key->struct-type
          make-prefab-struct
          prop:equal+hash
          inspector?
          inspector-superior?

          eq-hash-code
          eqv-hash-code
          equal-hash-code

          hash hasheqv hasheq
          make-hash make-hasheqv make-hasheq
          make-immutable-hash make-immutable-hasheqv make-immutable-hasheq
          make-weak-hash make-weak-hasheq make-weak-hasheqv
          hash-ref hash-set hash-set! hash-remove hash-remove!
          hash-for-each hash-map hash-copy hash-clear!
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

          bytes bytes?
          bytes-length
          make-bytes
          bytes-ref bytes-set!
          bytes->list list->bytes
          bytes->immutable-bytes
          bytes-copy! bytes-copy
          bytes=? bytes<? bytes>? bytes<=? bytes>=?
          bytes-append
          subbytes

          string-copy!
          substring

          gensym
          symbol-interned?
          string->uninterned-symbol
          string->unreadable-symbol
          symbol->string

          list?

          vector-copy!
          vector-immutable
          vector->values

          box-cas!
          make-weak-box weak-box? weak-box-value

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
          byte?
          arithmetic-shift
          random
          pseudo-random-generator?

          mpair? mcons mcar mcdr set-mcar! set-mcdr!
          
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

          time-apply
          current-inexact-milliseconds
          current-milliseconds
          current-gc-milliseconds
          current-seconds
          seconds->date

          collect-garbage
          current-memory-use

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

          extflsin extflcos extfltan
          extflasin extflacos extflatan
          extfltruncate extflround extflfloor extflceiling
          extflexp extfllog extflexpt

          unsafe-vector-ref
          unsafe-vector-set!
          unsafe-vector*-ref
          unsafe-vector*-set!
          unsafe-vector-length
          unsafe-vector*-length
          
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

  (define none (chez:gensym "none"))
  (define none2 (chez:gensym "none2"))

  (include "core/constant.ss")
  (include "core/hash-code.ss")
  (include "core/symbol.ss")
  (include "core/struct.ss")
  (include "core/procedure.ss")
  (include "core/hamt.ss")
  (include "core/hash.ss")
  (include "core/lock.ss")
  (include "core/thread-cell.ss")
  (include "core/begin0.ss")
  (include "core/control.ss")
  (include "core/interrupt.ss")
  (include "core/parameter.ss")
  (include "core/engine.ss")
  (include "core/error.ss")
  (include "core/bytes.ss")
  (include "core/string.ss")
  (include "core/list.ss")
  (include "core/vector.ss")
  (include "core/box.ss")
  (include "core/immutable.ss")
  (include "core/keyword.ss")
  (include "core/mpair.ss")
  (include "core/number.ss")
  (include "core/correlated.ss")
  (include "core/time.ss")
  (include "core/memory.ss")
  (include "core/system.ss")
  (include "core/unsafe.ss")
  
  (set-base-exception-handler!)
  (set-primitive-applicables!)
  (set-continuation-applicables!))
