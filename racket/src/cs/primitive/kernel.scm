
(define kernel-table
  (make-primitive-table
   *
   +
   -
   /
   <
   <=
   =
   >
   >=
   quotient
   remainder
   abort-current-continuation
   abs
   absolute-path?
   add1
   always-evt
   andmap
   append
   apply
   arithmetic-shift
   assoc
   assq
   assv
   bitwise-and
   bitwise-bit-set?
   bitwise-bit-field
   bitwise-ior
   bitwise-xor
   boolean?
   box
   box-cas!
   box-immutable
   box?
   break-enabled-key
   break-thread
   build-path
   byte-pregexp
   byte-pregexp?
   byte-regexp
   byte-regexp?
   byte?
   bytes
   bytes->immutable-bytes
   bytes->list
   bytes->path
   bytes->path-element
   bytes->string/latin-1
   bytes->string/locale
   bytes->string/utf-8
   bytes-append
   bytes-convert
   bytes-convert-end
   bytes-converter?
   bytes-copy!
   bytes-length
   bytes-open-converter
   bytes-ref
   bytes-set!
   bytes-utf-8-length
   bytes<?
   bytes=?
   bytes?
   caadr
   cache-configuration
   call-with-composable-continuation
   call-with-continuation-barrier
   call-with-continuation-prompt
   call-with-current-continuation
   call-with-escape-continuation
   call-with-input-file
   call-with-output-file
   call-with-semaphore
   call-with-values
   ceiling
   channel?
   channel-put-evt
   chaperone-of?
   chaperone-procedure
   chaperone-procedure*
   chaperone-struct
   char->integer
   char-alphabetic?
   char-downcase
   char-foldcase
   char-general-category
   char-graphic?
   char-numeric?
   char-upcase
   char-whitespace?
   char<=?
   char<?
   char=?
   char>=?
   char>?
   char?
   check-for-break
   checked-procedure-check-and-extract
   cleanse-path
   close-input-port
   close-output-port
   collect-garbage
   compile-enforce-module-constants
   complete-path?
   continuation-mark-set-first
   continuation-prompt-available?
   current-code-inspector
   current-command-line-arguments
   current-compile
   current-compiled-file-roots
   current-continuation-marks
   current-directory
   current-environment-variables
   current-eval
   current-gc-milliseconds
   current-inexact-milliseconds
   current-input-port
   current-inspector
   current-library-collection-links
   current-library-collection-paths
   current-load
   current-load-extension
   current-load-relative-directory
   current-load/use-compiled
   current-logger
   current-memory-use
   current-milliseconds
   current-output-port
   current-plumber
   current-print
   current-prompt-read
   current-read-interaction
   current-seconds
   current-thread
   current-thread-group
   datum-intern-literal
   default-continuation-prompt-tag
   directory-exists?
   directory-list
   display
   dynamic-wind
   environment-variables-ref
   environment-variables-set!
   eof
   eof-object?
   ephemeron-value
   eq-hash-code
   eq?
   equal-hash-code
   equal-secondary-hash-code
   equal?
   eqv?
   error
   error-print-source-location
   error-print-width
   error-value->string-handler
   eval-jit-enabled
   even?
   evt?
   exact-integer?
   exact-nonnegative-integer?
   exact-positive-integer?
   exact?
   exact->inexact
   exception-handler-key
   exit
   exit-handler
   exn-continuation-marks
   exn-message
   exn?
   expand-user-path
   expt
   extend-parameterization
   file-exists?
   file-or-directory-modify-seconds
   file-position
   file-stream-buffer-mode
   file-stream-port?
   filesystem-change-evt
   filesystem-change-evt-cancel
   find-system-path
   fixnum?
   flonum?
   floor
   floating-point-bytes->real
   flvector-length
   flvector-set!
   flvector?
   for-each
   format
   fprintf
   fxvector-length
   fxvector-set!
   fxvector?
   gensym
   get-output-bytes
   hash
   hash-clear!
   hash-copy
   hash-count
   hash-eq?
   hash-eqv?
   hash-equal?
   hash-for-each
   hash-iterate-first
   hash-iterate-key
   hash-iterate-key+value
   hash-iterate-next
   hash-iterate-pair
   hash-iterate-value
   hash-keys-subset?
   hash-map
   hash-ref
   hash-remove
   hash-remove!
   hash-set
   hash-set!
   hash-weak?
   hash?
   hasheq
   hasheqv
   immutable?
   impersonate-procedure
   impersonate-procedure*
   impersonate-struct
   impersonator-of?
   impersonator-property?
   inexact?
   inexact->exact
   input-port?
   inspector-superior?
   inspector?
   integer->char
   integer->integer-bytes
   integer-bytes->integer
   integer?
   interned-char?
   kill-thread
   length
   list
   list*
   list->bytes
   list->string
   list->vector
   list-ref
   list-tail
   list?
   list-pair?
   load
   load-extension
   load-on-demand-enabled
   logger?
   logger-name
   log-all-levels
   log-level?
   log-level-evt
   log-max-level
   log-message
   log-receiver?
   make-bytes
   make-continuation-prompt-tag
   make-ephemeron
   make-flvector
   make-fxvector
   make-hash
   make-hash-placeholder
   make-hasheq
   make-hasheq-placeholder
   make-hasheqv
   make-hasheqv-placeholder
   make-input-port
   make-immutable-hash
   make-immutable-hasheq
   make-immutable-hasheqv
   make-inspector
   make-logger
   make-log-receiver
   make-output-port
   make-parameter
   make-placeholder
   make-prefab-struct
   make-reader-graph
   make-semaphore
   make-string
   make-struct-field-accessor
   make-struct-field-mutator
   make-struct-type
   make-struct-type-property
   make-thread-cell
   make-thread-group
   make-vector
   make-weak-box
   make-weak-hash
   make-weak-hasheq
   make-weak-hasheqv
   map
   max
   min
   modulo
   negative?
   newline
   not
   null
   null?
   number->string
   number?
   object-name
   odd?
   open-input-bytes
   open-input-file
   open-input-output-file
   open-input-string
   open-output-bytes
   open-output-file
   open-output-string
   ormap
   output-port?
   parameterization?
   parameterization-key
   path->bytes
   path->complete-path
   path->directory-path
   path->string
   path-convention-type
   path-element->bytes
   path-element->string
   path-for-some-system?
   path?
   path<?
   peek-byte
   peek-bytes
   peek-bytes-avail!
   peek-bytes-avail!*
   peek-char-or-special
   placeholder-set!
   plumber-add-flush!
   port-count-lines!
   port-next-location
   port-read-handler
   positive?
   prefab-key->struct-type
   prefab-key?
   prefab-struct-key
   pregexp
   pregexp?
   primitive-table
   printf
   print
   print-as-expression
   procedure-arity
   procedure-arity?
   procedure-arity-includes?
   procedure-extract-target
   procedure-reduce-arity
   procedure-rename
   procedure->method
   procedure?
   prop:arity-string
   prop:checked-procedure
   prop:custom-write
   prop:equal+hash
   prop:evt
   prop:impersonator-of
   prop:incomplete-arity
   prop:method-arity-error
   prop:procedure
   pseudo-random-generator?
   random
   raise
   read-accept-bar-quote
   read-byte
   read-bytes
   read-bytes!
   read-bytes-avail!
   read-bytes-avail!*
   read-bytes-line
   read-case-sensitive
   read-char
   read-char-or-special
   read-decimal-as-inexact
   read-line
   read-on-demand-source
   read-string
   read-string!
   real?
   real->floating-point-bytes
   regexp
   regexp-match
   regexp-match/end
   regexp-match-positions
   regexp-match-positions/end
   regexp-match-peek
   regexp-match-peek-immediate
   regexp-match-peek-positions
   regexp-match-peek-positions/end
   regexp-match-peek-positions-immediate
   regexp-match-peek-positions-immediate/end
   regexp-match?
   regexp-max-lookbehind
   regexp-replace
   regexp-replace*
   regexp?
   relative-path?
   reparameterize
   resolve-path
   reverse
   round
   seconds->date
   semaphore-peek-evt
   semaphore-post
   semaphore-wait
   set-box!
   simplify-path
   sleep
   split-path
   string
   string->bytes/latin-1
   string->bytes/locale
   string->bytes/utf-8
   string->immutable-string
   string->list
   string->number
   string->path
   string->path-element
   string->symbol
   string->uninterned-symbol
   string->unreadable-symbol
   string-append
   string-ci<=?
   string-ci<?
   string-ci>=?
   string-ci>?
   string-copy!
   string-foldcase
   string-length
   string-locale-downcase
   string-ref
   string-set!
   string-utf-8-length
   string<=?
   string<?
   string=?
   string>=?
   string>?
   string?
   struct->vector
   struct-type?
   struct?
   sub1
   subbytes
   substring
   symbol->string
   symbol-interned?
   symbol<?
   symbol?
   sync
   sync/timeout
   system-big-endian?
   system-library-subpath
   system-path-convention-type
   system-type
   thread-group?
   thread-receive-evt
   thread-resume
   thread-send
   thread-suspend
   thread-wait
   true-object?
   time-apply
   thread
   unbox
   uncaught-exception-handler
   use-collection-link-paths
   use-compiled-file-paths
   use-user-specific-search-paths
   values
   vector
   vector->immutable-vector
   vector->list
   vector-copy!
   vector-immutable
   vector-length
   vector-ref
   vector-set!
   vector?
   version
   void
   void?
   weak-box-value
   with-input-from-file
   with-output-to-file
   wrap-evt
   write
   write-bytes
   write-bytes-avail
   write-bytes-avail*
   write-bytes-avail/enable-break
   write-string
   zero?

   flsin flcos fltan
   flasin flacos flatan
   fltruncate flround flfloor flceiling
   flexp fllog flexpt

   extflsin extflcos extfltan
   extflasin extflacos extflatan
   extfltruncate extflround extflfloor extflceiling
   extflexp extfllog extflexpt

   
   keyword<?
   string->keyword
   keyword->string
   keyword?

   cons pair?
   car cdr
   caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

   mpair?
   mcons
   mcar
   mcdr
   set-mcar!
   set-mcdr!

   raise-argument-error
   raise-arguments-error
   raise-result-error
   raise-mismatch-error
   raise-range-error
   raise-arity-error
   raise-type-error

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

   prop:exn:srclocs exn:srclocs? exn:srclocs-accessor

   struct:srcloc srcloc srcloc?
   srcloc-source srcloc-line srcloc-column srcloc-position srcloc-span
   srcloc->string

   struct:date date? date make-date
   date-second date-minute date-hour date-day date-month date-year
   date-week-day date-year-day date-dst? date-time-zone-offset
   
   struct:date* date*? date* make-date*
   date*-nanosecond date*-time-zone-name

   struct:arity-at-least arity-at-least arity-at-least?
   arity-at-least-value

   [core:correlated? syntax?]
   [core:correlated-source syntax-source]
   [core:correlated-line syntax-line]
   [core:correlated-column syntax-column]
   [core:correlated-position syntax-position]
   [core:correlated-span syntax-span]
   [core:correlated-e syntax-e]
   [core:correlated->datum syntax->datum]
   [core:datum->correlated datum->syntax]
   [core:correlated-property syntax-property]
   [core:correlated-property-symbol-keys syntax-property-symbol-keys]))
