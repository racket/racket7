#lang racket/base
(require "../common/check.rkt"
         "port.rkt")

(provide prop:input-port
         input-port?
         ->core-input-port
         (struct-out core-input-port)
         make-core-input-port)

(define-values (prop:input-port input-port-via-property? input-port-ref)
  (make-struct-type-property 'input-port
                             (lambda (v sti)
                               (check 'prop:input-port (lambda (v) (or (exact-nonnegative-integer? v)
                                                                       (input-port? v)))
                                      #:contract "(or/c input-port? exact-nonnegative-integer?)"
                                      v)
                               (check-immutable-field 'prop:input-port v sti)
                               (if (exact-nonnegative-integer? v)
                                   (make-struct-field-accessor (list-ref sti 3) v)
                                   v))))

(define (input-port? p)
  (or (core-input-port? p)
      (input-port-via-property? p)))

(define (->core-input-port v)
  (cond
    [(core-input-port? v) v]
    [(input-port? v)
     (let ([p (input-port-ref v)])
       (cond
         [(struct-accessor-procedure? p)
          (->core-input-port (p v))]
         [else
          (->core-input-port p)]))]
    [else
     empty-input-port]))

(struct core-input-port core-port
  (
   ;; Various functions below are called in atomic mode. The
   ;; intent of atomic mode is to ensure that the completion and
   ;; return of the function is atomic with respect to some further
   ;; activity, such as position and line counting. Any of the
   ;; functions is free to exit and re-enter atomic mode. Leave
   ;; atomic mode explicitly before raising an exception.

   read-byte ; #f or (-> (or/c byte? eof-object? evt?))
   ;;          Called in atomic mode.
   ;;          Non-blocking byte read, where an event must be
   ;;          returned if no byte is available. This shortcut is optional.

   read-in   ; port or (bytes start-k end-k copy? -> (or/c integer? ...))
   ;;          Called in atomic mode.
   ;;          A port values redirects to the port. Otherwise, the function
   ;;          never blocks, and can assume `(- end-k start-k)` is non-zero.
   ;;          The `copy?` flag indicates that the given byte string should
   ;;          not be exposed to untrusted code, and instead of should be
   ;;          copied if necessary. The return values are the same as
   ;;          documented for `make-input-port`.

   peek-byte ; #f or (-> (or/c byte? eof-object? evt?))
   ;;          Called in atomic mode.
   ;;          Non-blocking byte read, where an event must be
   ;;          returned if no byte is available. This shortcut is optional.

   peek-in   ; port or (bytes start-k end-k skip-k copy? -> (or/c integer? ...))
   ;;          Called in atomic mode.
   ;;          A port values redirects to the port. Otherwise, the function
   ;;          never blocks, and it can assume that `(- end-k start-k)` is non-zero.
   ;;          The `copy?` flag is the same as for `read-in`.  The return values
   ;;          are the same as documented for `make-input-port`.

   get-progress-evt ; #f or (-> evt?)
   ;;           *Not) called in atomic mode.
   ;;           Optional support for progress events.

   commit    ; (amt-k progress-evt? evt?) -> (or/c bytes? #f)
   ;;          Called in atomic mode.
   ;;          Goes with `get-progress-evt`. The final `evt?`
   ;;          argument is constrained to a few kinds of events;
   ;;          see docs for `port-commit-peeked` for more information.
   ;;          The result is the committed bytes on success, #f on
   ;;          failure.

   [pending-eof? #:mutable]
   [read-handler #:mutable]))

(define (make-core-input-port #:name name
                              #:data [data #f]
                              #:read-byte [read-byte #f]
                              #:read-in read-in
                              #:peek-byte [peek-byte #f]
                              #:peek-in peek-in
                              #:close close
                              #:get-progress-evt [get-progress-evt #f]
                              #:commit [commit #f]
                              #:get-location [get-location #f]
                              #:count-lines! [count-lines! #f]
                              #:on-file-position [on-file-position void])
  (core-input-port name
                   data

                   close
                   count-lines!
                   get-location
                   on-file-position
                   
                   #f   ; closed?
                   #f   ; closed-sema
                   0    ; offset
                   #f   ; state
                   #f   ; cr-state
                   #f   ; line
                   #f   ; column
                   #f   ; position
                   
                   read-byte
                   read-in
                   peek-byte
                   peek-in
                   get-progress-evt
                   commit
                   #f   ; pending-eof?
                   #f)) ; read-handler

(define empty-input-port
  (make-core-input-port #:name 'empty
                        #:read-in (lambda (bstr start-k end-k copy?) eof)
                        #:peek-in (lambda (bstr start-k end-k skip-k copy?) eof)
                        #:close void))
