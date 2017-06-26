#lang racket/base
(require "../common/check.rkt")

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

(struct core-input-port (name      ; anything, reported as `object-name` for the port
                         data      ; anything, effectively a subtype indicator

                         ;; No locks are held during the call of any of the following functions.

                         read-byte ; #f or (-> (or/c byte? eof-object?))
                         ;;          Block as needed to read one byte as a shortcut to
                         ;;          using `read-in`.  The shortcut is optional.

                         read-in   ; port or (bytes start-k end-k copy? -> (or/c integer? ...))
                         ;;          A port values redirects to the port. Otherwise, the function
                         ;;          never blocks, and can assume `(- end-k start-k)` is non-zero.
                         ;;          The `copy?` flag indicates that the given byte string should
                         ;;          not be exposed to untrusted code, and instead of should be
                         ;;          copied if necessary. The return values are the same as
                         ;;          documented for `make-input-port`.

                         peek-byte ; #f or (-> (or/c byte? eof-object?))
                         ;;          Blocks as needed to peek one byte as a shortcut to
                         ;;          using `peek-in`. The shortcut is optional.

                         peek-in   ; port or (bytes start-k end-k skip-k copy? -> (or/c integer? ...))

                         ;;          A port values redirects to the port. Otherwise, the function
                         ;;          never blocks, and it can assume that `(- end-k start-k)` is non-zero.
                         ;;          The `copy?` flag is the same as for `read-in`.  The return values
                         ;;          are the same as documented for `make-input-port`.

                         close     ; -> (void)

                         get-progress-evt ; #f or (-> evt?)
                         ;;           Optional support for progress events.

                         commit    ; (amt-k progress-evt? evt?) -> (void)
                         ;;          Goes with `get-progress-evt`. The final `evt?`
                         ;;          argument is constrained to a few kinds of events;
                         ;;          see docs for `port-commit-peeked` for more information.

                         get-location
                         count-lines!
                         on-file-position

                         [closed? #:mutable]
                         [closed-sema #:mutable] ; #f or a semaphore to be posed on close
                         [offset #:mutable] ; count plain bytes
                         [state #:mutable] ; state of UTF-8 decoding
                         [cr-state #:mutable] ; state of CRLF counting as a single LF
                         [line #:mutable]   ; count newlines
                         [column #:mutable] ; count UTF-8 characters in line
                         [position #:mutable]    ; count UTF-8 characters
                         [pending-eof? #:mutable]
                         [read-handler #:mutable])
  #:property prop:object-name (struct-field-index name))

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
                   read-byte
                   read-in
                   peek-byte
                   peek-in
                   close
                   get-progress-evt
                   commit
                   get-location
                   count-lines!
                   on-file-position
                   #f   ; closed?
                   #f   ; closed-sema
                   0    ; offset
                   #f   ; state
                   #f   ; cr-state
                   #f   ; line
                   #f   ; column
                   #f   ; position
                   #f   ; pending-eof?
                   #f)) ; read-handler

(define empty-input-port
  (make-core-input-port #:name 'empty
                        #:read-in (lambda (bstr start-k end-k copy?) eof)
                        #:peek-in (lambda (bstr start-k end-k skip-k copy?) eof)
                        #:close void))
