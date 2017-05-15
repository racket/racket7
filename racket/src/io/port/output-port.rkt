#lang racket/base
(require "../common/check.rkt")

(provide prop:output-port
         output-port?
         ->core-output-port
         (struct-out core-output-port)
         make-core-output-port)

(define-values (prop:output-port output-port-via-property? output-port-ref)
  (make-struct-type-property 'output-port
                             (lambda (v sti)
                               (check 'prop:output-port (lambda (v) (or (exact-nonnegative-integer? v)
                                                                        (output-port? v)))
                                      #:contract "(or/c output-port? exact-nonnegative-integer?)"
                                      v)
                               (check-immutable-field 'prop:output-port v sti)
                               (if (exact-nonnegative-integer? v)
                                   (make-struct-field-accessor (list-ref sti 3) v)
                                   v))))

(define (output-port? p)
  (or (core-output-port? p)
      (output-port-via-property? p)))

(define (->core-output-port v)
  (cond
    [(core-output-port? v) v]
    [(output-port? v)
     (let ([p (output-port-ref v)])
       (cond
         [(struct-accessor-procedure? p)
          (->core-output-port (p v))]
         [else
          (->core-output-port p)]))]
    [else
     empty-output-port]))

(struct core-output-port (name
                          data
                          evt
                          write-out ; (bstr start end no-buffer? enable-break? -> ...)
                          close
                          write-out-special ; (any no-buffer? enable-break? -> ...)
                          get-write-evt
                          get-write-special-evt
                          get-location
                          count-lines!
                          buffer-mode
                          [closed? #:mutable]
                          [offset #:mutable] ; count plain bytes
                          [line #:mutable]   ; count newlines
                          [column #:mutable] ; count UTF-8 characters in line
                          [position #:mutable]  ; count UTF-8 characters
                          [write-handler #:mutable]
                          [print-handler #:mutable]
                          [display-handler #:mutable])
  #:property prop:object-name (struct-field-index name))

(define (make-core-output-port #:name name
                               #:data [data #f]
                               #:evt evt
                               #:write-out write-out
                               #:close close
                               #:write-out-special [write-out-special #f]
                               #:get-write-evt [get-write-evt #f]
                               #:get-write-special-evt [get-write-special-evt #f]
                               #:get-location [get-location #f]
                               #:count-lines! [count-lines! #f])
  (core-output-port name
                    data
                    evt
                    write-out
                    close
                    write-out-special
                    get-write-evt
                    get-write-special-evt
                    get-location
                    count-lines!
                    'block ; buffer-mode
                    #f   ; closed?
                    0    ; offset
                    #f   ; line
                    #f   ; column
                    #f   ; position
                    #f   ; write-handler
                    #f   ; display-handler
                    #f)) ; print-handler

(define empty-output-port
  (make-core-output-port #:name 'empty
                         #:evt always-evt
                         #:write-out (lambda (bstr start end no-buffer? enable-break?)
                                       (- end start))
                         #:write-out-special (lambda (v no-buffer? enable-break?)
                                               #t)
                         #:close void))
