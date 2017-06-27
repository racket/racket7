#lang racket/base
(require "../common/check.rkt"
         "../host/evt.rkt"
         "port.rkt")

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

(struct core-output-port core-port
  (evt
   write-out ; (bstr start end no-buffer? enable-break? copy? -> ...)
   write-out-special ; (any no-buffer? enable-break? -> ...)
   get-write-evt
   get-write-special-evt
   [write-handler #:mutable]
   [print-handler #:mutable]
   [display-handler #:mutable])
  #:property prop:evt (lambda (o) (wrap-evt (core-output-port-evt o)
                                            (lambda (v) o))))

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

                    close
                    count-lines!
                    get-location
                    void ; on-file-position
                    
                    #f   ; closed?
                    #f   ; closed-sema
                    0    ; offset
                    #f   ; state
                    #f   ; cr-state
                    #f   ; line
                    #f   ; column
                    #f   ; position
                    
                    evt
                    write-out
                    write-out-special
                    get-write-evt
                    get-write-special-evt

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
