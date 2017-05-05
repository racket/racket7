#lang racket/base
(provide (struct-out output-port)
         make-output-port)

(struct output-port (name
                     data
                     evt
                     write-out
                     close
                     write-out-special
                     get-write-evt
                     get-write-special-evt
                     get-location
                     count-lines!
                     buffer-mode
                     [closed? #:mutable]
                     [offset #:mutable] ; count plain bytes
                     [line #:mutable]   ; count newlines
                     [column #:mutable] ; count UTF-8 characters in line
                     [position #:mutable])  ; count UTF-8 characters
  #:property prop:object-name (struct-field-index name))

(define (make-output-port #:name name
                          #:data [data #f]
                          #:evt evt
                          #:write-out write-out
                          #:close close
                          #:write-out-special [write-out-special #f]
                          #:get-write-evt [get-write-evt #f]
                          #:get-write-special-evt [get-write-special-evt #f]
                          #:get-location [get-location #f]
                          #:count-lines! [count-lines! #f])
  (output-port name
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
               #f)) ; position
