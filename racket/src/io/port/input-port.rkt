#lang racket/base
(provide (struct-out input-port)
         make-input-port)

(struct input-port (name
                    data
                    read-byte ; #f or (-> (or/c byte? eof-object?))
                    read-in ; port or (bytes start-k end-k copy? -> (or/c integer? ...))
                    peek-byte ; #f or (-> (or/c byte? eof-object?))
                    peek-in ; port or (bytes start-k end-k skip-k copy? -> (or/c integer? ...))
                    close
                    get-progress-evt
                    commit
                    get-location
                    count-lines!
                    [closed? #:mutable]
                    [offset #:mutable] ; count plain bytes
                    [state #:mutable] ; state of UTF-8 decoding
                    [cr-state #:mutable] ; state of CRLF counting as a single LF
                    [line #:mutable]   ; count newlines
                    [column #:mutable] ; count UTF-8 characters in line
                    [position #:mutable]    ; count UTF-8 characters
                    [pending-eof? #:mutable]))

(define (make-input-port #:name name
                         #:data [data #f]
                         #:read-byte [read-byte #f]
                         #:read-in read-in
                         #:peek-byte [peek-byte #f]
                         #:peek-in peek-in
                         #:close close
                         #:get-progress-evt [get-progress-evt #f]
                         #:commit [commit #f]
                         #:get-location [get-location #f]
                         #:count-lines! [count-lines! #f])
  (input-port name
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
              #f   ; closed?
              0    ; offset
              #f   ; state
              #f   ; cr-state
              #f   ; line
              #f   ; column
              #f   ; position
              #f)) ; pending-eof?
