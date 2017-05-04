#lang racket/base
(require (only-in racket/base
                  [read-byte host:read-byte]
                  [peek-byte host:peek-byte]
                  [read-bytes-avail!* host:read-bytes-avail!*]
                  [write-bytes-avail* host:write-bytes-avail*]
                  [close-input-port host:close-input-port]
                  [close-output-port host:close-output-port])
         "output-port.rkt"
         "peek-to-read-port.rkt")

(provide open-input-host
         open-output-host)

(define (open-input-host host-in name)
  (open-input-peek-to-read
   #:name name
   #:read-byte (lambda () (host:read-byte host-in))
   #:read-in (lambda (dest-bstr start end copy?)
               (host:read-bytes-avail!* dest-bstr host-in start end))
   #:peek-byte (lambda () (host:peek-byte host-in))
   #:close (lambda () (host:close-input-port host-in))))

;; ----------------------------------------

(define (open-output-host host-out name)
  (make-output-port
   #:name name
   
   #:evt 'evt
   
   #:write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break?)
     (host:write-bytes-avail* src-bstr host-out src-start src-end))

   #:close
   (lambda ()
     (host:close-output-port host-out))))
