#lang racket/base
(require (only-in racket/base
                  [read-byte host:read-byte]
                  [peek-byte host:peek-byte]
                  [read-bytes-avail!* host:read-bytes-avail!*]
                  [write-bytes-avail* host:write-bytes-avail*]
                  [close-input-port host:close-input-port]
                  [close-output-port host:close-output-port]
                  [file-stream-buffer-mode host:file-stream-buffer-mode])
         "output-port.rkt"
         "peek-to-read-port.rkt"
         "file-stream.rkt"
         "buffer-mode.rkt")

(provide open-input-host
         open-output-host)

(struct host-data (host-port)
  #:property prop:file-stream #t
  #:property prop:buffer-mode (case-lambda
                                [(hid)
                                 (host:file-stream-buffer-mode (host-data-host-port hid))]
                                [(hid mode)
                                 (host:file-stream-buffer-mode (host-data-host-port hid) mode)]))


(define (open-input-host host-in name)
  (open-input-peek-to-read
   #:name name
   #:data (host-data host-in)
   #:read-byte (lambda () (host:read-byte host-in))
   #:read-in (lambda (dest-bstr start end copy?)
               (host:read-bytes-avail!* dest-bstr host-in start end))
   #:peek-byte (lambda () (host:peek-byte host-in))
   #:close (lambda () (host:close-input-port host-in))))

;; ----------------------------------------

(define (open-output-host host-out name)
  (make-output-port
   #:name name
   #:data (host-data host-out)

   #:evt 'evt
   
   #:write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break?)
     (host:write-bytes-avail* src-bstr host-out src-start src-end))

   #:close
   (lambda ()
     (host:close-output-port host-out))))
