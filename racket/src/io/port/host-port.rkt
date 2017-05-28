#lang racket/base
(require (only-in racket/base
                  [read-byte host:read-byte]
                  [peek-byte host:peek-byte]
                  [read-bytes-avail!* host:read-bytes-avail!*]
                  [write-bytes-avail* host:write-bytes-avail*]
                  [close-input-port host:close-input-port]
                  [close-output-port host:close-output-port]
                  [file-stream-buffer-mode host:file-stream-buffer-mode]
                  [file-position host:file-position]
                  [file-truncate host:file-truncate]
                  [flush-output host:flush-output]
                  [terminal-port? host:terminal-port?])
         "input-port.rkt"
         "output-port.rkt"
         "peek-to-read-port.rkt"
         "file-stream.rkt"
         "file-position.rkt"
         "file-truncate.rkt"
         "buffer-mode.rkt")

(provide open-input-host
         open-output-host
         terminal-port?)

(struct host-data (host-port)
  #:property prop:file-stream #t
  #:property prop:file-position (case-lambda
                                  [(hd)
                                   (host:file-position (host-data-host-port hd))]
                                  [(hd pos)
                                   (host:file-position (host-data-host-port hd) pos)])
  #:property prop:file-truncate (case-lambda
                                  [(hd pos)
                                   (host:file-truncate (host-data-host-port hd) pos)])
  #:property prop:buffer-mode (case-lambda
                                [(hd)
                                 (host:file-stream-buffer-mode (host-data-host-port hd))]
                                [(hd mode)
                                 (host:file-stream-buffer-mode (host-data-host-port hd) mode)]))

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
  (make-core-output-port
   #:name name
   #:data (host-data host-out)

   #:evt 'evt
   
   #:write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (cond
       [(= src-start src-end)
        ;; Flush request
        (host:flush-output host-out)
        0]
       [else
        (host:write-bytes-avail* src-bstr host-out src-start src-end)]))

   #:close
   (lambda ()
     (host:close-output-port host-out))))

;; ----------------------------------------

(define (terminal-port? p)
  (define data
    (cond
      [(input-port? p)
       (core-input-port-data (->core-input-port p))]
      [(output-port? p)
       (core-output-port-data (->core-output-port p))]
      [else
       (raise-argument-error 'terminal-port? "port?" p)]))
  (and (host-data? p)
       (host:terminal-port? (host-data-host-port p))))
