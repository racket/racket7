#lang racket/base
(require (only-in "input-port.rkt"
                  input-port?
                  prop:input-port)
         (only-in "output-port.rkt"
                  output-port?
                  prop:output-port)
         "bytes-input.rkt"
         "string-input.rkt"
         "bytes-output.rkt"
         "string-output.rkt"
         "special-output.rkt"
         "line-input.rkt"
         "file-port.rkt"
         "file-stream.rkt"
         "bytes-port.rkt"
         "string-port.rkt"
         "custom-input-port.rkt"
         "custom-output-port.rkt"
         "handler.rkt"
         "pipe.rkt"
         "close.rkt"
         "count.rkt"
         "buffer-mode.rkt"
         "file-position.rkt"
         "file-truncate.rkt"
         "flush-output.rkt"
         "parameter.rkt")

(provide read-byte
         read-bytes
         read-bytes!
         read-bytes-avail!
         read-bytes-avail!*
         
         peek-byte
         peek-bytes
         peek-bytes!
         peek-bytes-avail!
         peek-bytes-avail!*
         
         read-char
         read-string
         read-string!
         
         peek-char
         peek-string
         peek-string!
         
         write-bytes
         write-bytes-avail
         write-bytes-avail*
         write-bytes-avail/enable-break
         write-bytes-avail-evt
         write-string
         port-writes-atomic?

         write-special
         write-special-evt
         port-writes-special?
         
         read-line
         read-bytes-line
         
         make-input-port
         make-output-port

         port-read-handler
         port-write-handler
         port-display-handler
         port-print-handler
         install-reader!

         prop:input-port
         prop:output-port
         input-port?
         output-port?
         
         open-input-file
         open-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file

         file-stream-port?

         open-input-bytes
         open-output-bytes
         get-output-bytes
         open-input-string
         open-output-string
         get-output-string
         string-port?
         
         make-pipe
         pipe-input-port?
         pipe-output-port?
         pipe-content-length
         
         close-input-port
         close-output-port
         
         file-stream-buffer-mode

         file-position
         file-position*
         file-truncate
         
         port-count-lines!
         port-next-location

         current-input-port
         current-output-port
         current-error-port

         flush-output)
