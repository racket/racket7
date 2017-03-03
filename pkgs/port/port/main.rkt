#lang racket/base
(require (only-in "input-port.rkt"
                  input-port?)
         (only-in "output-port.rkt"
                  output-port?)
         "bytes-input.rkt"
         "string-input.rkt"
         "bytes-output.rkt"
         "string-output.rkt"
         "line-input.rkt"
         "file-port.rkt"
         "bytes-port.rkt"
         "string-port.rkt"
         "custom-input-port.rkt"
         "custom-output-port.rkt"
         "pipe.rkt"
         "close.rkt"
         "count.rkt"
         "parameter.rkt"
         "name.rkt")

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
         write-string
         
         read-line
         read-bytes-line
         
         make-input-port
         make-output-port
         
         input-port?
         output-port?
         
         open-input-file
         open-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file
         
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
         
         port-count-lines!
         port-next-location

         current-input-port
         current-output-port
         current-error-port
         
         port-name)
