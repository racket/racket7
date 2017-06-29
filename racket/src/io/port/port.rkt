#lang racket/base

(provide (struct-out core-port))

(struct core-port (name      ; anything, reported as `object-name` for the port
                   data      ; anything, effectively a subtype indicator

                   close     ; -> (void)
                   ;;          Called in atomic mode.

                   count-lines!  ; #f or procedure
                   get-location  ; #f or procedure
                   file-position ; #f, port, or procedure
                   buffer-mode   ; #f or procedure

                   [closed? #:mutable]
                   [closed-sema #:mutable] ; #f or a semaphore posed on close

                   [offset #:mutable] ; count plain bytes
                   [state #:mutable] ; state of UTF-8 decoding
                   [cr-state #:mutable] ; state of CRLF counting as a single LF
                   [line #:mutable]   ; count newlines
                   [column #:mutable] ; count UTF-8 characters in line
                   [position #:mutable]) ; count UTF-8 characters
  #:property prop:object-name (struct-field-index name))
