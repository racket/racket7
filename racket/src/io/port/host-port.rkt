#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/evt.rkt"
         "../file/error.rkt"
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
                                   (start-atomic)
                                   (define ppos (rktio_get_file_position rktio
                                                                         (host-data-host-port hd)))
                                   (cond
                                     [(rktio-error? ppos)
                                      (end-atomic)
                                      (check-rktio-error* ppos "error getting stream position")]
                                     [else
                                      (define pos (rktio_filesize_ref pos))
                                      (rktio_free ppos)
                                      (end-atomic)
                                      pos])]
                                  [(hd pos)
                                   (check-rktio-error*
                                    (rktio_set_file_position rktio
                                                             (host-data-host-port hd)
                                                             pos)
                                    "error setting stream position")])
  #:property prop:file-truncate (case-lambda
                                  [(hd pos)
                                   (check-rktio-error*
                                    (rktio_set_file_size rktio
                                                         (host-data-host-port hd)
                                                         pos)
                                    "error setting file size")])
  #:property prop:buffer-mode (case-lambda
                                [(hd) 'none]
                                [(hd mode) (void)]))

(define (host-close host-port)
  (check-rktio-error*
   (rktio_close rktio host-port)
   "error closing stream port"))

;; ----------------------------------------

(define (open-input-host host-in name)
  (open-input-peek-to-read
   #:name name
   #:data (host-data host-in)
   #:read-byte (and
                (rktio_fd_is_regular_file rktio host-in)
                (lambda ()
                  (define bstr (make-bytes 1))
                  (let loop ()
                    (define n (rktio_read_in rktio host-in bstr 0 1))
                    (cond
                      [(rktio-error? n)
                       (raise-filesystem-error #f n "error reading from stream port")]
                      [(eqv? n RKTIO_READ_EOF) eof]
                      [(zero? n) (loop)] ; regular file should have bytes soon
                      [else (bytes-ref bstr 0)]))))
   #:read-in (lambda (dest-bstr start end copy?)
               (define n (rktio_read_in rktio host-in dest-bstr start end))
               (cond
                 [(rktio-error? n)
                  (raise-filesystem-error #f n "error reading from stream port")]
                 [(eqv? n RKTIO_READ_EOF) eof]
                 [else n]))
   #:close (lambda () (host-close host-in))))

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
        0]
       [else
        (define n (rktio_write_in rktio host-out src-bstr src-start src-end))
        (cond
          [(rktio-error? n)
           (raise-filesystem-error #f n "error writing to stream port")]
          [else n])]))

   #:close (lambda () (host-close host-out))))

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
       (rktio_fd_is_terminal (host-data-host-port p))))
