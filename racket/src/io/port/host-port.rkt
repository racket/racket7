#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/evt.rkt"
         "../file/error.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "peek-via-read-port.rkt"
         "file-stream.rkt"
         "file-position.rkt"
         "file-truncate.rkt"
         "buffer-mode.rkt")

(provide open-input-host
         open-output-host
         terminal-port?)

(struct host-data (host-port buffer-mode)
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
                                [(hd) ((host-data-buffer-mode hd))]
                                [(hd mode) ((host-data-buffer-mode hd) mode)]))

(define (host-close host-port)
  (check-rktio-error*
   (rktio_close rktio host-port)
   "error closing stream port"))

;; ----------------------------------------

(define (open-input-host host-in name)
  (define buffer-mode 'block)
  (open-input-peek-via-read
   #:name name
   #:data (host-data host-in (case-lambda
                               [() buffer-mode]
                               [(mode) (set! buffer-mode mode)]))
   #:read-in (lambda (dest-bstr start end copy?)
               (define n (rktio_read_in rktio host-in dest-bstr start end))
               (cond
                 [(rktio-error? n)
                  (raise-filesystem-error #f n "error reading from stream port")]
                 [(eqv? n RKTIO_READ_EOF) eof]
                 [else n]))
   #:get-buffer-mode (lambda () buffer-mode)
   #:close (lambda () (host-close host-in))))

;; ----------------------------------------

(define (open-output-host host-out name #:buffer-mode [buffer-mode 'infer])
  (define buffer (make-bytes 4096))
  (define buffer-start 0)
  (define buffer-end 0)
  
  (when (eq? buffer-mode 'infer)
    (if (rktio_fd_is_terminal rktio host-out)
        (set! buffer-mode 'line)
        (set! buffer-mode 'block)))
  
  (make-core-output-port
   #:name name
   #:data (host-data host-out (case-lambda
                                [() buffer-mode]
                                [(mode) (set! buffer-mode mode)]))

   #:evt 'evt
   
   #:write-out
   (lambda (src-bstr src-start src-end nonbuffer/nonblock? enable-break? copy?)
     (cond
       [(= src-start src-end)
        ;; Flush request
        0]
       [(and (not nonbuffer/nonblock?)
             (< buffer-end (bytes-length buffer)))
        (define amt (min (- src-end src-start) (- (bytes-length buffer) buffer-end)))
        (bytes-copy! buffer 0 src-bstr src-start (+ src-start amt))
        (set! buffer-end (+ buffer-end amt))
        amt]
       [(not (= buffer-start buffer-end))
        (define n (rktio_write_in rktio host-out buffer buffer-start buffer-end))
        (cond
          [(rktio-error? n)
           (raise-filesystem-error #f n "error writing to stream port")]
          [(zero? n)
           #f]
          [else
           (define new-buffer-start (+ buffer-start n))
           (cond
             [(= new-buffer-start buffer-end)
              (set! buffer-start 0)
              (set! buffer-end 0)]
             [else
              (set! buffer-start new-buffer-start)])
           #f])]
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
