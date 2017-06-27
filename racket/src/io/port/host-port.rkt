#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/evt.rkt"
         "../sandman/main.rkt"
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

(struct host-data (host-port       ; host-system file descriptor
                   buffer-mode     ; gets/sets buffer mode
                   buffer-control) ; flushes on 0 arguments; adjusts position as given argument
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
                                      (define pos (rktio_filesize_ref ppos))
                                      (rktio_free ppos)
                                      (end-atomic)
                                      ((host-data-buffer-control hd) pos)])]
                                  [(hd pos)
                                   ((host-data-buffer-control hd))
                                   (check-rktio-error*
                                    (rktio_set_file_position rktio
                                                             (host-data-host-port hd)
                                                             (if (eof-object? pos)
                                                                 0
                                                                 pos)
                                                             (if (eof-object? pos)
                                                                 RKTIO_POSITION_FROM_END
                                                                 RKTIO_POSITION_FROM_START))
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
  (define-values (port buffer-control)
    (open-input-peek-via-read
     #:name name
     #:data (host-data host-in
                       (case-lambda
                         [() buffer-mode]
                         [(mode) (set! buffer-mode mode)])
                       (case-lambda
                         [() (buffer-control)]
                         [(pos) (buffer-control pos)]))
     #:read-in (lambda (dest-bstr start end copy?)
                 (define n (rktio_read_in rktio host-in dest-bstr start end))
                 (cond
                   [(rktio-error? n)
                    (raise-filesystem-error #f n "error reading from stream port")]
                   [(eqv? n RKTIO_READ_EOF) eof]
                   [(eqv? n 0) (fd-evt host-in RKTIO_POLL_READ)]
                   [else n]))
     #:get-buffer-mode (lambda () buffer-mode)
     #:close (lambda () (host-close host-in))))
  port)

;; ----------------------------------------

(define (open-output-host host-out name #:buffer-mode [buffer-mode 'infer])
  (define buffer (make-bytes 4096))
  (define buffer-start 0)
  (define buffer-end 0)
  (define flush-handle
    (plumber-add-flush! (current-plumber)
                        (lambda (h)
                          (flush-buffer-fully)
                          (plumber-flush-handle-remove! h))))
  
  (when (eq? buffer-mode 'infer)
    (if (rktio_fd_is_terminal rktio host-out)
        (set! buffer-mode 'line)
        (set! buffer-mode 'block)))

  ;; Returns `#t` if the buffer is already or successfully flushed
  (define (flush-buffer)
    (cond
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
             (set! buffer-end 0)
             #t]
            [else
             (set! buffer-start new-buffer-start)
             #f])])]
      [else #t]))

  (define (flush-buffer-fully)
    (let loop ()
      (unless (flush-buffer)
        (loop))))

  (define (flush-buffer-fully-if-newline src-bstr src-start src-end)
    (for ([b (in-bytes src-bstr src-start src-end)])
      (define newline? (or (eqv? b (char->integer #\newline))
                           (eqv? b (char->integer #\return))))
      (when newline? (flush-buffer-fully))
      #:break newline?
      (void)))

  (make-core-output-port
   #:name name
   #:data (host-data host-out
                     (case-lambda
                       [() buffer-mode]
                       [(mode) (set! buffer-mode mode)])
                     (case-lambda
                       [()
                        (flush-buffer-fully)]
                       [(pos)
                        (+ pos (- buffer-end buffer-start))]))

   #:evt 'evt
   
   #:write-out
   (lambda (src-bstr src-start src-end nonbuffer/nonblock? enable-break? copy?)
     (cond
       [(= src-start src-end)
        ;; Flush request
        (and (flush-buffer) 0)]
       [(and (not nonbuffer/nonblock?)
             (< buffer-end (bytes-length buffer)))
        (define amt (min (- src-end src-start) (- (bytes-length buffer) buffer-end)))
        (bytes-copy! buffer buffer-end src-bstr src-start (+ src-start amt))
        (set! buffer-end (+ buffer-end amt))
        (unless nonbuffer/nonblock?
          (when (eq? buffer-mode 'line)
            (flush-buffer-fully-if-newline src-bstr src-start src-end)))
        amt]
       [(not (flush-buffer))
        #f]
       [else
        (define n (rktio_write_in rktio host-out src-bstr src-start src-end))
        (cond
          [(rktio-error? n)
           (raise-filesystem-error #f n "error writing to stream port")]
          [else n])]))

   #:close (lambda ()
             (end-atomic)
             (flush-buffer-fully)
             (start-atomic)
             (plumber-flush-handle-remove! flush-handle)
             (host-close host-out))))

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

;; ----------------------------------------

(struct fd-evt (fd mode)
  #:property
  prop:evt
  (poller
   ;; This function is called by the scheduler for `sync` to check
   ;; whether the file descriptor has data available:
   (lambda (fde ctx)
     (define mode (fd-evt-mode fde))
     (start-atomic)
     (define ready?
       (or
        (and (eqv? RKTIO_POLL_READ (bitwise-and mode RKTIO_POLL_READ))
             (eqv? (rktio_poll_read_ready rktio (fd-evt-fd fde))
                   RKTIO_POLL_READY))
        (and (eqv? RKTIO_POLL_WRITE (bitwise-and mode RKTIO_POLL_WRITE))
             (eqv? (rktio_poll_write_ready rktio (fd-evt-fd fde))
                   RKTIO_POLL_READY))))
     (cond
       [ready?
        (end-atomic)
        (values (list fde) #f)]
       [else
        ;; If `sched-info` is not #f, then we can register this file
        ;; descriptor so that if no thread is able to make progress,
        ;; the Racket process will sleep, but it will wake up when
        ;; input is available. The implementation of external events
        ;; is from the current sandman, which will in turn be the
        ;; one (or build on the one) in "../sandman".
        (define sched-info (poll-ctx-sched-info ctx))
        (when sched-info
          (schedule-info-current-exts sched-info
                                      (sandman-add-poll-set-adder
                                       (schedule-info-current-exts sched-info)
                                       ;; Cooperate with the sandman by registering
                                       ;; a funciton that takes a poll set and
                                       ;; adds to it:
                                       (lambda (ps)
                                         (rktio_poll_add rktio (fd-evt-fd fde) ps mode)))))
        (end-atomic)
        (values #f fde)]))))
