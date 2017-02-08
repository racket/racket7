#lang racket/base
(require (only-in racket/base
                  [open-input-file host:open-input-file]
                  [read-byte host:read-byte]
                  [peek-byte host:peek-byte]
                  [read-bytes-avail!* host:read-bytes-avail!*]
                  [close-input-port host:close-input-port])
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "check.rkt")

(provide open-input-file)

(define none (gensym))

(define (open-input-file path [mode1 none] [mode2 none])
  (check 'open-input-file path-string? path)
  (define host-in (host:open-input-file (path->string (path->complete-path path))))
  (define peek-pipe #f)
  (define buf (make-bytes 4096))
  (make-input-port
   #:name path
   
   #:read-byte
   (lambda ()
     (cond
      [(and peek-pipe
            (positive? (pipe-content-length peek-pipe)))
       ((input-port-read-byte peek-pipe))]
      [else
       (host:read-byte host-in)]))
   
   #:read-in
   (lambda (dest-bstr start end copy?)
     (cond
      [(and peek-pipe
            (positive? (pipe-content-length peek-pipe)))
       ((input-port-read-in peek-pipe) dest-bstr start end copy?)]
      [else
       (host:read-bytes-avail!* dest-bstr host-in start end)]))
   
   #:peek-byte
   (lambda ()
     (cond
      [(and peek-pipe
            (positive? (pipe-content-length peek-pipe)))
       ((input-port-peek-byte peek-pipe))]
      [else
       (host:peek-byte host-in)]))
   
   #:peek-in
   (lambda (dest-bstr start end skip copy?)
     (let try-again ()
       (define peeked-amt (if peek-pipe
                              (pipe-content-length peek-pipe)
                              0))
       (cond
        [(and peek-pipe
              (peeked-amt . > . skip))
         ((input-port-read-in peek-pipe) dest-bstr start end skip copy?)]
        [else
         (when (not peek-pipe)
           (set! peek-pipe (make-pipe)))
         (define v (host:read-bytes-avail!* host-in buf 0 (bytes-length buf)))
         (unless (zero? v)
           ((output-port-write-out peek-pipe) buf 0 v #t #f))
         (try-again)])))
   
   #:close
   (lambda ()
     (host:close-input-port host-in)
     (set! peek-pipe #f))))
