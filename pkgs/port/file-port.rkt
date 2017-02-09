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
  (define peek-pipe-i #f)
  (define peek-pipe-o #f)
  (define peeked-eof? #f)
  (define buf (make-bytes 4096))
  (make-input-port
   #:name path
   
   #:read-byte
   (lambda ()
     (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((input-port-read-byte peek-pipe-i))]
      [peeked-eof?
       (set! peeked-eof? #f)
       eof]
      [else
       (host:read-byte host-in)]))
   
   #:read-in
   (lambda (dest-bstr start end copy?)
     (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((input-port-read-in peek-pipe-i) dest-bstr start end copy?)]
      [peeked-eof?
       (set! peeked-eof? #f)
       eof]
      [else
       (host:read-bytes-avail!* dest-bstr host-in start end)]))
   
   #:peek-byte
   (lambda ()
     (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((input-port-peek-byte peek-pipe-i))]
      [else
       (host:peek-byte host-in)]))
   
   #:peek-in
   (lambda (dest-bstr start end skip copy?)
     (let try-again ()
       (define peeked-amt (if peek-pipe-i
                              (pipe-content-length peek-pipe-i)
                              0))
       (cond
        [(and peek-pipe-i
              (peeked-amt . > . skip))
         ((input-port-peek-in peek-pipe-i) dest-bstr start end skip copy?)]
        [peeked-eof? eof]
        [else
         (when (not peek-pipe-i)
           (set!-values (peek-pipe-i peek-pipe-o) (make-pipe)))
         (define v (host:read-bytes-avail!* buf host-in 0 (bytes-length buf)))
         (cond
          [(eof-object? v)
           (set! peeked-eof? #t)
           eof]
          [(zero? v) 0]
          [else
           ((output-port-write-out peek-pipe-o) buf 0 v #t #f)
           (try-again)])])))

   #:close
   (lambda ()
     (host:close-input-port host-in)
     (set! peek-pipe-i #f)
     (set! peek-pipe-o #f))))
