#lang racket/base
(require "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt")

(provide open-input-peek-to-read)

(define (open-input-peek-to-read #:name name
                                 #:data [data #f]
                                 #:read-byte read-byte
                                 #:read-in read-in
                                 #:peek-byte [peek-byte #f]
                                 #:close close)
  (define peek-pipe-i #f)
  (define peek-pipe-o #f)
  (define peeked-eof? #f)
  (define buf (make-bytes 4096))
  (make-core-input-port
   #:name name
   #:data data
   
   #:read-byte
   (lambda ()
     (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((core-input-port-read-byte peek-pipe-i))]
      [peeked-eof?
       (set! peeked-eof? #f)
       eof]
      [else
       (read-byte)]))
   
   #:read-in
   (lambda (dest-bstr start end copy?)
     (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((core-input-port-read-in peek-pipe-i) dest-bstr start end copy?)]
      [peeked-eof?
       (set! peeked-eof? #f)
       eof]
      [else
       (read-in dest-bstr start end copy?)]))
   
   #:peek-byte
   (and peek-byte
        (lambda ()
          (cond
           [(and peek-pipe-i
                 (positive? (pipe-content-length peek-pipe-i)))
            ((core-input-port-peek-byte peek-pipe-i))]
           [else
            (peek-byte)])))
   
   #:peek-in
   (lambda (dest-bstr start end skip copy?)
     (let try-again ()
       (define peeked-amt (if peek-pipe-i
                              (pipe-content-length peek-pipe-i)
                              0))
       (cond
        [(and peek-pipe-i
              (peeked-amt . > . skip))
         ((core-input-port-peek-in peek-pipe-i) dest-bstr start end skip copy?)]
        [peeked-eof? eof]
        [else
         (when (not peek-pipe-i)
           (set!-values (peek-pipe-i peek-pipe-o) (make-pipe)))
         (define v (read-in buf 0 (bytes-length buf) #f))
         (cond
          [(eof-object? v)
           (set! peeked-eof? #t)
           eof]
          [(zero? v) 0]
          [else
           (let loop ([wrote 0])
             (define just-wrote ((core-output-port-write-out peek-pipe-o) buf wrote v #t #f))
             (define next-wrote (+ wrote just-wrote))
             (unless (= v next-wrote)
               (loop next-wrote)))
           (try-again)])])))

   #:close
   (lambda ()
     (close)
     (set! peek-pipe-i #f)
     (set! peek-pipe-o #f))))
