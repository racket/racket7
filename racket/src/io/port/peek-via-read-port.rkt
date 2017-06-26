#lang racket/base
(require "../host/evt.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt")

(provide open-input-peek-via-read)

(define (open-input-peek-via-read #:name name
                                  #:data [data #f]
                                  #:get-buffer-mode [get-buffer-mode (lambda () 'block)]
                                  #:read-in read-in
                                  #:close close)
  (define peek-pipe-i #f)
  (define peek-pipe-o #f)
  (define peeked-eof? #f)
  (define buf (make-bytes 4096))

  (define (pull-some-bytes [amt (bytes-length buf)])
    (when (not peek-pipe-i)
      (set!-values (peek-pipe-i peek-pipe-o) (make-pipe)))
    (define v (read-in buf 0 amt #f))
    (cond
      [(eof-object? v)
       (set! peeked-eof? #t)
       eof]
      [(evt? v) v]
      [(eqv? v 0) 0]
      [else
       (let loop ([wrote 0])
         (define just-wrote ((core-output-port-write-out peek-pipe-o) buf wrote v #t #f #f))
         (define next-wrote (+ wrote just-wrote))
         (unless (= v next-wrote)
           (loop next-wrote)))
       v]))

  (define (get-more-bytes/block)
    (define v (pull-some-bytes (if (eq? 'block (get-buffer-mode))
                                   (bytes-length buf)
                                   1)))
    (cond
      [(eqv? v 0)
       (get-more-bytes/block)]
      [(evt? v)
       (sync v)
       (get-more-bytes/block)]))

  (define (do-read-in dest-bstr start end copy?)
    (let try-again ()
      (cond
        [(and peek-pipe-i
              (positive? (pipe-content-length peek-pipe-i)))
         ((core-input-port-read-in peek-pipe-i) dest-bstr start end copy?)]
        [peeked-eof?
         (set! peeked-eof? #f)
         eof]
        [else
         (cond
           [(and (< (- end start) (bytes-length buf))
                 (eq? 'block (get-buffer-mode)))
            (define v (pull-some-bytes))
            (cond
              [(or (eqv? v 0) (evt? v)) v]
              [else (try-again)])]
           [else
            (read-in dest-bstr start end copy?)])])))

  (define (read-byte)
    (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((core-input-port-read-byte peek-pipe-i))]
      [peeked-eof?
       (set! peeked-eof? #f)
       eof]
      [else
       (get-more-bytes/block)
       (read-byte)]))

  (define (do-peek-in dest-bstr start end skip copy?)
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
         (define v (pull-some-bytes))
         (cond
           [(or (eof-object? v) (eqv? v 0) (evt? v))
            v]
           [else (try-again)])])))

  (define (peek-byte)
    (cond
      [(and peek-pipe-i
            (positive? (pipe-content-length peek-pipe-i)))
       ((core-input-port-peek-byte peek-pipe-i))]
      [peeked-eof? eof]
      [else
       (get-more-bytes/block)
       (peek-byte)]))

  (define (purge-buffer)
    (set! peek-pipe-i #f)
    (set! peek-pipe-o #f)
    (set! peeked-eof? #f))

  (values (make-core-input-port
           #:name name
           #:data data
           
           #:read-byte read-byte
           #:read-in do-read-in
           #:peek-byte peek-byte
           #:peek-in do-peek-in

           #:on-file-position
           (lambda ()
             (set! peek-pipe-i #f)
             (set! peek-pipe-o #f)
             (set! peeked-eof? #f))

           #:close
           (lambda ()
             (close)
             (set! peek-pipe-i #f)
             (set! peek-pipe-o #f)))

          (case-lambda
            [() (purge-buffer)]
            [(pos) (if peek-pipe-i
                       (- pos (pipe-content-length peek-pipe-i))
                       pos)])))
