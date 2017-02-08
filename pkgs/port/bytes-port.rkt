#lang racket/base
(require "input-port.rkt"
         "check.rkt")

(provide open-input-bytes)

(define (open-input-bytes bstr [name 'string])
  (check 'open-input-bytes bytes? bstr)
  (define i 0)
  (define len (bytes-length bstr))
  (make-input-port
   #:name name
   
   #:read-byte
   (lambda ()
     (let ([pos i])
       (if (pos . < . len)
           (begin
             (set! i (add1 pos))
             (bytes-ref bstr pos))
           eof)))
   
   #:read-in
   (lambda (dest-bstr start end copy?)
     (define pos i)
     (cond
      [(pos . < . len)
       (define amt (min (- end start) (- len pos)))
       (set! i (+ pos amt))
       (bytes-copy! dest-bstr start bstr pos (+ pos amt))
       amt]
      [else eof]))
   
   #:peek-byte
   (lambda ()
     (let ([pos i])
       (if (pos . < . len)
           (bytes-ref bstr pos)
           eof)))
   
   #:peek-in
   (lambda (dest-bstr start end skip copy?)
     (define pos (+ i skip))
     (cond
      [(pos . < . len)
       (define amt (min (- end start) (- len pos)))
       (bytes-copy! dest-bstr start bstr pos (+ pos amt))
       amt]
      [else eof]))

   #:close void))
