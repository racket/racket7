#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "bytes-input.rkt")

(provide open-input-bytes
         open-output-bytes
         get-output-bytes
         string-port?)

(struct input-bytes-data ())

(define (open-input-bytes bstr [name 'string])
  (check 'open-input-bytes bytes? bstr)
  (define i 0)
  (define len (bytes-length bstr))
  (make-input-port
   #:name name
   #:data (input-bytes-data)
   
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

;; ----------------------------------------

(struct output-bytes-data (i))

(define (open-output-bytes [name 'string])
  (define-values (i o) (make-pipe))
  (make-output-port
   #:name name
   #:data (output-bytes-data i)
   #:evt o
   #:write-out (output-port-write-out o)
   #:close (output-port-close o)
   #:get-write-evt (output-port-get-write-evt o)
   #:get-location (output-port-get-location o)
   #:count-lines! (output-port-count-lines! o)))

(define (get-output-bytes o [reset? #f] [start-pos 0] [end-pos #f])
  (check 'get-output-bytes (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (define i (output-bytes-data-i (output-port-data o)))
  (define len (pipe-content-length i))
  (define amt (- (min len (or end-pos len)) start-pos))
  (define bstr (make-bytes amt))
  (peek-bytes! bstr start-pos i)
  bstr)

;; ----------------------------------------

(define (string-port? p)
  (cond
   [(input-port? p)
    (input-bytes-data? (input-port-data p))]
   [(output-port? p)
    (output-bytes-data? (output-port-data p))]
   [else
    (raise-argument-error 'string-port? "port?" p)]))
