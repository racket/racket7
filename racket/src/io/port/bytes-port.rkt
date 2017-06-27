#lang racket/base
(require "../common/check.rkt"
         "../common/atomic.rkt"
         "../host/evt.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "bytes-input.rkt"
         "count.rkt")

(provide open-input-bytes
         open-output-bytes
         get-output-bytes
         string-port?)

(struct input-bytes-data ())

(define/who (open-input-bytes bstr [name 'string])
  (check who bytes? bstr)
  (define i 0)
  (define len (bytes-length bstr))

  (define progress-sema #f)
  (define (progress!)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f)))

  (define p
    (make-core-input-port
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
          (progress!)
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

     #:close
     (lambda ()
       (progress!))

     #:get-progress-evt
     (lambda ()
       (atomically
        (unless progress-sema
          (set! progress-sema (make-semaphore)))
        (semaphore-peek-evt progress-sema)))

     #:commit
     (lambda (amt progress-evt ext-evt)
       ;; Very similar to the pipe implementation:
       (atomically
        (and (not (sync/timeout 0 progress-evt))
             (sync/timeout 0 ext-evt)
             (let ([amt (min amt (- len i))])
               (define dest-bstr (make-bytes amt))
               (bytes-copy! dest-bstr 0 bstr i (+ i amt))
               (set! i (+ i amt))
               (progress!)
               dest-bstr))))))

  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

;; ----------------------------------------

(struct output-bytes-data (i))

(define (open-output-bytes [name 'string])
  (define-values (i o) (make-pipe))
  (define p
    (make-core-output-port
     #:name name
     #:data (output-bytes-data i)
     #:evt o
     #:write-out (core-output-port-write-out o)
     #:close (core-output-port-close o)
     #:get-write-evt (core-output-port-get-write-evt o)
     #:get-location (core-output-port-get-location o)
     #:count-lines! (core-output-port-count-lines! o)))
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define/who (get-output-bytes o [reset? #f] [start-pos 0] [end-pos #f])
  (check who (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (let ([o (->core-output-port o)])
    (define i (output-bytes-data-i (core-output-port-data o)))
    (define len (pipe-content-length i))
    (define amt (- (min len (or end-pos len)) start-pos))
    (define bstr (make-bytes amt))
    (peek-bytes! bstr start-pos i)
    bstr))

;; ----------------------------------------

(define (string-port? p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (input-bytes-data? (core-input-port-data p)))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (output-bytes-data? (core-output-port-data p)))]
    [else
     (raise-argument-error 'string-port? "port?" p)]))
