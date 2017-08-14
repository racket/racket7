#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "port.rkt"
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
  (define alt-pos #f)
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
               (progress!)
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
     (lambda (dest-bstr start end skip progress-evt copy?)
       (define pos (+ i skip))
       (cond
         [(and progress-evt (sync/timeout 0 progress-evt))
          #f]
         [(pos . < . len)
          (define amt (min (- end start) (- len pos)))
          (bytes-copy! dest-bstr start bstr pos (+ pos amt))
          amt]
         [else eof]))

     #:byte-ready
     (lambda (work-done!)
       (i . < . len))

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
               dest-bstr))))

     #:file-position
     (case-lambda
       [() (or alt-pos i)]
       [(new-pos)
        (set! i (if (eof-object? new-pos)
                    len
                    (min len new-pos)))
        (set! alt-pos
              (and new-pos
                   (not (eof-object? new-pos))
                   (new-pos . > . i)
                   new-pos))])))

  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

;; ----------------------------------------

(struct output-bytes-data (i reset))

(define (open-output-bytes [name 'string])
  (define-values (i o) (make-pipe))
  (define p
    (make-core-output-port
     #:name name
     #:data (output-bytes-data i (lambda () (pipe-discard-all i)))
     #:evt o
     #:write-out (core-output-port-write-out o)
     #:close (core-port-close o)
     #:get-write-evt (core-output-port-get-write-evt o)
     #:get-location (core-port-get-location o)
     #:count-lines! (core-port-count-lines! o)
     #:file-position
     (case-lambda
       [() (pipe-write-position o)]
       [(new-pos)
        (define len (pipe-content-length i))
        (cond
          [(eof-object? new-pos)
           (pipe-write-position o len)]
          [(new-pos . > . len)
           (when (new-pos . >= . (expt 2 48))
             ;; implausibly large
             (end-atomic)
             (raise-arguments-error 'file-position
                                    "new position is too large"
                                    "port" p
                                    "position" new-pos))
           (pipe-write-position o len)
           (define amt (- new-pos len))
           ((core-output-port-write-out o) (make-bytes amt 0) 0 amt #f #f #f)
           (void)]
          [else
           (pipe-write-position o new-pos)])])))
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define/who (get-output-bytes o [reset? #f] [start-pos 0] [end-pos #f])
  (check who (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? #:or-false end-pos)
  (let ([o (->core-output-port o)])
    (define i (output-bytes-data-i (core-port-data o)))
    (define len (pipe-content-length i))
    (when (start-pos . > . len)
      (raise-range-error who "port content" "starting " start-pos o 0 len #f))
    (when end-pos
      (unless (<= start-pos end-pos len)
        (raise-range-error who "port content" "ending " end-pos o 0 len start-pos)))
    (define amt (- (min len (or end-pos len)) start-pos))
    (define bstr (make-bytes amt))
    (peek-bytes! bstr start-pos i)
    (when reset?
      ((output-bytes-data-reset (core-port-data o))))
    bstr))

;; ----------------------------------------

(define (string-port? p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (input-bytes-data? (core-port-data p)))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (output-bytes-data? (core-port-data p)))]
    [else
     (raise-argument-error 'string-port? "port?" p)]))
