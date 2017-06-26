#lang racket/base
(require "../common/check.rkt"
         "../host/evt.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "count.rkt")

(provide make-pipe
         pipe-input-port?
         pipe-output-port?
         pipe-content-length)

(define (min+1 a b) (if a (min (add1 a) b) b))

(struct pipe-data (get-content-length))

(define (pipe-input-port? p)
  (and (input-port? p)
       (pipe-data? (core-input-port-data (->core-input-port p)))))

(define (pipe-output-port? p)
  (and (output-port? p)
       (pipe-data? (core-output-port-data (->core-output-port p)))))

(define (pipe-content-length p)
  (cond
   [(pipe-input-port? p)
    ((pipe-data-get-content-length (core-input-port-data (->core-input-port p))))]
   [(pipe-output-port? p)
    ((pipe-data-get-content-length (core-output-port-data (->core-input-port p))))]
   [else
    (raise-argument-error 'pipe-contact-length "(or/c pipe-input-port? pipe-output-port?)" p)]))

(define/who (make-pipe [limit #f] [input-name 'pipe] [output-name 'pipe])
  (check who #:or-false exact-positive-integer? limit)
  (define bstr (make-bytes (min+1 limit 16)))
  (define start 0)
  (define end 0)
  (define output-closed? #f)
  (define data
    (pipe-data
     (lambda ()
       (if (start . <= . end)
           (- end start)
           (+ end (- (bytes-length bstr) start))))))

  (define read-ready-sema (make-semaphore))
  (define write-ready-sema (and limit (make-semaphore 1)))
  (define more-read-ready-sema #f) ; for lookahead peeks
  (define read-ready-evt (semaphore-peek-evt read-ready-sema))
  (define write-ready-evt (and limit (semaphore-peek-evt write-ready-sema)))
  (define progress-sema #f)

  (define (content-length)
    (if (start . <= . end)
        (- end start)
        (+ end (- (bytes-length bstr) start))))
  (define (input-empty?) (= start end))
  (define (output-full?) (and limit
                              (let ([len (bytes-length bstr)])
                                (and (limit . < . len)
                                     (or (and (zero? start)
                                              (= end (sub1 len)))
                                         (= end (sub1 start)))))))

  ;; Used before/after read:
  (define (check-output-unblocking)
    (when (output-full?) (semaphore-post write-ready-sema)))
  (define (check-input-blocking)
    (when (input-empty?) (semaphore-wait read-ready-sema)))

  ;; Used before/after write:
  (define (check-input-unblocking)
    (when (and (input-empty?) (not output-closed?)) (semaphore-post read-ready-sema)))
  (define (check-output-blocking)
    (when (output-full?) (semaphore-wait write-ready-sema)))

  (define (progress!)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f)))

  ;; input ----------------------------------------
  (define ip
    (make-core-input-port
     #:name input-name
     #:data data
     
     #:read-byte
     (lambda ()
       (let try-again ()
         (start-atomic)
         (cond
           [(input-empty?)
            (define done? output-closed?)
            (end-atomic)
            (if done?
                eof
                (begin
                  (sync read-ready-evt)
                  (try-again)))]
           [else
            (define pos start)
            (check-output-unblocking)
            (set! start (modulo (add1 pos) (bytes-length bstr)))
            (check-input-blocking)
            (progress!)
            (begin0
              (bytes-ref bstr pos)
              (end-atomic))])))

     #:read-in
     (lambda (dest-bstr dest-start dest-end copy?)
       (start-atomic)
       (cond
         [(input-empty?)
          (define done? output-closed?)
          (end-atomic)
          (if done?
              eof
              read-ready-evt)]
         [else
          (check-output-unblocking)
          (begin0
            (cond
              [(start . < . end)
               (define amt (min (- dest-end dest-start)
                                (- end start)))
               (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
               (set! start (+ start amt))
               amt]
              [else
               (define len (bytes-length bstr))
               (define amt (min (- dest-end dest-start)
                                (- len start)))
               (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
               (set! start (modulo (+ start amt) len))
               amt])
            (check-input-blocking)
            (progress!)
            (end-atomic))]))

     #:peek-byte
     (lambda ()
       (let try-again ()
         (start-atomic)
         (cond
           [(input-empty?)
            (define done? output-closed?)
            (end-atomic)
            (if done?
                eof
                (begin
                  (sync read-ready-evt)
                  (try-again)))]
           [else
            (begin0
              (bytes-ref bstr start)
              (end-atomic))])))
     
     #:peek-in
     (lambda (dest-bstr dest-start dest-end skip copy?)
       (start-atomic)
       (define content-amt (content-length))
       (cond
         [(content-amt . <= . skip)
          (cond
            [output-closed?
             (end-atomic)
             eof]
            [else
             (unless (or (zero? skip) more-read-ready-sema)
               (set! more-read-ready-sema (make-semaphore)))
             (define evt (if (zero? skip)
                             read-ready-evt
                             (semaphore-peek-evt more-read-ready-sema)))
             (end-atomic)
             evt])]
         [else
          (define len (bytes-length bstr))
          (define peek-start (modulo (+ start skip) len))
          (begin0
            (cond
              [(peek-start . < . end)
               (define amt (min (- dest-end dest-start)
                                (- end peek-start)))
               (bytes-copy! dest-bstr dest-start bstr peek-start (+ peek-start amt))
               amt]
              [else
               (define amt (min (- dest-end dest-start)
                                (- len peek-start)))
               (bytes-copy! dest-bstr dest-start bstr peek-start (+ peek-start amt))
               amt])
            (end-atomic))]))
     
     #:close
     void

     #:get-progress-evt
     (lambda ()
       (start-atomic)
       (unless progress-sema
         (set! progress-sema (make-semaphore)))
       (define sema progress-sema)
       (end-atomic)
       (semaphore-peek-evt progress-sema))

     #:commit
     (lambda (amt progress-evt ext-evt)
       ;; `progress-evt` is a `semepahore-peek-evt`, and `ext-evt`
       ;; is constrained; both can work with `sync/timeout` in
       ;; atomic mode.
       (start-atomic)
       (begin0
         (and (not (sync/timeout 0 progress-evt))
              (sync/timeout 0 ext-evt)
              (let ([amt (min amt (content-length))])
                (set! start (modulo (+ start amt) (bytes-length bstr)))
                (progress!)
                (check-input-blocking)
                #t))
         (end-atomic)))))
    
  ;; out ----------------------------------------
  (define op
    (make-core-output-port
     #:name output-name
     #:data data

     #:evt write-ready-evt
     
     #:write-out
     (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
       (let try-again ()
         (start-atomic)
         (define len (bytes-length bstr))
         (define top-pos (if (zero? start)
                             (sub1 len)
                             len))
         (define (maybe-grow)
           (cond
             [(or (not limit)
                  (limit . > . (sub1 len)))
              ;; grow pipe size
              (define new-bstr (make-bytes (min+1 limit (* len 2))))
              (cond
                [(zero? start)
                 (bytes-copy! new-bstr 0 bstr 0 (sub1 len))]
                [else
                 (bytes-copy! new-bstr 0 bstr start len)
                 (bytes-copy! new-bstr (- len start) bstr 0 end)
                 (set! start 0)
                 (set! end (sub1 len))])
              (set! bstr new-bstr)
              (end-atomic)
              (try-again)]
             [else
              ;; pipe is full
              (end-atomic)
              write-ready-evt]))
         (cond
           [(and (end . >= . start)
                 (end . < . top-pos))
            (define amt (min (- top-pos end)
                             (- src-end src-start)))
            (check-input-unblocking)
            (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
            (let ([new-end (+ end amt)])
              (set! end (if (= new-end len) 0 new-end)))
            (check-output-blocking)
            (end-atomic)
            amt]
           [(= end top-pos)
            (cond
              [(zero? start)
               (maybe-grow)]
              [else
               (check-input-unblocking)
               (define amt (min (sub1 start)
                                (- src-end src-start)))
               (bytes-copy! bstr 0 src-bstr src-start (+ src-start amt))
               (set! end amt)
               (check-output-blocking)
               (end-atomic)
               amt])]
           [(end . < . (sub1 start))
            (check-input-unblocking)
            (define amt (min (- (sub1 start) end)
                             (- src-end src-start)))
            (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
            (set! end (+ end amt))
            (check-output-blocking)
            (end-atomic)
            amt]
           [else
            (maybe-grow)])))
     
     #:close
     (lambda ()
       (start-atomic)
       (unless output-closed?
         (set! output-closed? #t)
         (semaphore-post write-ready-sema)
         (semaphore-post read-ready-sema))
       (end-atomic))))

  ;; Results ----------------------------------------
  (when (port-count-lines-enabled)
    (port-count-lines! ip)
    (port-count-lines! op))
  
  (values ip op))
