#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt")

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

(define (make-pipe [limit #f] [input-name 'pipe] [output-name 'pipe])
  (check 'open-input-bytes (lambda (x) (or (not x) (exact-positive-integer? limit)))
         #:contract "(or/c #f exact-nonnegative-integer?)"
         limit)
  (define bstr (make-bytes (min+1 limit 256)))
  (define start 0)
  (define end 0)
  (define output-closed? #f)
  (define data
    (pipe-data
     (lambda ()
       (if (start . <= . end)
           (- end start)
           (+ end (- (bytes-length bstr) start))))))
  (values
   ;; input ----------------------------------------
   (make-core-input-port
    #:name input-name
    #:data data
    
    #:read-byte
    (lambda ()
      (let try-again ()
        (cond
         [(= start end)
          (if output-closed?
              eof
              (begin
                '(wait) ;; FIXME
                (try-again)))]
         [else
          (define pos start)
          (set! start (modulo (add1 pos) (bytes-length bstr)))
          (bytes-ref bstr pos)])))

    #:read-in
    (lambda (dest-bstr dest-start dest-end copy?)
      (cond
       [(= start end)
        (if output-closed?
            eof
            0)]
       [else
        (define len (bytes-length bstr))
        (cond
         [(start . < . end)
          (define amt (min (- dest-end dest-start)
                           (- end start)))
          (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
          (set! start (+ start amt))
          amt]
         [else
          (define amt (min (- dest-end dest-start)
                           (- len start)))
          (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
          (set! start (modulo (+ start amt) len))
          amt])]))
    
    #:peek-byte
    (lambda ()
      (let try-again ()
        (cond
         [(= start end)
          (if output-closed?
              eof
              (begin
                '(wait) ; FIXME
                (try-again)))]
         [else
          (bytes-ref bstr start)])))
    
    #:peek-in
    (lambda (dest-bstr dest-start dest-end skip copy?)
      (define content-amt
        (if (start . < . end)
            (- end start)
            (+ end (- (bytes-length bstr) start))))
      (cond
       [(content-amt . <= . skip)
        (if output-closed?
            eof
            0)]
       [else
        (define len (bytes-length bstr))
        (define peek-start (modulo (+ start skip) len))
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
          amt])]))
    
    #:close
    void)
   
   ;; out ----------------------------------------
   (make-core-output-port
    #:name output-name
    #:data data

    #:evt 'evt ;; FIXME
    
    #:write-out
    (lambda (src-bstr src-start src-end nonblock? enable-break?)
      (let try-again ()
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
            (try-again)]
           [else
            ;; pipe is full
            0]))
        (cond
         [(and (end . >= . start)
               (end . < . top-pos))
          (define amt (min (- top-pos end)
                           (- src-end src-start)))
          (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
          (let ([new-end (+ end amt)])
            (set! end (if (= new-end len) 0 new-end)))
          amt]
         [(= end top-pos)
          (cond
           [(zero? start)
            (maybe-grow)]
           [else
            (define amt (min (sub1 start)
                             (- src-end src-start)))
            (bytes-copy! bstr 0 src-bstr src-start (+ src-start amt))
            (set! end amt)
            amt])]
         [(end . < . (sub1 start))
          (define amt (min (- (sub1 start) end)
                           (- src-end src-start)))
          (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
          (set! end (+ end amt))
          amt]
         [else
          (maybe-grow)])))
    
    #:close
    (lambda ()
      (set! output-closed? #t)))))
