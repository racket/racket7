
(define (set-collect-handler!)
  (collect-request-handler (lambda () (collect/report #f))))

(define (set-garbage-collect-notify! proc)
  (set! garbage-collect-notify proc))

(define garbage-collect-notify
  (lambda (gen pre-allocated pre-allocated+overhead pre-time re-cpu-time
               post-allocated post-allocated+overhead post-time post-cpu-time)
    (void)))

;; Replicate the counting that `(collect)` would do
;; so that we can report a generation to the notification
;; callback
(define gc-counter 1)
(define log-collect-generation-radix 2)
(define collect-generation-radix-mask (sub1 (bitwise-arithmetic-shift 1 log-collect-generation-radix)))

(define (collect/report g)
  (let ([this-counter (if g (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix g)) gc-counter)]
        [pre-allocated (bytes-allocated)]
        [pre-allocated+overhead (current-memory-bytes)]
        [pre-time (real-time)] 
        [pre-cpu-time (cpu-time)])
    (if (> (add1 this-counter) (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix (collect-maximum-generation))))
        (set! gc-counter 1)
        (set! gc-counter (add1 this-counter)))
    (let ([gen (let loop ([c this-counter] [gen 0])
                 (cond
                  [(zero? (bitwise-and c collect-generation-radix-mask))
                   (loop (bitwise-arithmetic-shift-right c log-collect-generation-radix) (add1 gen))]
                  [else gen]))])
      (collect gen)
      (garbage-collect-notify gen
                              pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                              (bytes-allocated)  (current-memory-bytes) (real-time) (cpu-time)))))
  
(define collect-garbage
  (case-lambda
   [() (collect-garbage 'major)]
   [(request)
    (void)
    #;(case request
      [(incremental) (void)]
      [(minor)
       (collect/report 0)]
      [(major)
       (collect/report (collect-maximum-generation))]
      [else
       (raise-argument-error 'collect-garbage
                             "(or/c 'major 'minor 'incremental)"
                             request)])]))

(define current-memory-use
  (case-lambda
   [() (bytes-allocated)]
   [(mode)
    (cond
     [(not mode) (bytes-allocated)]
     [(eq? mode 'cumulative) (sstats-bytes (statistics))]
     [else
      ;; must be a custodian...
      (bytes-allocated)])]))

(define (dump-memory-stats . args)
  (enable-object-counts #t)
  (collect (collect-maximum-generation))
  (let* ([counts (object-counts)]
         [extract (lambda (static? cxr)
                    (lambda (c) (if (or static? (not (eq? (car c) 'static)))
                                    (cxr c)
                                    0)))]
         [get-count (lambda (static?) (lambda (e) (apply + (map (extract static? cadr) (cdr e)))))]
         [get-bytes (lambda (static?) (lambda (e) (apply + (map (extract static? cddr) (cdr e)))))])
    (enable-object-counts #f)
    (chez:fprintf (current-error-port) "Current memory use: ~a\n" (bytes-allocated))
    (for-each (lambda (e)
                (chez:fprintf (current-error-port) " ~a   ~a ~a | ~a ~a\n"
                              (car e)
                              ((get-count #f) e) ((get-bytes #f) e)
                              ((get-count #t) e) ((get-bytes #t) e)))
              (list-sort (lambda (a b) (< ((get-bytes #f) a) ((get-bytes #f) b))) counts))
    (chez:fprintf (current-error-port) " total  ~a ~a | ~a ~a\n"
                  (apply + (map (get-count #f) counts))
                  (apply + (map (get-bytes #f) counts))
                  (apply + (map (get-count #t) counts))
                  (apply + (map (get-bytes #t) counts)))))

;; ----------------------------------------

(define-record-type (phantom-bytes create-phantom-bytes phantom-bytes?)
  (fields [mutable size]))

(define/who (make-phantom-bytes k)
  (check who exact-nonnegative-integer? k)
  (create-phantom-bytes k))

(define/who (set-phantom-bytes! phantom-bstr k)
  (check who phantom-bytes? phantom-bstr)
  (check who exact-nonnegative-integer? k)
  (phantom-bytes-size-set! phantom-bstr k))
