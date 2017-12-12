
(define (set-collect-handler!)
  (collect-request-handler (lambda () (collect/report #f))))

(define (set-garbage-collect-notify! proc)
  (set! garbage-collect-notify proc))

(define garbage-collect-notify
  (lambda (gen pre-allocated pre-allocated+overhead pre-time re-cpu-time
               post-allocated post-allocated+overhead post-time post-cpu-time)
    (void)))

(define collect-garbage-pending-major? (box '()))
(define collect-garbage-pending-minor? (box '()))

;; Replicate the counting that `(collect)` would do
;; so that we can report a generation to the notification
;; callback
(define gc-counter 1)
(define log-collect-generation-radix 2)
(define collect-generation-radix-mask (sub1 (bitwise-arithmetic-shift 1 log-collect-generation-radix)))
(define allocated-after-major (* 32 1024 1024))

(define (collect/report g)
  (let ([this-counter (if g (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix g)) gc-counter)]
        [pre-allocated (bytes-allocated)]
        [pre-allocated+overhead (current-memory-bytes)]
        [pre-time (real-time)] 
        [pre-cpu-time (cpu-time)])
    (if (> (add1 this-counter) (bitwise-arithmetic-shift-left 1 (* log-collect-generation-radix (sub1 (collect-maximum-generation)))))
        (set! gc-counter 1)
        (set! gc-counter (add1 this-counter)))
    (let ([gen (cond
                [(and (not g)
                      (>= pre-allocated (* 2 allocated-after-major)))
                 ;; Force a major collection if memory use has doubled
                 (collect-maximum-generation)]
                [else
                 ;; Find the minor generation implied by the counter
                 (let loop ([c this-counter] [gen 0])
                   (cond
                    [(zero? (bitwise-and c collect-generation-radix-mask))
                     (loop (bitwise-arithmetic-shift-right c log-collect-generation-radix) (add1 gen))]
                    [else gen]))])])
      (collect gen)
      (let ([post-allocated (bytes-allocated)])
        (when (= gen (collect-maximum-generation))
          (set! allocated-after-major post-allocated))
        (garbage-collect-notify gen
                                pre-allocated pre-allocated+overhead pre-time pre-cpu-time
                                post-allocated  (current-memory-bytes) (real-time) (cpu-time)))
      (poll-foreign-guardian))))
  
(define collect-garbage
  (case-lambda
   [() (collect-garbage 'major)]
   [(request)
    (cond
     [(eq? request 'incremental) ;; don't want to halt for void.
      (void)]
     [(= 0 (get-thread-id))
      (halt-workers)
      (case request
        [(minor)
         (collect/report 0)]
        [(major)
         (collect/report (collect-maximum-generation))]
        [else
         (raise-argument-error 'collect-garbage
                               "(or/c 'major 'minor 'incremental)"
                               request)])
      (resume-workers)]
     [else
      (let ([rq-box (case request
                      [(minor)
                       collect-garbage-pending-minor?]
                      [(major)
                       collect-garbage-pending-major?]
                      [else
                       (raise-argument-error 'collect-garbage
                               "(or/c 'major 'minor 'incremental)"
                               request)])])
        (let add-request ()
          (let ([old-val (unbox rq-box)])
            (unless (box-cas! rq-box
                              old-val
                              (cons (current-future) old-val))
                    (add-request)))))
      (future-wait)])]))

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
  (let ([backtrace-predicate (parse-dump-memory-stats-arguments args)])
    (enable-object-counts #t)
    (enable-object-backreferences (and backtrace-predicate #t))
    (collect (collect-maximum-generation))
    (let* ([counts (object-counts)]
           [backreferences (object-backreferences)]
           [extract (lambda (static? cxr)
                      (lambda (c) (if (or static? (not (eq? (car c) 'static)))
                                      (cxr c)
                                      0)))]
           [get-count (lambda (static?) (lambda (e) (apply + (map (extract static? cadr) (cdr e)))))]
           [get-bytes (lambda (static?) (lambda (e) (apply + (map (extract static? cddr) (cdr e)))))]
           [pad (lambda (s n)
                  (string-append (make-string (max 0 (- n (string-length s))) #\space) s))]
           [pad-right (lambda (s n)
                        (string-append s (make-string (max 0 (- n (string-length s))) #\space)))]
           [commas (lambda (n)
                     (let* ([l (string->list (number->string n))]
                            [len (length l)])
                       (list->string
                        (cons #\space
                              (let loop ([l l] [len len])
                                (cond
                                 [(<= len 3) l]
                                 [else
                                  (let ([m (modulo len 3)])
                                    (case m
                                      [(0) (list* (car l)
                                                  (cadr l)
                                                  (caddr l)
                                                  #\,
                                                  (loop (cdddr l) (- len 3)))]
                                      [(2) (list* (car l)
                                                  (cadr l)
                                                  #\,
                                                  (loop (cddr l) (- len 2)))]
                                      [else  (list* (car l)
                                                    #\,
                                                    (loop (cdr l) (- len 1)))]))]))))))]
           [count-width 11]
           [size-width 13]
           [trim-type (lambda (s)
                        (let ([len (string-length s)])
                          (cond
                           [(and (> len 14)
                                 (string=? (substring s 2 14) "record type "))
                            (string-append "#<" (substring s 14 len))]
                           [else s])))]
           [layout (lambda args
                     (let loop ([args args] [actual-col 0] [want-col 0])
                       (cond
                        [(null? args) "\n"]
                        [(< actual-col want-col)
                         (string-append (make-string (- want-col actual-col) #\space)
                                        (loop args want-col want-col))]
                        [(integer? (car args))
                         (loop (cons (pad (commas (car args))
                                          (- (+ want-col (sub1 (cadr args)))
                                             actual-col))
                                     (cdr args))
                               actual-col
                               want-col)]
                        [else
                         (string-append (car args)
                                        (loop (cddr args)
                                              (+ actual-col (string-length (car args)))
                                              (+ want-col (cadr args))))])))]
           [layout-line (lambda (label c1 s1 c2 s2)
                          (layout " " 1
                                  (trim-type label) 22
                                  c1 count-width
                                  s1 size-width
                                  " | " 3
                                  c2 count-width
                                  s2 size-width))])
      (enable-object-counts #f)
      (enable-object-backreferences #f)
      (chez:fprintf (current-error-port) "Begin Dump\n")
      (chez:fprintf (current-error-port) "Current memory use: ~a\n" (bytes-allocated))
      (chez:fprintf (current-error-port) "Begin RacketCS\n")
      (for-each (lambda (e)
                  (chez:fprintf (current-error-port)
                                (layout-line (chez:format "~a" (car e))
                                             ((get-count #f) e) ((get-bytes #f) e)
                                             ((get-count #t) e) ((get-bytes #t) e))))
                (list-sort (lambda (a b) (< ((get-bytes #f) a) ((get-bytes #f) b))) counts))
      (chez:fprintf (current-error-port) (layout-line "total"
                                                      (apply + (map (get-count #f) counts))
                                                      (apply + (map (get-bytes #f) counts))
                                                      (apply + (map (get-count #t) counts))
                                                      (apply + (map (get-bytes #t) counts))))
      (chez:fprintf (current-error-port) "End RacketCS\n")
      (when backtrace-predicate
        (let ([backreference-ht (make-eq-hashtable)])
          (for-each (lambda (l)
                      (for-each (lambda (p)
                                  (hashtable-set! backreference-ht (car p) (cdr p)))
                                l))
                    backreferences)
          (chez:fprintf (current-error-port) "Begin Traces\n")
          (let ([prev-trace '()])
            (for-each (lambda (l)
                        (for-each (lambda (p)
                                    (when (backtrace-predicate (car p))
                                      (chez:printf "*== ~a" (object->backreference-string (car p)))
                                      (let loop ([prev (car p)] [o (cdr p)] [accum '()])
                                        (cond
                                         [(not o) (set! prev-trace (reverse accum))]
                                         [(chez:memq o prev-trace)
                                          => (lambda (l)
                                               (chez:printf " <- DITTO\n")
                                               (set! prev-trace (append (reverse accum) l)))]
                                         [else
                                          (chez:printf " <- ~a" (object->backreference-string
                                                                 (cond
                                                                  [(and (pair? o)
                                                                        (eq? prev (car o)))
                                                                   (cons 'PREV (cdr o))]
                                                                  [(and (pair? o)
                                                                        (eq? prev (cdr o)))
                                                                   (cons (car o) 'PREV)]
                                                                  [else o])))
                                          (loop o (hashtable-ref backreference-ht o #f) (cons o accum))]))))
                                  l))
                      backreferences))
          (chez:fprintf (current-error-port) "End Traces\n")))
      (chez:fprintf (current-error-port) "End Dump\n"))))

(define (parse-dump-memory-stats-arguments args)
  (cond
   [(null? args) #f]
   [(and (list? (car args))
         (= 2 (length (car args)))
         (eq? (caar args) 'struct)
         (symbol? (cadar args)))
    (let ([struct-name (cadar args)])
      (lambda (o)
        (and (#%$record? o)
             (eq? (record-type-name (#%$record-type-descriptor o)) struct-name))))]
   [(symbol? (car args))
    (let ([type (car args)])
      (lambda (o)
        (eq? ((inspect/object o) 'type) type)))]
   [else #f]))

(define (object->backreference-string o)
  (parameterize ([print-level 3])
    (let ([s (call-with-string-output-port
              (lambda (dest)
                (pretty-print o dest)))])
      (if (> (string-length s) 256)
          (let ([s (substring s 0 256)])
            (string-set! s 252 #\.)
            (string-set! s 253 #\.)
            (string-set! s 254 #\.)
            (string-set! s 255 #\newline)
            s)
          s))))

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
