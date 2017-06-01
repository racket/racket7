(define collect-garbage
  (case-lambda
   [() (collect-garbage 'major)]
   [(request)
    (case request
      [(incremental) (void)]
      [(minor) (collect 0)]
      [(major) (collect (collect-maximum-generation))]
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
  (chez:fprintf (current-error-port) "Current memory use: ~a\n" (bytes-allocated))
  (void))

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
