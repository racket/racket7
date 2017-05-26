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

(define (make-phantom-bytes k)
  (unless (exact-nonnegative-integer? k)
    (raise-argument-error 'make-phantom-bytes "exact-nonnegative-integer?" k))
  (create-phantom-bytes k))

(define (set-phantom-bytes! phantom-bstr k)
  (unless (phantom-bytes? phantom-bstr)
    (raise-argument-error 'set-phantom-bytes! "phantom-bytes?" phantom-bstr))
  (unless (exact-nonnegative-integer? k)
    (raise-argument-error 'set-phantom-bytes! "exact-nonnegative-integer?" k))
  (phantom-bytes-size-set! phantom-bstr k))
