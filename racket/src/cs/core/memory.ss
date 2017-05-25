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
