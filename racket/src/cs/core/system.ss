
(define system-type
  (case-lambda
   [() (system-type* 'os)]
   [(mode) (if (eq? mode 'vm)
               'chez-scheme
               (system-type* mode))]))

(define (system-type* mode)
  (case mode
    [(vm) 'not-chez-scheme]
    [(os) 'unix]
    [(word) 64]
    [(gc) '3m]
    [(link) 'framework]
    [(machine) "localhost info..."]
    [(so-suffix) (string->utf8 ".dylib")]
    [(so-mode) 'local]
    [(fs-change) '#(#f #f #f #f)]
    [(cross) 'infer]
    [else (raise-argument-error 'system-type
                                (string-append
                                 "(or/c 'os 'word 'vm 'gc 'link 'machine\n"
                                 "      'so-suffix 'so-mode 'fs-change 'cross)")
                                mode)]))
