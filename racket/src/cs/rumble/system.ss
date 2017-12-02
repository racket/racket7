
(define system-type
  (case-lambda
   [() (system-type* 'os)]
   [(mode) (if (eq? mode 'vm)
               'chez-scheme
               (system-type* mode))]))

(define (system-type* mode)
  (case mode
    [(vm) 'chez-scheme]
    [(os) (case (machine-type)
            [(a6osx ta6osx i3osx ti3osx) 'macosx]
            [(a6nt ta6nt i3nt ti3nt) 'windows]
            [else 'unix])]
    [(word) (if (> (fixnum-width) 32) 64 32)]
    [(gc) '3m]
    [(link) 'framework]
    [(machine) "localhost info..."]
    [(so-suffix) (case (machine-type)
                   [(a6osx ta6osx i3osx ti3osx) (string->utf8 ".dylib")]
                   [(a6nt ta6nt i3nt ti3nt) (string->utf8 ".dll")]
                   [else (string->utf8 ".so")])]
    [(so-mode) 'local]
    [(fs-change) '#(#f #f #f #f)]
    [(cross) 'infer]
    [else (raise-argument-error 'system-type
                                (string-append
                                 "(or/c 'os 'word 'vm 'gc 'link 'machine\n"
                                 "      'so-suffix 'so-mode 'fs-change 'cross)")
                                mode)]))
