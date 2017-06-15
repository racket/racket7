
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

;; lifted from Chez: mats/foreign.ms
(define-syntax machine-case
  (lambda (x)
    (syntax-case x ()
      [(_ [(a ...) e ...] m ...)
       (if (#%memq (machine-type) (datum (a ...)))
           #'(begin (void) e ...)
           #'(machine-case m ...))]
      [(_ [else e ...]) #'(begin (void) e ...)]
      [(_) #'(void)])))

(define (find-processor-count)
  (machine-case
    [(ta6osx a6osx ti3osx i3osx)
     ;; OS X
     (load-shared-object "libc.dylib")

     (cond
      [(foreign-entry? "sysctlbyname")
       (let ([fn (foreign-procedure "sysctlbyname" (string (* int) (* size_t) (* int) size_t) int)]
             [result-out (make-ftype-pointer int (foreign-alloc (ftype-sizeof int)))]
             [size-inout (make-ftype-pointer size_t (foreign-alloc (ftype-sizeof size_t)))]
             [null-ptr (make-ftype-pointer int 0)])

         (ftype-set! size_t () size-inout (ftype-sizeof int))

         (begin0
          (if (zero? (fn "hw.ncpu" result-out size-inout null-ptr 0))
              (ftype-ref int () result-out)
              1)

          (foreign-free (ftype-pointer-address result-out))
          (foreign-free (ftype-pointer-address size-inout))))]
      [else
       1])]

    [(ta6le a6le ti3le i3le tppc32le ppc32le arm32le)
     ;; Linux
     (load-shared-object "libc.so.6")

     (cond
      [(foreign-entry? "sysconf")
       (let ([fn (foreign-procedure "sysconf" (int) long)]
             [_SC_NPROCESSORS_ONLN 84])
         (fn _SC_NPROCESSORS_ONLN))]
      [else
       1])]

    [(ta6nt a6nt ti3nt i3nt)
     ;; Windows
     (let ()
       (define-ftype SYSTEM_INFO
         (struct
          [wProcessorArchitecture unsigned-16]
          [wReserved unsigned-16]
          [dwPageSize unsigned-32]
          [lpMinimumApplicationAddress void*]
          [lpMaximumApplicationAddress void*]
          [dwActiveProcessorMask (* unsigned-32)]
          [dwNumberOfProcessors unsigned-32]
          [dwProcessorType unsigned-32]
          [dwAllocationGranularity unsigned-32]
          [wProcessorLevel unsigned-16]
          [wProcessorRevision unsigned-16]))

       (load-shared-object "crtdll.dll")

       (cond
        [(foreign-entry? "GetSystemInfo")
         (let ([fn (foreign-procedure __stdcall "GetSystemInfo" ((* SYSTEM_INFO)) void)]
               [info-out (make-ftype-pointer SYSTEM_INFO (foreign-alloc (ftype-sizeof SYSTEM_INFO)))])

           (fn info-out)

           (begin0
            (ftype-ref SYSTEM_INFO (dwNumberOfProcessors) info-out)
            (foreign-free (ftype-pointer-address info-out))))]
        [else
         1]))]

    [else
     1]))
