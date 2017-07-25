;; Temporary compatibility stubs

(define-values (prop:arity-string arity-string? arity-string-ref)
  (make-struct-type-property 'arity-string))

(define (chaperone-evt v . args) v)
(define (chaperone-channel v . args) v)
(define (impersonate-channel v . args) v)

(define (replace-evt a b) (error 'replace-evt "unsupported"))

(define (port-try-file-lock? port mode) #f)
(define (port-file-unlock port) (void))
(define (port-file-identity port) (error 'port-file-identity "not yet supported"))

(define (equal-secondary-hash-code v) (equal-hash-code v))

(define (primitive? v) #f)
(define (primitive-closure? v) #f)
(define (primitive-result-arity prim)
  (raise-argument-error 'primitive-result-arity "primitive?" prim))

(define fx->fl fixnum->flonum)
(define fxrshift fxarithmetic-shift-right)
(define fxlshift fxarithmetic-shift-left)
(define shared-fxvector fxvector)
(define make-shared-fxvector make-fxvector)

(define fl->fx flonum->fixnum)
(define ->fl real->flonum)
(define (fl->exact-integer fl) (inexact->exact (flfloor fl)))
(define unsafe-flrandom random)

(define flreal-part real-part)
(define flimag-part imag-part)
(define make-flrectangular make-rectangular)

(define (char-graphic? x) #f)
(define (char-blank? x) #f)
(define (char-iso-control? x) #f)
(define (char-punctuation? x) #f)
(define (char-symbolic? x) #f)

(define system-library-subpath
  (case-lambda
   [() (system-library-subpath (system-type 'gc))]
   [(mode)
    (case mode
      [(3m) (string->path "x86_64-macosx/3m")]
      [else (string->path "x86_64-macosx")])]))

(define (system-path-convention-type) 'unix)

(define (subprocess? v) #f)
(define (subprocess . args) #f)
(define subprocess-group-enabled
  (make-parameter #f (lambda (v) (and v #t))))
(define (subprocess-kill p force?) (void))
(define (subprocess-pid p) 0)
(define (subprocess-status p) 'something)
(define (subprocess-wait p) (void))
(define current-subprocess-custodian-mode
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v) (eq? v 'kill) (eq? v 'interrupt))
                      (raise-argument-error 'current-subprocess-custodian-mode
                                            "(or/c #f 'kill 'interrupt)"))
                    v)))

(define (shell-execute . args) (error "shell-execute"))

(define (make-environment-variables . args)
  #f)
(define (environment-variables-ref e k)
  (let ([v (getenv (bytes->string/utf-8 k))])
    (and v (string->bytes/utf-8 v))))
(define current-environment-variables
  (make-parameter #f))
(define (environment-variables-set! e k v)
  (error "environment-variables-set! not ready"))
(define (environment-variables-copy e)
  'copy)
(define (environment-variables-names e)
  'names)
(define (environment-variables? e)
  #f)

(define (reparameterize . args) (void))

(define executable-yield-handler
  (make-parameter void (lambda (p)
                         (unless (and (procedure? p)
                                      (procedure-arity-includes? p 1))
                           (raise-argument-error 'executable-yield-handler
                                                 "(procedure-arity-includes/c 1)"
                                                 p))
                         p)))

(define current-command-line-arguments
  (make-parameter '#() (lambda (v)
                         (define l (and (vector? v)
                                        (vector->list v)))
                         (unless (and (vector? v)
                                      (andmap string? l))
                           (raise-argument-error 'current-command-line-arguments
                                                 "(vectorof string?)"))
                         (list->vector (map string->immutable-string l)))))

(define current-code-inspector
  (make-parameter (|#%app| current-inspector)))
(define current-print
  (make-parameter (lambda (v)
                    (unless (void? v)
                      (print v)
                      (newline)))))
(define current-read-interaction
  (make-parameter
   (lambda (src in)
     (parameterize ([1/read-accept-reader #t]
                    [1/read-accept-lang #f])
       (1/read-syntax src in)))))
(define error-print-source-location
  (make-parameter #t))
(define current-prompt-read
  (make-parameter
   (lambda ()
     (display "> ")
     (let ([in ((|#%app| current-get-interaction-input-port))])
       (|#%app| (|#%app| current-read-interaction) (object-name in) in)))))
(define current-get-interaction-input-port
  (make-parameter
   (lambda () (|#%app| current-input-port))))

(define load-on-demand-enabled
  (make-parameter #t))

(define compile-enforce-module-constants
  (make-parameter #t))
(define compile-context-preservation-enabled
  (make-parameter #f))
(define compile-allow-set!-undefined
  (make-parameter #f))

(define (cache-configuration id proc) (proc))

(define (open-input-output-file . args) (error "no open-input-output-file"))

(define exec-file #f)
(define (set-exec-file! p) (set! exec-file p))

(define run-file #f)
(define (set-run-file! p) (set! run-file p))

(define (find-system-path key)
  (case key
    [(exec-file) (or exec-file
                     (string->path "/usr/local/bin/racket"))]
    [(config-dir host-config-dir) (string->path "../config")]
    [(collects-dir host-collects-dir) (string->path "../collects")]
    [(addon-dir) (string->path "/tmp/addon")]
    [(orig-dir) (string->path (|#%app| current-directory))]
    [(run-file) (or run-file
                    (find-system-path 'exec-file))]
    [(temp-dir) (string->path (or (getenv "TMPDIR") "/usr/tmp/"))]
    [else `(find-system-path-not-ready ,key)]))

(define-values (prop:exn:srclocs exn:srclocs? exn:srclocs-accessor)
  (make-struct-type-property 'exn:srclocs))

(define (thread-receive-evt t) 'thread-receive-evt)

(define current-thread-initial-stack-size
  (make-parameter 64
                  (lambda (v)
                    (unless (exact-positive-integer? v)
                      (raise-argument-error 'current-thread-initial-stack-size
                                            "exact-positive-integer?"
                                            v))
                    v)))

(define filesystem-change-evt
  (case-lambda
   [(p) (error 'filesystem-change-evt "unsupported")]
   [(p fail) (fail)]))

(define (filesystem-change-evt-cancel e) (void))

(define (filesystem-change-evt? v) #f)

(define current-directory-for-user
  (make-parameter (|#%app| current-directory)))
(define (srcloc->string s)
  (and (srcloc-source s)
       (format "~a:~s:~s"
               (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s))))

(define (procedure->method p)
  p)

(define (interned-char? v)
  (and (char? v) (< (char->integer v) 256)))

(define (true-object? v) (eq? v #t))

(define eval-jit-enabled
  (make-parameter #t))

(define current-load-relative-directory
  (make-parameter #f))

(define datums (make-weak-hash))

(define (datum-intern-literal v)
  (cond
   [(or (number? v)
        (string? v)
        (char? v)
        (bytes? v)
        (regexp? v))
    (or (weak-hash-ref-key datums v)
        (let ([v (cond
                  [(string? v) (string->immutable-string v)]
                  [(bytes? v) (bytes->immutable-bytes v)]
                  [else v])])
          (hash-set! datums v #t)
          v))]
   [else v]))

(define (make-known-char-range-list)
  '((0 255 #f)))

;; ----------------------------------------

(define current-load-extension
  (make-parameter (lambda args (error "no extensions"))))

(define (reset-future-logs-for-tracing!)
  (void))
(define (mark-future-trace-end!)
  (void))

(define vector-set-performance-stats!
  (case-lambda
   [(vec) (vector-set-performance-stats! vec #f)]
   [(vec thd)
    (define (maybe-set! i v)
      (when (< i (vector-length vec))
        (vector-set! vec i v)))
    (cond
     [(not thd)
      (maybe-set! 0 (current-process-milliseconds))
      (maybe-set! 1 (current-milliseconds))
      (maybe-set! 2 (current-gc-milliseconds))
      (maybe-set! 3 0) ; # of GCs
      (maybe-set! 4 0) ; # of thread switches
      (maybe-set! 5 0) ; # of stack overflows
      (maybe-set! 6 0) ; # of threads scheduled for running
      (maybe-set! 7 0) ; # of syntax objects read
      (maybe-set! 8 0) ; # of hash table searches
      (maybe-set! 9 0) ; # of hash table collisions
      (maybe-set! 10 0) ; non-GCed memory allocated for machine code
      (maybe-set! 11 0) ; peak memory use before a GC
      (void)]
     [else
      (maybe-set! 0 (thread-running? thd))
      (maybe-set! 1 (thread-dead? thd))
      (maybe-set! 2 #f) ; blocked for synchronization?
      (maybe-set! 3 #f) ; continuation size in bytes
      (void)])]))


(define-record-type (fsemaphore create-fsemaphore fsemaphore?)
  (fields sema))

(define (make-fsemaphore init)
  (create-fsemaphore (make-semaphore init)))

(define (fsemaphore-post fsema)
  (semaphore-post (fsemaphore-sema fsema)))

(define (fsemaphore-wait fsema)
  (semaphore-wait (fsemaphore-sema fsema)))

(define (fsemaphore-try-wait? fsema)
  (semaphore-try-wait? (fsemaphore-sema fsema)))

(define (fsemaphore-count fsema)
  (void))

;; ----------------------------------------

;; Table of things temporarily defined here; since these are not put
;; in the evaluation environment with `(import (core) (thread) ....)`,
;; each must be specifically defined
(define compat-table
  (make-primitive-table

   prop:arity-string arity-string? arity-string-ref

   chaperone-evt
   chaperone-channel
   impersonate-channel

   replace-evt

   equal-secondary-hash-code

   port-try-file-lock?
   port-file-unlock
   port-file-identity

   primitive?
   primitive-closure?
   primitive-result-arity

   fx->fl
   fxrshift
   fxlshift
   shared-fxvector
   make-shared-fxvector

   fl->fx
   ->fl
   fl->exact-integer
   unsafe-flrandom
   
   flreal-part
   flimag-part
   make-flrectangular

   char-graphic?
   char-blank?
   char-iso-control?
   char-punctuation?
   char-symbolic?

   system-type
   system-library-subpath
   system-path-convention-type

   subprocess?
   subprocess
   subprocess-group-enabled
   subprocess-kill
   subprocess-pid
   subprocess-status
   subprocess-wait
   current-subprocess-custodian-mode
   shell-execute

   make-environment-variables
   environment-variables-ref
   current-environment-variables
   environment-variables-set!
   environment-variables-copy
   environment-variables-names
   environment-variables?

   reparameterize
   read-decimal-as-inexact
   read-accept-bar-quote

   read-case-sensitive

   current-write-relative-directory

   current-code-inspector
   current-print
   current-read-interaction
   current-get-interaction-input-port
   error-print-source-location
   current-prompt-read

   load-on-demand-enabled

   compile-enforce-module-constants
   compile-context-preservation-enabled
   compile-allow-set!-undefined

   cache-configuration

   open-input-output-file

   find-system-path

   prop:exn:srclocs exn:srclocs? exn:srclocs-accessor

   gensym

   thread-receive-evt
   filesystem-change-evt
   filesystem-change-evt-cancel
   filesystem-change-evt?
   current-thread-initial-stack-size

   current-directory-for-user
   srcloc->string

   make-known-char-range-list
   
   procedure->method

   interned-char?
   true-object?
   
   eval-jit-enabled
   current-memory-use

   current-load-relative-directory
   datum-intern-literal
   current-load-extension
   string->number

   reset-future-logs-for-tracing!
   mark-future-trace-end!

   vector-set-performance-stats!

   make-fsemaphore
   fsemaphore-post
   fsemaphore-wait
   fsemaphore-try-wait?
   fsemaphore-count))
