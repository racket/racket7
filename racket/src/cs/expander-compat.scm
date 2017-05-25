;; Temporary compatibility stubs

(define-values (prop:checked-procedure checked-procedure? checked-procedure-ref)
  (make-struct-type-property 'checked-procedure))
(define-values (prop:impersonator-of -impersonator-of? impersonator-of-ref)
  (make-struct-type-property 'impersonator-of))
(define-values (prop:arity-string arity-string? arity-string-ref)
  (make-struct-type-property 'arity-string))

(define (checked-procedure-check-and-extract st v alt-proc v1 v2)
  (if (and (record? v st)
           (checked-procedure? v)
           (|#%app| (unsafe-struct-ref v 0) v1 v2))
      (unsafe-struct-ref v 1)
      (|#%app| alt-proc v v1 v2)))

(define (prop:chaperone-unsafe-undefined chaperone-unsafe-undefined? chaperone-unsafe-undefined-ref)
  (make-struct-type-property 'chaperone-unsafe-undefined))

(define (chaperone-struct-unsafe-undefined v) v)

(define (will-execute e)
  (raise-arguments-error 'will-execute "not yet ready"))

(define (chaperone-evt v . args) v)
(define (chaperone-channel v . args) v)
(define (impersonate-channel v . args) v)
(define (chaperone-struct-type v . args) v)

(define (port-try-file-lock? port mode) #f)
(define (port-file-unlock port) (void))

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

(define string-locale-downcase string-downcase)

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

(define (make-environment-variables . args)
  #f)
(define (environment-variables-ref e k)
  (let ([v (getenv (bytes->string/utf-8 k))])
    (and v (string->bytes/utf-8 v))))
(define (current-environment-variables) #f)
(define (environment-variables-set! e k v)
  (error "environment-variables-set! not ready"))
(define (environment-variables-copy e)
  'copy)
(define (environment-variables-names e)
  'names)
(define (environment-variables? e)
  #f)

(define (reparameterize . args) (void))

(define current-eval
  (make-parameter (lambda args (error "eval not ready"))))

(define exit-handler
  (make-parameter (lambda (v)
                    (cond
                     [(byte? v)
                      (chez:exit v)]
                     [else
                      (chez:exit 0)]))))

(define exit
  (case-lambda
   [() (|#%app| (|#%app| exit-handler) #t)]
   [(v) (|#%app| (|#%app| exit-handler) v)]))
             
(define read-decimal-as-inexact
  (make-parameter #t))

(define read-accept-bar-quote
  (make-parameter #t))

(define read-case-sensitive
  (make-parameter #t))

(define current-command-line-arguments
  (make-parameter '#()))

(define current-library-collection-paths
  (make-parameter null))
(define current-library-collection-links
  (make-parameter null))
(define use-collection-link-paths
  (make-parameter null))
(define use-user-specific-search-paths
  (make-parameter #t))
(define use-compiled-file-paths
  (make-parameter (list (string->path (string-append "compiled/" (symbol->string (machine-type)))))))
(define use-compiled-file-check
  (make-parameter 'modify-seconds
                  (lambda (v)
                    (unless (or (eq? v 'exists)
                                (eq? v 'modify-seconds))
                      (raise-argument-error 'use-compiled-file-check
                                            "(or/c 'modify-seconds 'exists)"
                                            v))
                    v)))
(define current-compiled-file-roots
  (make-parameter '(same)))
(define current-write-relative-directory
  (make-parameter #f))

(define current-load/use-compiled
  (make-parameter #f))
(define read-on-demand-source
  (make-parameter #f))

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

(define current-compile
  (make-parameter 'current-compile))
(define current-load
  (make-parameter 'current-load))
(define load-on-demand-enabled
  (make-parameter #f))

(define compile-enforce-module-constants
  (make-parameter #t))

(define (load s) (|#%app| (|#%app| current-load) s #f))

(define (load-extension f) (error "no load-extension"))

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
    [else `(find-system-path-not-ready ,key)]))

(define-values (prop:exn:srclocs exn:srclocs? exn:srclocs-accessor)
  (make-struct-type-property 'exn:srclocs))

;; Not good enough...
(define (make-ephemeron k v)
  (weak-cons k v))
(define (ephemeron-value p)
  (cond
   [(car p) (cdr p)]
   [(cdr p) (set-cdr! p #f) #f]
   [else #f]))
(define (ephemeron? x) (weak-pair? x))

(define (thread-receive-evt t) 'thread-receive-evt)
(define filesystem-change-evt
  (case-lambda
   [(p) (error 'filesystem-change-evt "unsupported")]
   [(p fail) (fail)]))
(define (filesystem-change-evt-cancel e) (void))
(define call-with-semaphore
  (case-lambda
   [(s proc) (proc)]
   [(s proc try-fail) (proc)]
   [(s proc try-fail . args) (apply proc args)]))

(define (srcloc->string s)
  (and (srcloc-source s)
       (format "~a:~s:~s"
               (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s))))

(define (procedure-arity? v)
  #t)

(define (procedure-reduce-arity p a)
  p)

(define (procedure->method p)
  p)

(define (procedure-rename p name)
  p)

(define (list-pair? v) #f)
(define (interned-char? v) #f)
(define (true-object? v) (eq? v #t))

(define eval-jit-enabled
  (make-parameter #t))

(define (plumber? p) (eq? p 'plumber))
(define (make-plumber) 'plumber)
(define (current-plumber) 'plumber)
(define (plumber-add-flush! p v) (set! at-exit (cons v null)))
(define (plumber-flush-all p) (void))
(define (plumber-flush-handle-remove! h) (void))
(define (plumber-flush-handle? p) #f)

(define at-exit null)
(define (flush)
  (let ([l at-exit])
    (set! at-exit null)
    (for-each (lambda (p) (p #f)) l)))

(define current-load-relative-directory
  (make-parameter #f))

(define read-char-or-special
  (case-lambda
   [() (read-char)]
   [(in) (read-char in)]
   [(in special src) (read-char in)]))

(define peek-char-or-special
  (case-lambda
   [() (peek-char)]
   [(in) (peek-char in)]
   [(in skip) (peek-char in skip)]
   [(in skip special src) (peek-char in skip)]))

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

;; ----------------------------------------

(define current-load-extension
  (make-parameter (lambda args (error "no extensions"))))

(define (futures-enabled?) #f)
(define (processor-count) 1)

(define-record-type (future* make-future future?)
  (fields (mutable thunk) (mutable result)))

(define (future thunk)
  (make-future thunk #f))

(define (touch f)
  ;; FIXME
  (when (future*-thunk f)
    (let ([t (future*-thunk f)])
      (future*-thunk-set! f #f)
      (future*-result-set! f (t))))
  (future*-result f))
  
(define (would-be-future thunk)
  (future thunk))

(define (current-future) #f)

(define-record-type (fsemaphore create-fsemaphore fsemaphore?)
  (fields sema))

(define (make-fsemaphore init)
  (create-fsemaphore (make-semaphore init)))

(define (fsemaphore-post f)
  (semaphore-post (fsemaphore-sema f)))

(define (fsemaphore-wait f)
  (semaphore-wait (fsemaphore-sema f)))

(define (fsemaphore-try-wait? f)
  (semaphore-try-wait? (fsemaphore-sema f)))

(define (fsemaphore-count f)
  ;; FIXME
  0)

(define (reset-future-logs-for-tracing!)
  (void))
(define (mark-future-trace-end!)
  (void))

;; ----------------------------------------

;; Table of things temporarily defined here; since these are not put
;; in the evaluation environment with `(import (core) (thread) ....)`,
;; each must be specifically defined
(define compat-table
  (make-primitive-table

   prop:checked-procedure checked-procedure? checked-procedure-ref
   prop:impersonator-of -impersonator-of? impersonator-of-ref
   prop:arity-string arity-string? arity-string-ref

   checked-procedure-check-and-extract

   chaperone-evt
   chaperone-channel
   impersonate-channel
   chaperone-struct-type

   equal-secondary-hash-code

   port-try-file-lock?
   port-file-unlock

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

   string-locale-downcase

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

   make-environment-variables
   environment-variables-ref
   current-environment-variables
   environment-variables-set!
   environment-variables-copy
   environment-variables-names
   environment-variables?

   exit
   exit-handler

   reparameterize
   current-eval
   read-decimal-as-inexact
   read-accept-bar-quote

   read-case-sensitive

   current-library-collection-paths
   current-library-collection-links
   current-command-line-arguments
   use-collection-link-paths
   use-user-specific-search-paths
   use-compiled-file-paths
   use-compiled-file-check
   current-compiled-file-roots
   current-write-relative-directory

   current-load/use-compiled
   read-on-demand-source

   current-code-inspector
   current-print
   current-read-interaction
   current-get-interaction-input-port
   error-print-source-location
   current-prompt-read

   current-compile
   current-load
   load
   load-on-demand-enabled

   compile-enforce-module-constants

   load-extension
   cache-configuration

   open-input-output-file

   find-system-path

   prop:exn:srclocs exn:srclocs? exn:srclocs-accessor

   gensym

   ephemeron?
   make-ephemeron
   ephemeron-value
   thread-receive-evt
   filesystem-change-evt
   filesystem-change-evt-cancel
   call-with-semaphore
   will-execute

   make-plumber
   current-plumber
   plumber-add-flush!
   plumber-flush-all
   plumber-flush-handle-remove!
   plumber-flush-handle?
   plumber?

   srcloc->string
   
   procedure-arity?
   procedure-reduce-arity
   procedure->method
   procedure-rename

   list-pair?
   interned-char?
   true-object?
   
   eval-jit-enabled
   current-memory-use

   current-load-relative-directory
   read-char-or-special
   peek-char-or-special
   datum-intern-literal
   current-load-extension
   string->number

   prop:chaperone-unsafe-undefined
   chaperone-struct-unsafe-undefined

   futures-enabled?
   processor-count
   future
   future?
   touch
   would-be-future
   current-future
   make-fsemaphore
   fsemaphore?
   fsemaphore-post
   fsemaphore-wait
   fsemaphore-try-wait?
   fsemaphore-count
   reset-future-logs-for-tracing!
   mark-future-trace-end!))
