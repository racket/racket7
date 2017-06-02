(top-level-program
 (import (except (chezpart)
                 eval)
         (core)
         (only (expander)
               set-exec-file!
               boot
               current-command-line-arguments
               use-compiled-file-paths
               current-library-collection-links
               find-library-collection-links
               current-library-collection-paths
               find-library-collection-paths
               executable-yield-handler
               load-on-demand-enabled
               eval
               dynamic-require
               namespace-require
               version
               exit)
         (regexp)
         (io)
         (thread))

 (unless (pair? (command-line-arguments))
   (error 'racket "expected a `self` executable path to start"))
 (set-exec-file! (path->complete-path (car (command-line-arguments))))

 (define (see saw . args)
   (let loop ([saw saw] [args args])
     (if (null? args)
         saw
         (loop (hash-set saw (car args) #t) (cdr args)))))
 (define (saw? saw tag)
   (hash-ref saw tag #f))

 (define rx:logging-spec (pregexp "^[\\s]*(none|fatal|error|warning|info|debug)(?:@([^\\s @]+))?(.*)$"))
 (define rx:all-whitespace (pregexp "^[\\s]*$"))
 (define (parse-logging-spec str where exit-on-fail?)
   (define (fail)
     (let ([msg (string-append
                 "stderr <levels> " where " must be one of the following\n"
                 " <level>s:\n"
                 "   none fatal error warning info debug\n"
                 "or up to one such <level> in whitespace-separated sequence of\n"
                 "   <level>@<name>\n"
                 "given: " str)])
       (cond
        [exit-on-fail?
         (raise-user-error 'racket msg)]
        [else
         (eprintf "~a\n" msg)])))
   (let loop ([str str] [default #f])
     (let ([m (regexp-match rx:logging-spec str)])
       (cond
        [m
         (let ([level (string->symbol (cadr m))]
               [topic (caddr m)])
           (cond
            [topic
             (cons level (cons (string->symbol topic) (loop (cadddr m) default)))]
            [default (fail)]
            [else (loop (cadddr m) level)]))]
        [(regexp-match? rx:all-whitespace str)
         (if default (list default) null)]
        [else (fail)]))))

 (define init-library '(lib "racket"))
 (define loads '())
 (define repl? #f)
 (define version? #f)
 (define stderr-logging-arg #f)

 (define (no-init! saw)
   (unless (saw? saw 'top)
     (set! init-library #f)))

 (define (next-arg what flag within-flag args)
   (let loop ([args (cdr args)] [accum '()])
     (cond
      [(null? args)
       (error 'racket "missing ~a after ~a switch" what (or within-flag flag))]
      [(pair? (car args))
       (loop (cdr args) (cons (car args) accum))]
      [else
       (values (car args) (append (reverse accum) (cdr args)))])))

 (define-syntax string-case
   ;; Assumes that `arg` is a variable
   (syntax-rules ()
     [(_ arg [else body ...])
      (let () body ...)]
     [(_ arg [(str ...) body ...] rest ...)
      (if (or (string=? arg str) ...)
          (let () body ...)
          (string-case arg rest ...))]))

 (let flags-loop ([args (cdr (command-line-arguments))]
                  [saw (hasheq)])
   ;; An element of `args` can become `(cons _arg _within-arg)`
   ;; due to splitting multiple flags with a single "-"
   (define (loop args) (flags-loop args saw))
   ;; Called to handle remaining non-switch arguments:
   (define (finish args saw)
     (cond
      [(and (pair? args)
            (not (saw? saw 'non-config)))
       (loop (cons "-u" args))]
      [else
       (|#%app| current-command-line-arguments (list->vector args))
       (when (and (null? args) (not (saw? saw 'non-config)))
         (set! repl? #t)
         (set! version? #t))]))
   ;; Dispatch on first argument:
   (if (null? args)
       (finish args saw)
       (let* ([arg (car args)]
              [within-arg (and (pair? arg) (cdr arg))]
              [arg (if (pair? arg) (car arg) arg)])
         (string-case
          arg
          [("-l" "--lib")
           (let-values ([(lib-name rest-args) (next-arg "library name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (dynamic-require `(lib ,lib-name) #f))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-t" "--require")
           (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (dynamic-require `(file ,file-name) #f))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-u" "--script")
           (let-values ([(file-name rest-args) (next-arg "file name" arg within-arg args)])
             (set! loads
                   (cons
                    (lambda ()
                      (dynamic-require `(file ,file-name) #f))
                    loads))
             (no-init! saw)
             (flags-loop rest-args (see saw 'non-config 'lib)))]
          [("-i" "--repl") 
           (set! repl? #t)
           (set! version? #t)
           (flags-loop (cdr args) (see saw 'non-config 'top))]
          [("-n" "--no-lib")
           (set! init-library #f)
           (flags-loop (cdr args) (see saw 'non-config))]
          [("-v" "--version") 
           (set! version? #t)
           (flags-loop (cddr args) (see saw 'non-config))]
          [("-c" "--no-compiled")
           (|#%app| use-compiled-file-paths '())
           (loop (cdr args))]
          [("-I")
           (let-values ([(lib-name rest-args) (next-arg "library name" arg within-arg args)])
             (when init-library
               (set! init-library `(lib ,lib-name)))
             (loop rest-args))]
          [("-d")
           (|#%app| load-on-demand-enabled #f)
           (loop (cdr args))]
          [("-W" "--stderr")
           (let-values ([(spec rest-args) (next-arg "stderr level" arg within-arg args)])
             (set! stderr-logging-arg (parse-logging-spec spec (format "after ~a switch" (or within-arg arg)) #t))
             (loop rest-args))]
          [("--")
           (cond
            [(or (null? (cdr args)) (not (pair? (cadr args))))
             (finish (cdr args) saw)]
            [else
             ;; Need to handle more switches from a combined flag
             (loop (cons (cadr args) (cons (car args) (cddr args))))])]
          [else
           (cond
            [(and (> (string-length arg) 2)
                  (eqv? (string-ref arg 0) #\-))
             (cond
              [(not (eqv? (string-ref arg 1) #\-))
               ;; Split flags
               (loop (append (map (lambda (c) (cons (string #\- c) arg))
                                  (cdr (string->list arg)))
                             (cdr args)))]
              [else
               (raise-user-error 'racket "bad switch: ~a~a"
                                 arg
                                 (if within-arg
                                     (format " within: ~a" within-arg)
                                     ""))])]
            [else
             ;; Non-flag argument
             (finish args saw)])]))))

 (define stderr-logging
   (or stderr-logging-arg
       (let ([spec (getenv "PLTSTDERR")])
         (if spec
             (parse-logging-spec spec "in PLTSTDERR environment variable" #f)
             '(error)))))

 (when version?
   (printf "Welcome to Racket v~a [cs]\n" (version)))
 (call-in-main-thread
  (lambda ()
    (boot)
    (when (and stderr-logging
               (not (null? stderr-logging)))
      (apply add-stderr-log-receiver! (|#%app| current-logger) stderr-logging))
    (|#%app| current-library-collection-links
     (find-library-collection-links))
    (|#%app| current-library-collection-paths
     (find-library-collection-paths))

   (when init-library
     (namespace-require init-library))

   (for-each (lambda (ld)
               (ld))
             (reverse loads))

   (when repl?
     (|#%app| (dynamic-require 'racket/base 'read-eval-print-loop)))

   (|#%app| (|#%app| executable-yield-handler) 0)
   
   (exit))))
