(top-level-program
 (import (except (chezpart)
                 eval)
         (core)
         (only (expander)
               set-exec-file!
               boot
               current-library-collection-links
               find-library-collection-links
               current-library-collection-paths
               find-library-collection-paths
               eval
               dynamic-require
               namespace-require
               version
               exit)
         (io)
         (thread))

 (unless (pair? (command-line-arguments))
   (error 'racket "expected a `self` executable path to start"))
 (set-exec-file! (string->path (car (command-line-arguments))))
 
 (define init-library '(lib "racket"))
 (define loads '())
 (define repl-by-default? #t)
 (define repl? #f)
 (define version-by-default? #t)
 (define version? #f)

 (define (no-repl-by-default!)
   (set! version-by-default? #f)
   (set! repl-by-default? #f))
 (define (no-init!)
   (set! init-library #f))

 (define (next-arg flag args)
   (when (null? (cdr args))
     (error 'racket "missing argument for `~a`" flag))
   (cadr args))

 (let loop ([args (cdr (command-line-arguments))])
   (unless (null? args)
     (let ([arg (car args)])
       (cond
        [(equal? arg "-l")
         (no-repl-by-default!)
         (no-init!)
         (set! loads
               (cons
                (lambda ()
                  (dynamic-require `(lib ,(next-arg "-l" args)) #f))
                loads))
         (loop (cddr args))]
        [(equal? arg "-t")
         (no-repl-by-default!)
         (no-init!)
         (set! loads
               (cons
                (lambda ()
                  (dynamic-require `(file ,(next-arg "-t" args)) #f))
                loads))
         (loop (cddr args))]
        [(equal? arg "-I")
         (set! init-library `(lib ,(next-arg "-I" args)))
         (loop (cddr args))]
        [else
         (error 'racket "unknown argument: ~a" arg)]))))

 (when (or version? version-by-default?)
   (printf "Welcome to Racket v~a [cs]\n" (version)))
 (call-in-main-thread
  (lambda ()
    (boot)
    (add-stderr-log-receiver! (|#%app| current-logger) 'error)
    (|#%app| current-library-collection-links
     (find-library-collection-links))
    (|#%app| current-library-collection-paths
     (find-library-collection-paths))

   (when init-library
     (namespace-require init-library))

   (for-each (lambda (ld)
               (ld))
             (reverse loads))

   (when (or repl? repl-by-default?)
     (|#%app| (dynamic-require 'racket/base 'read-eval-print-loop)))

   (exit))))
