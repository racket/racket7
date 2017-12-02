(import (rumble)
        (expander)
        (io))

(define (show v) (write v) (newline))

(call-in-main-thread
 (lambda ()
   (boot)

   (set-exec-file! (path->complete-path (string->path "../../bin/racket")))
   
   (namespace-require ''|#%kernel|)

   (expand '1)
   (eval '((lambda (x) x) 1))

   (eval '(module m '|#%kernel|
           (|#%require| (for-syntax '|#%kernel|))
           (define-syntaxes (m)
             (lambda (stx)
               (quote-syntax 'ex)))
           (define-values (x) (m))
           (|#%provide| x)))
   (eval '(|#%require| 'm))
   (eval 'x)

   (let ()
     (define (run s)
       (show (eval (read (open-input-string s)))))
     ;; (run "'x")
     (void))

   (|#%app| use-compiled-file-paths '()) ; => expand from source
   (|#%app| current-library-collection-links
    (find-library-collection-links))
   (|#%app| current-library-collection-paths
    (find-library-collection-paths))
     
   (time (eval '(|#%require| racket/base)))
   
   ;;(time (eval `(|#%require| "../regexp/demo.rkt")))
   ;;(time (eval `(|#%require| "../../../pkgs/expander/main.rkt")))
   
   (void)))
