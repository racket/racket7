(import (rumble)
        (expander)
        (io))

(define time-compiler-passes? #f)

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

   (when time-compiler-passes?
     (#%$enable-pass-timing #t))
     
   (time (eval '(|#%require| racket/base)))
   
   ;;(time (eval `(|#%require| "../regexp/demo.rkt")))
   ;;(time (eval `(|#%require| "../../../pkgs/expander/main.rkt")))

   (when time-compiler-passes?
     (let ([l (sort
               (lambda (a b) (< (cdr a) (cdr b)))
               (map (lambda (r) (cons (car r)
                                      (let ([t (caddr r)])
                                        (+ (* 1000. (time-second t))
                                           (/ (time-nanosecond t) 1000000.)))))
                    (#%$pass-stats)))])
       (for-each (lambda (p) (printf "~a~a: ~s\n"
                                     (car p)
                                     (make-string (max 0 (- 25 (string-length (symbol->string (car p))))) #\space)
                                     (cdr p)))
                 (append l
                         (list (cons 'total (apply + (map cdr l))))))))

   (void)))
