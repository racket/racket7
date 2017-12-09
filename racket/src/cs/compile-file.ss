
;; Check to make we're using a build of Chez Scheme
;; that has all the features we need.

(define (check-defined sym)
  (unless (guard (x [else #f]) (eval sym))
    (error 'compile-file
           (format
            "missing definition of `~a`; probably you need a newer Chez Scheme"
            sym))))

(check-defined 'box-cas!)
(check-defined 'make-arity-wrapper-procedure)
(check-defined 'generate-procedure-source-information)
(check-defined 'object-backreferences)

;; ----------------------------------------

(current-make-source-object
 (lambda (sfd bfp efp)
   (call-with-values (lambda () (locate-source sfd bfp #t))
     (case-lambda
      [() (error 'compile-config "cannot get line and column")]
      [(name line col)
       (make-source-object sfd bfp efp line col)]))))

(generate-wpo-files #t)

(define (get-opt args flag arg-count)
  (cond
   [(null? args) #f]
   [(equal? (car args) flag)
    (unless (> (length args) arg-count)
      (error 'compile-file "missing argument for ~a" flag))
    (cdr args)]
   [else #f]))

(define whole-program? #f)
(generate-inspector-information #f)
(generate-procedure-source-information #t)

(define-values (src deps)
  (let loop ([args (command-line-arguments)])
    (cond
     [(get-opt args "--debug" 0)
      => (lambda (args)
           (generate-inspector-information #t)
           (loop args))]
     [(get-opt args "--unsafe" 0)
      => (lambda (args)
           (optimize-level 3)
           (loop args))]
     [(get-opt args "--whole-program" 0)
      => (lambda (args)
           (set! whole-program? #t)
           (loop args))]
     [(null? args)
      (error 'compile-file "missing source file")]
     [else
      (values (car args) (cdr args))])))

(cond
 [whole-program?
  (unless (= 1 (length deps))
    (error 'compile-file "expected a single dependency for whole-program compilation"))
  (compile-whole-program (car deps) src #t)]
 [else
  (for-each load deps)
  (compile-file src)])
