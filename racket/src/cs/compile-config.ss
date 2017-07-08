
(reset-handler abort)
(keyboard-interrupt-handler abort)

(current-make-source-object
 (lambda (sfd bfp efp)
   (cond
    [(integer? bfp)
     (call-with-values (lambda () (locate-source sfd bfp #t))
       (case-lambda
        [() (error 'compile-config "cannot get line and column")]
        [(name line col)
         (make-source-object sfd (make-file-position-object bfp line col) efp)]))]
    [else
     (make-source-object sfd bfp efp)])))

(generate-wpo-files #t)
