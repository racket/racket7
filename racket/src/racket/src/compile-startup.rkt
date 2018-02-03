(module compile-startup '#%kernel
  (#%require '#%linklet
             "help-startup.rkt")
  
  ;; Decode a linklet S-expression from "startup.inc" (in the source
  ;; directory), compile it, and write it back as "cstartup.inc" (in
  ;; the build directory)
  
  (define-values (dest) (vector-ref (current-command-line-arguments) 0))
  (define-values (src) (vector-ref (current-command-line-arguments) 1))
  (define-values (vers) (vector-ref (current-command-line-arguments) 2))
  (define-values (other-files) (cdddr (vector->list (current-command-line-arguments))))

  (define-values (linklet) (compile-linklet (get-linklet src)))
  (define-values (version-comparisons) (get-version-comparisons vers))

  ;; Bail out if we don't need to do anything:
  (if (file-exists? dest)
      (if (call-with-input-file dest (lambda (i)
                                       (begin
                                         (read-line i 'any)
                                         (not (eof-object? (read-line i 'any))))))
          (if (andmap (lambda (f)
                        ((file-or-directory-modify-seconds dest)
                         . > . 
                         (file-or-directory-modify-seconds f)))
                      (list* src vers other-files))
              (exit 0)
              (void))
          (void))
      (void))
    
  (define-values (DIGS-PER-LINE) 20)
  
  (call-with-output-file
   dest
   (lambda (outfile)
     (let-values ([(p) (open-output-bytes)])
       (write (hash->linklet-bundle (hasheq 'startup linklet)) p)
       (let-values ([(s) (get-output-bytes p)])
         (fprintf outfile "#if 0 ~a\n" version-comparisons)
         (fprintf outfile "# include \"startup.inc\"\n")         
         (fprintf outfile "#else\n")
         (fprintf outfile "static unsigned char expr[] = {\n")
         (letrec-values ([(loop)
                          (lambda (chars pos)
                            (if (null? chars)
                                (void)
                                (begin
                                  (fprintf outfile "~a," (car chars))
                                  (loop (cdr chars)
                                        (if (= pos DIGS-PER-LINE)
                                            (begin
                                              (newline outfile)
                                              0)
                                            (add1 pos))))))])
                        (loop (bytes->list s) 0))
         (fprintf outfile "0};\n")
         (fprintf outfile "# define EVAL_STARTUP EVAL_ONE_SIZED_STR((char *)expr, ~a)\n" (bytes-length s))
         (fprintf outfile "#endif\n"))))
   'truncate))
