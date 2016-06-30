(module sstoinc '#%kernel
  (#%require '#%linklet)
  
  ;; Decode a linklet S-expression from "startup.inc" (in the source
  ;; directory), compile it, and write it back as "cstartup.inc" (in
  ;; the build directory)
  
  (define-values (dest) (vector-ref (current-command-line-arguments) 0))
  (define-values (src) (vector-ref (current-command-line-arguments) 1))
  
  (define-values (get-lines)
    (lambda (in)
      (let-values ([(l) (read-line in 'any)])
        (if (eof-object? l)
            null
            (cons l (get-lines in))))))
  
  (define-values (linklet)
    (compile-linklet
     (read
      (open-input-string
       (apply
        string-append
        (map (lambda (l)
               (regexp-replace* #rx"\\\\(.)"
                                (substring l 1 (sub1 (string-length l)))
                                "\\1"))
             (reverse (cdr (reverse (cdr (call-with-input-file src get-lines)))))))))))
  
  (define-values (DIGS-PER-LINE) 20)
  
  (call-with-output-file
   dest
   (lambda (outfile)
     (let-values ([(p) (open-output-bytes)])
       (write (hash->linklet-bundle (hasheq 'startup linklet)) p)
       (let-values ([(s) (get-output-bytes p)])
         (fprintf outfile "  {\n    SHARED_OK static unsigned char expr[] = {")
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
         (fprintf outfile "0};\n    EVAL_ONE_SIZED_STR((char *)expr, ~a);\n" (bytes-length s))
         (fprintf outfile "  }\n"))))
   'truncate)) 
