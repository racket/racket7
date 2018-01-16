#lang racket/base
(require racket/cmdline
         racket/file
         compiler/private/mach-o)

(command-line
 #:args (src-file dest-file boot-dir racket.so)

 (define bstr1 (file->bytes (build-path boot-dir "petite.boot")))
 (define bstr2 (file->bytes (build-path boot-dir "scheme.boot")))
 (define bstr3 (file->bytes racket.so))

 (with-handlers ([exn? (lambda (x)
                         (when (file-exists? dest-file)
                           (delete-file dest-file))
                         (raise x))])
   (copy-file src-file dest-file #t)
   (define pos (add-plt-segment dest-file (bytes-append bstr1 #"\0"
                                                        bstr2 #"\0"
                                                        bstr3 #"\0")))

   (define-values (i o) (open-input-output-file dest-file #:exists 'update))
   (define m (regexp-match-positions #rx"BooT FilE OffsetS:" i))
   (unless m
     (error 'embed-boot "cannot file boot-file offset tag"))
   
   (file-position o (cdar m))
   (void (write-bytes (integer->integer-bytes pos 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) 1) 4 #t #f) o))
   (void (write-bytes (integer->integer-bytes (+ pos (bytes-length bstr1) (bytes-length bstr2) 2) 4 #t #f) o))))
