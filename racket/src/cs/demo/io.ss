(import (io))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ([total 0])
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop (+ total (string-length s)))))
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([i 1000000] [v #f])
   (if (zero? i)
       v
       (loop (sub1 i)
             (bytes->string/utf-8 (string->bytes/utf-8 "ap\x3BB;ple"))))))
