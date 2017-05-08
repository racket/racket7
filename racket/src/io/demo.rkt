#lang racket/base
(require "main.rkt"
         (only-in racket/base
                  [string->bytes/utf-8 host:string->bytes/utf-8]
                  [bytes->string/utf-8 host:bytes->string/utf-8]
                  [open-input-file host:open-input-file]
                  [close-input-port host:close-input-port]
                  [read-line host:read-line]))

(struct animal (name weight)
        #:property prop:custom-write (lambda (v o mode)
                                       (fprintf o "<~a>" (animal-name v))))

(format "1~%~  \n  ~o~c~s" 0 #\! "hi")

(format "*~a*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) ,(string->path "file") #"1\"2\"3"))
(format "*~.v*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) #"1\"2\"3\t\0010"))

(fprintf (current-output-port) "*~v*" '!!!)
(newline)

(with-handlers ([exn:fail? exn-message])
  (error 'no "hi ~s" 10))

(with-handlers ([exn:fail? exn-message])
  (error 'no "hi ~s" 1 2 3))

(define infinite-ones 
  (make-input-port 'ones
                   (lambda (s) 
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   #f
                   void))

(read-byte infinite-ones)
(read-char infinite-ones)
(read-bytes 5 infinite-ones)
(peek-bytes 5 3 infinite-ones)
(read-bytes 5 infinite-ones)
(read-string 5 infinite-ones)

(bytes->string/utf-8 (string->bytes/utf-8 "!!ap\u3BBple__" #f 2) #f 0 7)
(bytes->string/latin-1 (string->bytes/latin-1 "ap\u3BBple" (char->integer #\?)))
(bytes->string/utf-8 (string->bytes/utf-8 "ap\u3BBp\uF7F8\U101234le"))

(define apple (string->bytes/utf-8 "ap\u3BBple"))
(define elppa (list->bytes (reverse (bytes->list (string->bytes/utf-8 "ap\u3BBple")))))

(let ()
  (define-values (i o) (make-pipe))
  (for ([n 3])
    (write-bytes (make-bytes 4096 (char->integer #\a)) o)
    (for ([j (in-range 4096)])
      (read-byte i))
    (unless (zero? (pipe-content-length i))
      (error "pipe loop failed\n"))))

(define p (open-input-bytes apple))
(define-values (i o) (make-pipe))

(void (write-bytes #"x" o))
(let loop ([x 1] [content '(#"x")] [accum null])
  (cond
   [(= x 256) x]
   [(null? content)
    (loop x (reverse accum) null)]
   [else
    (define bstr (list->bytes
                  (for/list ([j (in-range x)])
                    (modulo j 256))))
    (write-bytes bstr o)
    (write-bytes bstr o)
    (unless (equal? (read-bytes (bytes-length (car content)) i)
                    (car content))
      (error))
    (loop (add1 x) (cdr content) (list* bstr bstr accum))]))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (port-count-lines! p)
       (let loop ()
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'read-line
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (host:open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (host:read-line p))
           (loop)))
       (host:close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (read-line p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'encoding
(time
 (for/fold ([v #f]) ([i (in-range 1000000)])
   (bytes->string/utf-8 (string->bytes/utf-8 "ap\u3BBple"))))
(time
 (for/fold ([v #f]) ([i (in-range 1000000)])
   (host:bytes->string/utf-8 (host:string->bytes/utf-8 "ap\u3BBple"))))

(read-line (open-input-string "a"))
(read-line (open-input-string "a\nb"))
(read-line (open-input-string "a\r\nb") 'any)
(read-line (open-input-string "a\rb") 'any)
