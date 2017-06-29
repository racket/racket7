#lang racket/base
(require "bootstrap-main.rkt"
         (only-in racket/base
                  [string->bytes/utf-8 host:string->bytes/utf-8]
                  [bytes->string/utf-8 host:bytes->string/utf-8]
                  [open-input-file host:open-input-file]
                  [close-input-port host:close-input-port]
                  [read-line host:read-line]
                  [read-byte host:read-byte]
                  [file-stream-buffer-mode host:file-stream-buffer-mode]
                  [port-count-lines! host:port-count-lines!]
                  [current-directory host:current-directory]
                  [path->string host:path->string]))

(current-directory (host:path->string (host:current-directory)))

(define-syntax-rule (test expect rhs)
  (let ([e expect]
        [v rhs])
    (unless (equal? e v)
      (error 'failed "~s: ~e" 'rhs v))))

(test #t (file-exists? "demo.rkt"))
(test #f (file-exists? "compiled"))
(test #f (file-exists? "compiled/demo-file"))

(test #t (directory-exists? "compiled"))
(test #f (directory-exists? "compiled/demo-dir"))

(test #f (link-exists? "compiled"))
(test #f (link-exists? "compiled/demo-dir"))

(call-with-output-file "compiled/demo-file" void)
(call-with-output-file "compiled/demo-file" void 'replace)
(let ([now (current-seconds)]
      [f-now (file-or-directory-modify-seconds "compiled/demo-file")])
  (test #t (<= (- now 10) f-now now))
  (file-or-directory-modify-seconds "compiled/demo-file" (- now 5))
  (test (- now 5) (file-or-directory-modify-seconds "compiled/demo-file")))
(rename-file-or-directory "compiled/demo-file" "compiled/demo-file2")
(delete-file "compiled/demo-file2")

(test 88 (file-or-directory-modify-seconds "compiled/bad" #f (lambda () 88)))
(test 89 (file-or-directory-modify-seconds "compiled/bad" (current-seconds) (lambda () 89)))

(test #t (and (memq 'read (file-or-directory-permissions "demo.rkt")) #t))
(test #t (and (memq 'read (file-or-directory-permissions "compiled")) #t))

(printf "~s\n" (filesystem-root-list))
(printf "~s\n" (directory-list))
(make-directory "compiled/demo-dir")
(delete-directory "compiled/demo-dir")

(printf "demo.rkt = ~s\n" (file-or-directory-identity "demo.rkt"))
(test (file-or-directory-identity "demo.rkt") (file-or-directory-identity "demo.rkt"))
(test #f (= (file-or-directory-identity "compiled") (file-or-directory-identity "demo.rkt")))

(test (call-with-input-file "demo.rkt"
        (lambda (i)
          (let loop ([n 0])
            (if (eof-object? (read-byte i))
                n
                (loop (add1 n))))))
      (file-size "demo.rkt"))

(copy-file "demo.rkt" "compiled/demo-copy" #t)
(test (file-size "demo.rkt")
      (file-size "compiled/demo-copy"))
(test (file-or-directory-permissions "demo.rkt" 'bits)
      (file-or-directory-permissions "compiled/demo-copy" 'bits))
(delete-file "compiled/demo-copy")

(make-file-or-directory-link "../demo.rkt" "compiled/also-demo.rkt")
(test #t (link-exists? "compiled/also-demo.rkt"))
(test (string->path "../demo.rkt") (resolve-path "compiled/also-demo.rkt"))
(delete-file "compiled/also-demo.rkt")
(test #f (link-exists? "compiled/also-demo.rkt"))

(printf "~s\n" (expand-user-path "~/at-home"))

(struct animal (name weight)
  #:property prop:custom-write (lambda (v o mode)
                                 (fprintf o "<~a>" (animal-name v))))

(test "1\n0!\"hi\"" (format "1~%~  \n  ~o~c~s" 0 #\! "hi"))

(test "*(1 2 3 apple\t\u0001 end <spot> file 1\"2\"3 #hash((a . 1) (b . 2)))*"
      (format "*~a*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) ,(string->path "file") #"1\"2\"3" #hash((b . 2) (a . 1)))))
(test "*(1 2 3 \"apple\\t\\u0001\" end <spot> #\"1\\\"2\\\"3\t\\0010\")*"
      (format "*~.v*" `(1 2 3 "apple\t\001" end ,(animal 'spot 155) #"1\"2\"3\t\0010")))

(fprintf (current-output-port) "*~v*" '!!!)
(newline)

(test "no: hi 10"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s" 10)))

(test "error: format string requires 1 arguments, given 3"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s" 1 2 3)))
(test "error: format string requires 2 arguments, given 1"
      (with-handlers ([exn:fail? exn-message])
        (error 'no "hi ~s ~s" 8)))

(define infinite-ones 
  (make-input-port 'ones
                   (lambda (s) 
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   #f
                   void))

(test 49 (read-byte infinite-ones))
(test #\1 (read-char infinite-ones))
(test #"11111" (read-bytes 5 infinite-ones))
(test #"11111" (peek-bytes 5 3 infinite-ones))
(test #"11111" (read-bytes 5 infinite-ones))
(test "11111" (read-string 5 infinite-ones))

(define fancy-infinite-ones
  (make-input-port 'fancy-ones
                   (lambda (s)
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   (lambda (s skip progress-evt)
                     (bytes-set! s 0 (char->integer #\1))
                     1)
                   (lambda () (void))
                   (lambda () (make-semaphore))
                   (lambda (amt evt ext-evt) (make-bytes amt (char->integer #\1)))
                   (lambda () (values 7 42 1024))
                   (lambda () (void))
                   (lambda () 99)
                   (case-lambda
                     [() 'block]
                     [(m) (void)])))
(test #"11111" (read-bytes 5 fancy-infinite-ones))
(test #t (evt? (port-progress-evt fancy-infinite-ones)))
(test #t (port-commit-peeked 5 (port-progress-evt fancy-infinite-ones) always-evt fancy-infinite-ones))
(test '(#f #f #f) (call-with-values (lambda () (port-next-location fancy-infinite-ones)) list))
(port-count-lines! fancy-infinite-ones)
(test '(7 42 1024) (call-with-values (lambda () (port-next-location fancy-infinite-ones)) list))
(test 98 (file-position fancy-infinite-ones))
(test 'block (file-stream-buffer-mode fancy-infinite-ones))
(test (void) (file-stream-buffer-mode fancy-infinite-ones 'none))

(test "apλple" (bytes->string/utf-8 (string->bytes/utf-8 "!!ap\u3BBple__" #f 2) #f 0 7))
(test "ap?ple" (bytes->string/latin-1 (string->bytes/latin-1 "ap\u3BBple" (char->integer #\?))))
(test "apλp\uF7F8\U00101234le" (bytes->string/utf-8 (string->bytes/utf-8 "ap\u3BBp\uF7F8\U101234le")))

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
(test
 256
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
      (loop (add1 x) (cdr content) (list* bstr bstr accum))])))


(let ()
  (define path (build-path "compiled" "demo-out"))
  (define o (open-output-file path 'truncate))
  ;; We expect this to be buffered:
  (test 12 (write-bytes #"abcdefghijkl" o))
  (test 12 (file-position o))
  (test (void) (file-position o 6))
  (test 3 (write-bytes #"xyz" o))
  (test (void) (file-position o eof))
  (test 1 (write-bytes #"!" o))
  (close-output-port o)

  (test 13 (file-size path))

  (define i (open-input-file path))
  (test #"abcdefxyzjkl!" (read-bytes 20 i))
  (test (void) (file-position i 0))
  (test #"abcdef" (read-bytes 6 i))
  (test (void) (file-position i 9))
  (test #"jkl!" (read-bytes 6 i))
  (close-input-port i))

(let ()
  (define in (open-input-bytes #"hello"))
  (test 0 (file-position in))
  (test #"hel" (read-bytes 3 in))
  (test 3 (file-position in))
  (test (void) (file-position in 2))
  (test #"llo" (read-bytes 3 in))
  (test 5 (file-position in))
  (test eof (read-bytes 3 in))
  (test 5 (file-position in))
  (test (void) (file-position in eof))
  (test 5 (file-position in))
  (test (void) (file-position in 100))
  (test 5 (file-position in)))

(let ()
  (define out (open-output-bytes))
  (test 0 (file-position out))
  (write-bytes #"hello" out)
  (test 5 (file-position out))
  (test (void) (file-position out 1))
  (test 1 (file-position out))
  (write-bytes #"ola" out)
  (test 4 (file-position out))
  (test #"holao" (get-output-bytes out))
  (write-bytes #"!!" out)
  (test 6 (file-position out))
  (test #"hola!!" (get-output-bytes out))
  (test (void) (file-position out 10))
  (test #"hola!!\0\0\0\0" (get-output-bytes out)))

(let ()
  (define-values (i o) (make-pipe))
  (port-count-lines! i)
  (port-count-lines! o)
  (define (next-location p)
    (define-values (line col pos) (port-next-location p))
    (list line col pos))
  (test '(1 0 1) (next-location i))
  (test '(1 0 1) (next-location o))

  (write-bytes #"a\n b" o)
  (test '(2 2 5) (next-location o))

  (test #"a" (read-bytes 1 i))
  (test '(1 1 2) (next-location i))
  (test #"\n" (read-bytes 1 i))
  (test '(2 0 3) (next-location i))
  (test #" b" (read-bytes 2 i))
  (test '(2 2 5) (next-location i))

  (write-bytes #"x\r" o)
  (test '(3 0 7) (next-location o))
  (write-bytes #"\n" o)
  (test '(3 0 7) (next-location o))
  (write-bytes #"!" o)
  (test '(3 1 8) (next-location o))

  (test #"x\r" (read-bytes 2 i))
  (test '(3 0 7) (next-location i))
  (test #"\n!" (read-bytes 2 i))
  (test '(3 1 8) (next-location i)))

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

(define read-byte-buffer-mode 'block)

'read-byte/host
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (host:open-input-file "compiled/io.rktl"))
       (host:file-stream-buffer-mode p read-byte-buffer-mode)
       (host:port-count-lines! p)
       (let loop ()
         (unless (eof-object? (host:read-byte p))
           (loop)))
       (host:close-input-port p)
       (loop (sub1 j))))))

'read-byte
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (file-stream-buffer-mode p read-byte-buffer-mode)
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'read-line/host
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

'read-line
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

(test "a" (read-line (open-input-string "a")))
(test "a" (read-line (open-input-string "a\nb")))
(test "a" (read-line (open-input-string "a\r\nb") 'any))
(test "a" (read-line (open-input-string "a\rb") 'any))

(test #\l (bytes-utf-8-ref #"apple" 3))
(test #\λ (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 2))
(test #\p (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 3))
(test #\l (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 3 #\? 1))
(test #f (bytes-utf-8-ref (string->bytes/utf-8 "apλple") 6))

(test 4 (bytes-utf-8-index #"apple" 3))
(test 5 (bytes-utf-8-index (string->bytes/utf-8 "apλple") 3))
