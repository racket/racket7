#lang racket/base
(require "main.rkt")

(define apple (string->bytes/utf-8 "ap\u3BBple"))
(define elppa (list->bytes (reverse (bytes->list (string->bytes/utf-8 "ap\u3BBple")))))

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
 (let loop ([j 1000])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "/Users/mflatt/Desktop/exp/port.rktl"))
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
       (define p (open-input-file "/Users/mflatt/Desktop/exp/port.rktl"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))
