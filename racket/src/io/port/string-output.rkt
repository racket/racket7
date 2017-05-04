#lang racket/base
(require "../common/check.rkt"
         "output-port.rkt"
         "../string/convert.rkt"
         (submod "bytes-output.rkt" internal))

(provide write-string)

(define (write-string str [out (current-output-port)] [start 0] [end (and (string? str)
                                                                          (string-length str))])
  (check 'write-string string? str)
  (check 'write-string output-port? out)
  (check 'write-string exact-nonnegative-integer? start)
  (check 'write-string exact-nonnegative-integer? end)
  (check-range 'write-string start end (string-length str) str)
  (let loop ([i start])
    (cond
     [(= i end) (- i start)]
     [else
      (define next-i (min end (+ i 4096)))
      (define bstr (string->bytes/utf-8 str 0 i next-i))
      (do-write-bytes 'write-string out bstr 0 (bytes-length bstr))
      (loop next-i)])))
