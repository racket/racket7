#lang racket/base
(require "../common/check.rkt"
         "output-port.rkt"
         "bytes-port.rkt"
         "../string/convert.rkt")

(provide open-input-string
         open-output-string
         get-output-string)

(define (open-input-string str [name 'string])
  (check 'open-input-string string? str)
  (open-input-bytes (string->bytes/utf-8 str) name))

(define (open-output-string [name 'string])
  (open-output-bytes name))

(define (get-output-string o)
  (check 'get-output-string (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (bytes->string/utf-8 (get-output-bytes o) #\?))
