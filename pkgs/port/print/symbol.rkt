#lang racket/base
(require "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "write-with-max.rkt")

(provide print-symbol)

(define (print-symbol sym o max-length)
  (define str (symbol->string sym))
  (write-string/max str o max-length))
