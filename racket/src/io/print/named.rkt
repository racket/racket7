#lang racket/base
(require "../port/string-output.rkt"
         "write-with-max.rkt"
         "symbol.rkt")

(provide print-named)

(define (print-named what v mode o max-length)
  (define name (object-name v))
  (let* ([max-length (write-string/max "#<" o max-length)]
         [max-length (write-string/max what o max-length)])
    (cond
      [(symbol? name)
       (let* ([max-length (write-string/max ":" o max-length)]
              [max-length (write-string/max (symbol->print-string name #:for-type? #t) o max-length)])
         (write-string/max ">" o max-length))]
      [else
       (write-string/max ">" o max-length)])))
