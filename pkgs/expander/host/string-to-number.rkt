#lang racket/base
(require "linklet.rkt")

;; Get host implementation of `string->number` for very basic number
;; parsing. Going through `primitive-table` prevents the reference
;; from being tied back to out implementation here when flattening the
;; expander+reader.

(provide string->number)

(define string->number (hash-ref (primitive-table '#%kernel) 'string->number))
