#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/convert.rkt")

(provide encoding->bytes
         locale-encoding-is-utf-8?)

;; in atomic mode
(define (encoding->bytes who str)
  (cond
    [(equal? str "")
     (define e (rktio_locale_encoding rktio))
     (cond
       [(rktio-error? e)
        (end-atomic)
        (raise-rktio-error who e "error getting locale encoding")]
       [else
        (begin0
          (rktio_to_bytes e)
          (rktio_free e))])]
    [else
     (string->bytes/utf-8 str (char->integer #\?))]))

(define (locale-encoding-is-utf-8?)
  (define t (system-type))
  (or
   (eq? t 'macosx)
   (eq? t 'windows)
   (zero? (bitwise-and (rktio_convert_properties rktio) RKTIO_CONVERTER_SUPPORTED))))
