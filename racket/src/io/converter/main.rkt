#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/convert.rkt")

(provide bytes-converter?
         bytes-open-converter
         bytes-close-converter
         bytes-convert
         bytes-convert-end)

(struct bytes-converter ([c #:mutable]))

(define/who (bytes-open-converter from-str to-str)
  (check who string? from-str)
  (check who string? to-str)
  (define props (rktio_convert_properties rktio))
  (cond
    [(zero? (bitwise-and props RKTIO_CONVERTER_SUPPORTED))
     #f]
    [else
     (define c (rktio_converter_open rktio
                                     (string->bytes/utf-8 from-str #\?)
                                     (string->bytes/utf-8 to-str #\?)))
     (when (rktio-error? c)
       (raise-rktio-error who c "failed"))
     (bytes-converter c)]))

(define (bytes-close-converter converter)
  (void))

(define (bytes-convert converter	 
                       src-bstr
                       [src-start-pos 0]
                       [src-end-pos (and (bytes? src-bstr) (bytes-length src-bstr))]
                       [dest-bstr #f]
                       [dest-start-pos 0]
                       [dest-end-pos (and (bytes? dest-bstr) (bytes-length dest-bstr))])
  (void))

(define (bytes-convert-end converter
                           [dest-bstr #f]
                           [dest-start-pos 0]
                           [dest-end-pos (and (bytes? dest-bstr) (bytes-length dest-bstr))])
  (void))
