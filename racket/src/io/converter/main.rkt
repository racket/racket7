#lang racket/base
(require "../common/check.rkt")

(provide bytes-converter?
         bytes-open-converter
         bytes-close-converter
         bytes-convert
         bytes-convert-end)

(struct bytes-converter ())

(define/who (bytes-open-converter from-str to-str)
  (check who string? from-str)
  (check who string? to-str)
  (error who "not ready"))

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
