#lang racket/base
(require "../host/rktio.rkt")

(provide bytes-no-nuls?
         bytes-environment-variable-name?
         normalize-key)

(define (bytes-no-nuls? s)
  (and (bytes? s)
       (not (for/or ([c (in-bytes s)])
              (= c 0)))))

(define (bytes-environment-variable-name? k)
  (and (bytes? k)
       (rktio_is_ok_envvar_name rktio k)))

(define (normalize-key k)
  (if (rktio_are_envvar_names_case_insensitive rktio)
      (string->immutable-string (string-foldcase k))
      k))
