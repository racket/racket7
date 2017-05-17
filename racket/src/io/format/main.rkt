#lang racket/base
(require "../common/check.rkt"
         "../port/parameter.rkt"
         "../port/output-port.rkt"
         "../port/string-port.rkt"
         "printf.rkt")

(provide format
         fprintf
         printf
         eprintf)

(define (format fmt . args)
  (check 'format string? fmt)
  (define o (open-output-string))
  (do-printf 'printf o fmt args)
  (get-output-string o))

(define (fprintf o fmt . args)
  (check 'fprintf output-port? o)
  (check 'fprintf string? fmt)
  (do-printf 'fprintf o fmt args))

(define (printf fmt . args)
  (check 'printf string? fmt)
  (do-printf 'printf (current-output-port) fmt args))

(define (eprintf fmt . args)
  (check 'eprintf string? fmt)
  (do-printf 'eprintf (current-error-port) fmt args))
