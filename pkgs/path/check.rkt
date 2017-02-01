#lang racket/base
(require (for-syntax racket/base))
(provide check
         check-convention
         check-path-string
         check-path-bytes)

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ who pred #:contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who pred v)
     #`(check who pred #:contract #,(format "~a" (syntax->datum #'pred)) v)]))

(define (check-convention who c)
  (check who (lambda (c) (or (eq? c 'windows) (eq? c 'unix)))
         #:contract "(or/c 'windows 'unix)"
         c))

(define (check-path-string who s)
  (when (zero? (string-length s))
    (raise-arguments-error who "path string is empty"))
  (for ([c (in-string s)])
    (when (char=? c #\nul)
      (raise-arguments-error who "path string contains a nul character"
                             "path string" s))))

(define (check-path-bytes who s)
  (when (zero? (bytes-length s))
    (raise-arguments-error who "byte string is empty"))
  (for ([c (in-bytes s)])
    (when (zero? c)
      (raise-arguments-error who "byte string contains a nul character"
                             "byte string" s))))

