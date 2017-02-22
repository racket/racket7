#lang racket/base

(provide make-internal-pipe
         nonempty-internal-pipe?
         read-some-internal-pipe-bytes!
         peek-some-internal-pipe-bytes!)

(define (make-internal-pipe)
  (void))

(define (nonempty-internal-pipe? p skip)
  #f)

(define (read-some-internal-pipe-bytes! who pipe bstr start end)
  0)

(define (peek-some-internal-pipe-bytes! who pipe bstr start end)
  0)
