#lang racket/base
(require "../common/performance.rkt"
         "../host/reader-syntax-to-syntax.rkt"
         (only-in "../host/reader-syntax.rkt"
                  [read-syntax reader:read-syntax]
                  [read-syntax/recursive reader:read-syntax/recursive]))

(provide read-syntax
         read-syntax/recursive)

(define (read-syntax src in)
  (performance-region
   ['read]
   (reader-syntax->syntax (reader:read-syntax src in))))

(define (read-syntax/recursive src in start readtable graph?)
  (performance-region
   ['read]
   (reader-syntax->syntax (reader:read-syntax/recursive src in start readtable graph?))))
