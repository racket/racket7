#lang racket/base
(require "../common/contract.rkt"
         (rename-in "../syntax/read-syntax.rkt"
                    [read-syntax raw:read-syntax]
                    [read-syntax/recursive raw:read-syntax/recursive]
                    [read raw:read]
                    [read/recursive raw:read/recursive]))

(provide read-syntax
         read-syntax/recursive
         read
         read/recursive)

(define (read-syntax [src (object-name (current-input-port))] [in (current-input-port)])
  (check 'read-syntax input-port? in)
  (raw:read-syntax src in))

(define (read-syntax/recursive [src (object-name (current-input-port))]
                               [in (current-input-port)]
                               [start #f]
                               [readtable (current-readtable)]
                               [graph? #t])
  (check 'read-syntax/recursive input-port? in)
  (unless (or (char? start) (not start))
    (raise-argument-error 'read-syntax/recursive "(or/c char? #f)" start))
  (unless (or (readtable? readtable) (not readtable))
    (raise-argument-error 'read-syntax/recursive "(or/c readtable? #f)" readtable))
  (raw:read-syntax/recursive src in start readtable graph?))

(define (read [in (current-input-port)])
  (check 'read input-port? in)
  (raw:read in))

(define (read/recursive [in (current-input-port)]
                        [start #f]
                        [readtable (current-readtable)]
                        [graph? #t])
  (check 'read/recursive input-port? in)
  (unless (or (char? start) (not start))
    (raise-argument-error 'read/recursive "(or/c char? #f)" start))
  (unless (or (readtable? readtable) (not readtable))
    (raise-argument-error 'read/recursive "(or/c readtable? #f)" readtable))
  (raw:read/recursive in start readtable graph?))
