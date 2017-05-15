#lang racket/base

(define-syntax-rule (define-boolean-parameter print-x init-val)
  (begin
    (provide print-x)
    (define print-x (make-parameter init-val (lambda (v) (and v #t))))))

(define-boolean-parameter print-graph #f)
(define-boolean-parameter print-struct #t)
(define-boolean-parameter print-box #t)
(define-boolean-parameter print-unreadable #t)
(define-boolean-parameter print-hash-table #t)
(define-boolean-parameter print-as-expression #f)
(define-boolean-parameter print-vector-length #f)
(define-boolean-parameter print-pair-curly-braces #f)
(define-boolean-parameter print-mpair-curly-braces #t)
(define-boolean-parameter print-boolean-long-form #f)
(define-boolean-parameter print-reader-abbreviations #t)
