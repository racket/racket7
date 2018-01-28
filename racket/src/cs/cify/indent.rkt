#lang racket/base

(provide tab)

(define (tab indent)
  (string-append "  " indent))
