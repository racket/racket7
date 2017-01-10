#lang racket/base

(provide special-comment?
         make-special-comment
         special-comment-value)

(define-struct special-comment (value))

