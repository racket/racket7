#lang racket/base

;; For internal errors

(provide abort)

(define (abort str)
  (raise (exn:fail str (current-continuation-marks))))
