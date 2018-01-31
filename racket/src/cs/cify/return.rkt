#lang racket/base
(require "out.rkt")

(provide (struct-out tail-return)
         (struct-out multiple-return)
         (struct-out multiple-return/suffix)
         return
         return-can-omit?)

(struct tail-return (function-id lam self-args))
(struct multiple-return (prefix))
(struct multiple-return/suffix multiple-return (generate-suffix))

(define (return ret e #:can-omit? [can-omit? #f])
  (unless (and can-omit? (return-can-omit? ret))
    (out "~a ~a;"
         (cond
           [(tail-return? ret) "return"]
           [(multiple-return? ret) (multiple-return-prefix ret)]
           [else ret])
         e)
    (when (multiple-return/suffix? ret)
      ((multiple-return/suffix-generate-suffix ret)))))

(define (return-can-omit? ret)
  (or (equal? ret "")
      (and (multiple-return? ret)
           (equal? (multiple-return-prefix ret) ""))))

