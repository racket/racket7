#lang racket/base

(provide (struct-out tail-return)
         (struct-out multiple-return)
         (struct-out multiple-return/suffix)
         return
         return-can-omit?)

(struct tail-return (function-id lam self-args))
(struct multiple-return (prefix))
(struct multiple-return/suffix multiple-return (generate-suffix))

(define (return indent ret e #:can-omit? [can-omit? #f])
  (unless (and can-omit? (return-can-omit? ret))
    (printf "~a~a ~a;\n"
            indent
            (cond
              [(tail-return? ret) "return"]
              [(multiple-return? ret) (multiple-return-prefix ret)]
              [else ret])
            e)
    (when (multiple-return/suffix? ret)
      ((multiple-return/suffix-generate-suffix ret) indent))))

(define (return-can-omit? ret)
  (or (equal? ret "")
      (and (multiple-return? ret)
           (equal? (multiple-return-prefix ret) ""))))

