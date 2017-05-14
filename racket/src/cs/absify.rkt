#lang racket/base
(require racket/path)

(define arg0 (vector-ref (current-command-line-arguments) 0))
(define exec? (equal? arg0 "--exec"))

(define p
  (if exec?
      (let ([p (vector-ref (current-command-line-arguments) 1)])
        (if (path-element? (string->path p))
            (find-executable-path p)
            p))
      arg0))

(displayln
 (simplify-path (path->complete-path p)))
