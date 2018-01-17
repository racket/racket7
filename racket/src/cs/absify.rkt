#lang racket/base
(require racket/path)

(define arg0 (vector-ref (current-command-line-arguments) 0))
(define exec? (equal? arg0 "--exec"))

(define p-arg-k (if exec? 1 0))

(define p
  (if exec?
      (let ([p (vector-ref (current-command-line-arguments) 1)])
        (if (path-element? (string->path p))
            (or (find-executable-path p)
                p)
            p))
      arg0))

(display (simplify-path (path->complete-path p)))

;; In case there are extra arguments to an executable, preserve them
(for ([e (in-vector (current-command-line-arguments) (add1 p-arg-k))])
  (display " ")
  (display e))

(newline)
