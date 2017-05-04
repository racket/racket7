#lang racket/base

(displayln
 (simplify-path (path->complete-path (vector-ref (current-command-line-arguments) 0))))
