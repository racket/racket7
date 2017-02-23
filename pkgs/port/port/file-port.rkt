#lang racket/base
(require (only-in racket/base
                  [open-input-file host:open-input-file])
         "../common/check.rkt"
         "../path/path.rkt"
         "host-port.rkt")

(provide open-input-file)

(define none (gensym))

(define (open-input-file path [mode1 none] [mode2 none])
  (check 'open-input-file path-string? path)
  (open-input-host (host:open-input-file (path->string (path->complete-path path)))
                   path))
