#lang racket/base
(require "../path/path.rkt"
         "../path/complete.rkt"
         "../path/parameter.rkt"
         "../path/cleanse.rkt"
         "../host/rktio.rkt")

(provide ->host
         ->host/as-is
         host->)

;; Note: `(host-> (->host x))` is not the same as `x`, since it
;; normalizes `x`. That's why `(host-> (->host x))` is generally used
;; in error reporting.

(define (->host p)
  (path-bytes (cleanse-path (path->complete-path p (current-directory)))))

(define (->host/as-is p)
  (let ([p (if (string? p) (string->path p) p)])
    (path-bytes p)))

(define (host-> s)
  (path (bytes->immutable-bytes s)
        (system-path-convention-type)))
