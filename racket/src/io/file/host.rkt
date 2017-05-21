#lang racket/base
(require "../path/path.rkt"
         "../path/complete.rkt"
         "../path/cleanse.rkt"
         (only-in racket/base
                  [bytes->path host:bytes->path]
                  [path->bytes host:path->bytes]))

(provide ->host
         ->host/as-is
         host->)

(define (->host p)
  (host:bytes->path (path-bytes (cleanse-path (path->complete-path p)))))

(define (->host/as-is p)
  (let ([p (if (string? p) (string->path p) p)])
    (host:bytes->path (path-bytes p))))

(define (host-> s)
  (path (bytes->immutable-bytes (host:path->bytes s))
        (system-path-convention-type)))
