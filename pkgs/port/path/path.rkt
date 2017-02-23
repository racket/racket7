#lang racket/base
(require "../print/custom-write.rkt"
         "../port/string-output.rkt"
         "../string/convert.rkt")

(provide (struct-out path)
         is-path?
         path-for-some-system?
         path-string?
         string->path
         ->path)

(struct path (bytes convention)
        #:property prop:custom-write
        (lambda (p port mode)
          (write-string "#<path:" port)
          (write-string (bytes->string/locale (path-bytes p)) port)
          (write-string ">" port)))

(define is-path?
  (let ([path? (lambda (p)
                 (and (path? p)
                      (eq? (path-convention p)
                           (system-path-convention-type))))])
    path?))

(define (path-for-some-system? p)
  (path? p))

(define (path-string? p)
  (or (is-path? p)
      (and (string? p)
           (positive? (string-length p))
           (for/and ([c (in-string p)])
             (not (char=? c #\nul))))))

(define (string->path s)
  (path (string->bytes/locale s (char->integer #\?))
        (system-path-convention-type)))

(define (->path p)
  (if (string? p)
      (string->path p)
      p))
