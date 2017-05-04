#lang racket/base
(require "../error/abort.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt")

(provide relative-path?
         absolute-path?
         complete-path?)

(define-syntax-rule (define-...-path? id 
                      unix-bstr-check unix-str-check)
  (define (id p)
    (check-path-argument 'id p)
    (cond
     [(path? p)
      (case (path-convention p)
        [(unix)
         (define bstr (path-bytes p))
         (unix-bstr-check bstr)]
        [(windows)
         (abort "much more here")])]
     [(string? p)
      (case (system-path-convention-type)
        [(unix)
         (unix-str-check p)]
        [(windows)
         (abort "much more here")])])))

(define-...-path? relative-path?
  (lambda (p) 
    (not (is-sep? (bytes-ref p 0) 'unix)))
  (lambda (p)
    (not (is-sep? (char->integer (string-ref p 0)) 'unix))))

(define-...-path? absolute-path?
  (lambda (p) 
    (is-sep? (bytes-ref p 0) 'unix))
  (lambda (p)
    (is-sep? (char->integer (string-ref p 0)) 'unix)))

(define-...-path? complete-path?
  (lambda (p) 
    (is-sep? (bytes-ref p 0) 'unix))
  (lambda (p)
    (is-sep? (char->integer (string-ref p 0)) 'unix)))
