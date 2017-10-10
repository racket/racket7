#lang racket/base
(require "../common/internal-error.rkt"
         "../common/check.rkt"
         "path.rkt"
         "sep.rkt")

(provide relative-path?
         absolute-path?
         complete-path?)

(define-syntax-rule (define-...-path? id 
                      unix-bstr-check unix-str-check)
  (define (id p)
    (check-path-test-argument 'id p)
    (cond
     [(path? p)
      (case (path-convention p)
        [(unix)
         (define bstr (path-bytes p))
         (unix-bstr-check bstr)]
        [(windows)
         (internal-error "much more here")])]
     [(string? p)
      (and (string-no-nuls? p)
           (positive? (string-length p))
           (case (system-path-convention-type)
             [(unix)
              (unix-str-check p)]
             [(windows)
              (internal-error "much more here")]))])))

(define (check-path-test-argument who p)
  (check who (lambda (p) (or (path? p) (string? p) (path-for-some-system? p)))
         #:contract "(or/c path? string? path-for-some-system?)"
         p))

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
