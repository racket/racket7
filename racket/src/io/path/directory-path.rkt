#lang racket/base
(require "../common/check.rkt"
         "../error/abort.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt")

(provide directory-path?
         path->directory-path
         path->path-without-trailing-separator)

(define/who (path->directory-path p-in)
  (check-path-argument who p-in)
  (define p (->path p-in))
  (cond
   [(directory-path? p) p]
   [else
    (case (path-convention p)
      [(unix)
       (path (bytes-append (path-bytes p) #"/") 'unix)]
      [else
       (abort "path->dir-path for Windows")])]))

(define (directory-path? p)
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (case (path-convention p)
    [(unix)
     (or (is-sep? (bytes-ref bstr (sub1 len)) 'unix)
         (and (len . >= . 2)
              (eq? (bytes-ref bstr (sub1 len)) (char->integer #\.))
              (eq? (bytes-ref bstr (- len 2)) (char->integer #\.))
              (or (len . = . 2)
                  (is-sep? (bytes-ref bstr (- len 3)) 'unix)))
         (and (len . >= . 1)
              (eq? (bytes-ref bstr (sub1 len)) (char->integer #\.))
              (or (len . = . 1)
                  (is-sep? (bytes-ref bstr (- len 2)) 'unix))))]
    [else
     ;; FIXME
     (abort "dir-path? for Windows")]))

(define (path->path-without-trailing-separator p)
  (define bstr (path-bytes p))
  (define orig-len (bytes-length bstr))
  (define len
    (let loop ([len orig-len])
      (cond
        [(zero? len) 0]
        [else
         (define c (bytes-ref bstr (sub1 len)))
         (if (is-sep? c (path-convention p))
             (loop (sub1 len))
             len)])))
  (cond
    [(< len orig-len) (path (subbytes bstr 0 len) (path-convention p))]
    [else p]))

    
