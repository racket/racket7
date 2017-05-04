#lang racket/base
(require "../error/abort.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt")

(provide directory-path?
         path->directory-path)

(define (path->directory-path p-in)
  (check-path-argument 'path->directory-path p-in)
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

