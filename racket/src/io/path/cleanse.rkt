#lang racket/base
(require "../common/check.rkt"
         "../error/abort.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt")

(provide cleanse-path
         clean-double-slashes)

(define/who (cleanse-path p-in)
  (check-path-argument who p-in)
  (define p (->path p-in))
  (case (path-convention p)
    [(unix)
     (define bstr (clean-double-slashes (path-bytes p) 'unix 0))
     (if (eq? bstr (path-bytes p))
         p
         (path bstr 'unix))]
    [(windows)
     ;; FIXME
     (abort "Windows path cleanse")]))

;; ----------------------------------------

(define (clean-double-slashes bstr convention allow-double-before)
  (define extra-count
    (let loop ([i (sub1 (bytes-length bstr))])
      (cond
       [(= i allow-double-before) 0]
       [(and (is-sep? (bytes-ref bstr i) convention)
             (is-sep? (bytes-ref bstr (sub1 i)) convention))
        (add1 (loop (sub1 i)))]
       [else (loop (sub1 i))])))
  (cond
   [(zero? extra-count)
    bstr]
   [else
    (define new-bstr (make-bytes (- (bytes-length bstr) extra-count)))
    (let loop ([i (sub1 (bytes-length bstr))] [j (sub1 (bytes-length new-bstr))])
      (unless (= i allow-double-before)
        (cond
         [(and (is-sep? (bytes-ref bstr i) convention)
               (is-sep? (bytes-ref bstr (sub1 i)) convention))
          (loop (sub1 i) j)]
         [else
          (bytes-set! new-bstr j (bytes-ref bstr i))
          (loop (sub1 i) (sub1 j))])))
    (bytes-copy! new-bstr 0 bstr 0 (add1 allow-double-before))
    new-bstr]))

