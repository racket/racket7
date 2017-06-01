#lang racket/base
(require "path.rkt"
         "check.rkt"
         "check-path.rkt"
         "sep.rkt"
         "relativity.rkt"
         "split.rkt"
         "build.rkt"
         "cleanse.rkt"
         "directory-path.rkt")

(provide simplify-path)

(define/who (simplify-path p-in [use-filesystem? #t])
  (check-path-argument who p-in)
  (define p (->path p-in))
  (define convention (path-convention p))
  (cond
   [(simple? p convention) p]
   [else
    (define clean-p (cleanse-path p))
    (cond
     [(simple? clean-p convention) clean-p]
     [else
      (define l (explode-path clean-p))
      (define simpler-l
        (let loop ([l l] [accum null])
          (cond
           [(null? l) (reverse accum)]
           [(eq? 'same (car l)) (loop (cdr l) accum)]
           [(and (eq? 'up (car l)) (pair? accum))
            (loop (cdr l) (cdr accum))]
           [else (loop (cdr l) (cons (car l) accum))])))
      (define simple-p (apply build-path simpler-l))
      (if (directory-path? p)
          (path->directory-path simple-p)
          simple-p)])]))

;; ----------------------------------------

;; Quick check for whether the path is already simple:
(define (simple? p convention)
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (let loop ([i 0])
    (cond
     [(= i len) #t]
     [(is-sep? (bytes-ref bstr i) convention)
      (cond
       [(= (add1 i) len) #t]
       [(is-sep? (bytes-ref bstr (add1 i)) convention)
        #f]
       [(and (eq? (bytes-ref bstr (add1 i)) (char->integer #\.))
             (or (= (+ i 2) len)
                 (is-sep? (bytes-ref bstr (+ i 2)) convention)
                 (and (eq? (bytes-ref bstr (+ i 2)) (char->integer #\.))
                      (or (= (+ i 3) len)
                          (is-sep? (bytes-ref bstr (+ i 3)) convention)))))
        #f]
       [else (loop (add1 i))])]
     [(and (zero? i)
           (eq? (bytes-ref bstr 0) (char->integer #\.))
           (or (= 1 len)
               (is-sep? (bytes-ref bstr 1) convention)
               (and (eq? (bytes-ref bstr 1) (char->integer #\.))
                    (or (= 2 len)
                        (is-sep? (bytes-ref bstr 2) convention)))))
      #f]
     [else (loop (add1 i))])))
