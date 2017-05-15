#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "../print/main.rkt")

(provide port-read-handler
         port-write-handler
         port-display-handler
         port-print-handler

         install-reader!)

(define port-read-handler
  (case-lambda
    [(i)
     (check 'port-read-handler input-port? i)
     (let ([i (->core-input-port i)])
       (or (core-input-port-read-handler i)
           default-port-read-handler))]
    [(i h)
     (check 'port-read-handler input-port? i)
     (check 'port-read-handler (lambda (p)
                                  (and (procedure? p)
                                       (procedure-arity-includes? p 2)
                                       (procedure-arity-includes? p 3)))
            #:contract "(and/c (procedure-arity-includes/c 2) (procedure-arity-includes/c 3))"
            h)
     (let ([i (->core-input-port i)])
       (set-core-input-port-read-handler! i h))]))

(define default-port-read-handler
  (case-lambda
    [(i)
     (check 'default-port-read-handler input-port? i)
     (installed-read i)]
    [(i src)
     (check 'default-port-read-handler input-port? i)
     (installed-read-syntax src i)]))

(define installed-read #f)
(define installed-read-syntax #f)

(define (install-reader! read read-syntax)
  (set! installed-read read)
  (set! installed-read-syntax read-syntax))

;; ----------------------------------------

(define port-write-handler
  (case-lambda
    [(o)
     (check 'port-write-handler output-port? o)
     (let ([o (->core-input-port o)])
       (or (core-output-port-write-handler o)
           default-port-write-handler))]
    [(o h)
     (check 'port-write-handler output-port? o)
     (check 'port-write-handler (lambda (p)
                                  (and (procedure? p)
                                       (procedure-arity-includes? p 2)))
            #:contract "(procedure-arity-includes/c 2)"
            h)
     (let ([o (->core-input-port o)])
       (set-core-output-port-write-handler! o h))]))

(define (default-port-write-handler v o)
  (check 'default-port-write-handler output-port? o)
  (write v o))

(define port-display-handler
  (case-lambda
    [(o)
     (check 'port-display-handler output-port? o)
     (let ([o (->core-input-port o)])
       (or (core-output-port-display-handler o)
           default-port-display-handler))]
    [(o h)
     (check 'port-display-handler output-port? o)
     (check 'port-display-handler (lambda (p)
                                  (and (procedure? p)
                                       (procedure-arity-includes? p 2)))
            #:contract "(procedure-arity-includes/c 2)"
            h)
     (let ([o (->core-input-port o)])
       (set-core-output-port-display-handler! o h))]))

(define (default-port-display-handler v o)
  (check 'default-port-display-handler output-port? o)
  (display v o))

(define port-print-handler
  (case-lambda
    [(o)
     (check 'port-print-handler output-port? o)
     (let ([o (->core-input-port o)])
       (or (core-output-port-print-handler o)
           default-port-print-handler))]
    [(o h)
     (check 'port-print-handler output-port? o)
     (check 'port-print-handler (lambda (p)
                                  (and (procedure? p)
                                       (procedure-arity-includes? p 2)))
            #:contract "(procedure-arity-includes/c 2)"
            h)
     (let ([o (->core-input-port o)])
       (set-core-output-port-print-handler! o (if (procedure-arity-includes? h 3)
                                                  h
                                                  (lambda (v o [w #f]) (h v o)))))]))

(define (default-port-print-handler v o [quote-depth 0])
  (check 'default-port-print-handler output-port? o)
  (check 'default-port-print-handler (lambda (d) (or (eq? d 0) (eq? d 1)))
         #:contract "(or/c 0 1)"
         quote-depth)
  (print v o quote-depth))
