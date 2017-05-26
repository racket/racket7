#lang racket/base
(require "../common/check.rkt"
         "../error/abort.rkt"
         "../port/output-port.rkt"
         "../port/string-output.rkt"
         "../port/bytes-output.rkt"
         "../port/parameter.rkt"
         "custom-write.rkt"
         "write-with-max.rkt"
         "string.rkt"
         "bytes.rkt"
         "symbol.rkt"
         "char.rkt"
         "parameter.rkt")

(provide display
         write
         print

         newline
         
         prop:custom-write
         custom-write?
         custom-write-accessor

         prop:custom-print-quotable
         custom-print-quotable?
         custom-print-quotable-accessor

         (all-from-out "parameter.rkt"))

(define (display v [o (current-output-port)] [max-length #f])
  (check 'display output-port? o)
  (check 'display max-length? #:contract max-length-contract max-length)
  (dots (p 'display v #f o (sub3 max-length)) o)
  (void))

(define (write v [o (current-output-port)] [max-length #f])
  (check 'write output-port? o)
  (check 'write max-length? #:contract max-length-contract max-length)
  (dots (p 'write v #t o (sub3 max-length)) o)
  (void))

(define (print v [o (current-output-port)] [quote-depth 0] [max-length #f])
  (check 'print output-port? o)
  (check 'print (lambda (v) (or (eq? v 0) (eq? v 1))) #:contract "(or/c 0 1)" quote-depth)
  (check 'print max-length? #:contract max-length-contract max-length)
  (dots (p 'print v 0 o (sub3 max-length)) o)
  (void))

(define (newline [o (current-output-port)])
  (check 'newline output-port? o)
  (write-bytes #"\n" o)
  (void))

;; ----------------------------------------

(define (max-length? v)
  (or (not v)
      (and (exact-nonnegative-integer? v)
           (v . >= . 3))))

(define max-length-contract "(or/c #f (and/c exact-integer? (>=/c 3)))")

(define (sub3 n) (and n (- n 3)))

(define (dots max-length o)
  (when (eq? max-length 'full)
    (write-string "..." o)))

;; ----------------------------------------

;; Returns the max length that is still available
(define (p who v mode o max-length)
  (cond
   [(eq? max-length 'full) 'full]
   [(number? v)
    (write-string/max (number->string v) o max-length)]
   [(string? v)
    (case mode
      [(#f) (write-string/max v o max-length)]
      [else (print-string v o max-length)])]
   [(bytes? v)
    (case mode
      [(#f) (write-bytes/max v o max-length)]
      [else (print-bytes v o max-length)])]
   [(symbol? v)
    (case mode
      [(#f) (write-string/max (symbol->string v) o max-length)]
      [else (print-symbol v o max-length)])]
   [(keyword? v)
    (let ([max-length (write-string/max "#:" o max-length)])
      (case mode
        [(#f) (write-string/max (keyword->string v) o max-length)]
        [else (print-symbol (string->symbol (keyword->string v)) o max-length)]))]
   [(char? v)
    (case mode
      [(#f) (write-string/max (string v) o max-length)]
      [else (print-char v o max-length)])]
   [(not v)
    (write-string/max "#f" o max-length)]
   [(eq? v #t)
    (write-string/max "#t" o max-length)]
   [(pair? v)
    (let loop ([v v] [max-length (write-string/max "(" o max-length)])
      (cond
       [(eq? max-length 'full) 'full]
       [(null? (cdr v))
        (let ([max-length (p who (car v) mode o max-length)])
          (write-string/max ")" o max-length))]
       [(pair? (cdr v))
        (let ([max-length (p who (car v) mode o max-length)])
          (loop (cdr v) (write-string/max " " o max-length)))]
       [else
        (let* ([max-length (p who (car v) mode o max-length)]
               [max-length (write-string/max " . " o max-length)]
               [max-length (p who (cdr v) mode o max-length)])
          (write-string/max ")" o max-length))]))]
   [(custom-write? v)
    (let ([o (make-output-port/max o max-length)])
      ((custom-write-accessor v) v o mode)
      (output-port/max-max-length o max-length))]
   [else
    ;; As a last resort, fall back to the host `format`:
    (write-string/max (format "~s" v) o max-length)]))
