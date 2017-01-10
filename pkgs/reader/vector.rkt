#lang racket/base
(require racket/fixnum
         racket/flonum
         "sequence.rkt"
         "wrap.rkt"
         "error.rkt"
         "consume.rkt"
         "config.rkt"
         "digit.rkt"
         "parameter.rkt"
         "accum-string.rkt"
         "number-literal.rkt")

(provide read-vector
         read-fixnum-or-flonum-vector)

(define (read-vector read-one opener-c opener closer in config 
                     #:mode [vector-mode 'any]
                     #:length [expected-len #f])
  (define read-one-element
    (case vector-mode
      [(any) read-one]
      [(fixnum) (lambda (in config) (read-fixnum read-one in config))]
      [(flonum) (lambda (in config) (read-flonum read-one in config))]))
  
  (define seq (read-unwrapped-sequence read-one-element
                                       opener-c #\( #\) in config
                                       #:whitespace-read-one read-one
                                       #:dot-mode #f))
  
  ;; Extend `seq` as needed to match the declared length
  (define vec
    (cond
     [(not expected-len)
      (case vector-mode
        [(any) (list->vector seq)]
        [(fixnum) (for/fxvector #:length (length seq) ([e (in-list seq)]) e)]
        [(flonum) (for/flvector #:length (length seq) ([e (in-list seq)]) e)])]
     [else
      (define len (length seq))
      (cond
       [(= expected-len len) (list->vector seq)]
       [(expected-len . < . len)
        (reader-error in config
                      "~avector length ~a is too small, ~a values provided"
                      (case vector-mode
                        [(any) ""]
                        [(fixnum) "fx"]
                        [(flonum) "fl"])
                      expected-len len)]
       [else
        (define (last-or v)
          (if (null? seq)
              v
              (let loop ([seq seq])
                (if (null? (cdr seq)) (car seq) (loop (cdr seq))))))
        (define vec
          (case vector-mode
            [(any) (make-vector expected-len (last-or 0))]
            [(fixnum) (make-fxvector expected-len (last-or 0))]
            [(flonum) (make-flvector expected-len (last-or 0.0))]))
        (case vector-mode
          [(any) (for ([e (in-list seq)]
                       [i (in-naturals)])
                   (vector-set! vec i e))]
          [(fixnum) (for ([e (in-list seq)]
                          [i (in-naturals)])
                      (fxvector-set! vec i e))]
          [(flonum) (for ([e (in-list seq)]
                          [i (in-naturals)])
                      (flvector-set! vec i e))])
        vec])]))
       
  (wrap vec
        in
        config
        opener))

;; ----------------------------------------

(define (read-fixnum-or-flonum-vector read-one dispatch-c c c2 in config)
  (define vector-mode (if (char=? c2 #\x) 'fixnum 'flonum))
  (consume-char in c2)
  (when (read-config-for-syntax? config)
    (reader-error in config "literal f~avectors not allowed" c2))
  
  (define c3 (read-char-or-special in))
  (define-values (vector-len len-str c4)
    (cond
     [(decimal-digit? c3) (read-simple-number in config c3)]
     [else (values #f "" c3)]))
  
  (define-syntax-rule (guard-legal e c body ...)
    (cond
     [e body ...]
     [else (bad-syntax-error in config (format "~a~a" dispatch-c c))]))
  
  (case c4
    [(#\()
     (read-vector read-one #\{ #\} in config #:mode vector-mode #:length vector-len)]
    [(#\[)
     (guard-legal
      (check-parameter read-square-bracket-as-paren config)
      (format "~a~a" c c2)
      (read-vector read-one #\[ #\] in config #:mode vector-mode #:length vector-len))]
    [(#\{)
     (guard-legal
      (check-parameter read-curly-brace-as-paren config)
      (format "~a~a" c c2)
      (read-vector read-one #\{ #\} in config #:mode vector-mode #:length vector-len))]
    [else
     (reader-error in config #:eof (eof-object? c4)
                   "expected `(`, `[`, or `{` after `#~a~a~a`"
                   c c2 len-str)]))


(define (read-simple-number in config init-c)
  (define accum-str (accum-string-init! config))
  (define v (read-digits in config accum-str
                         #:base 10 #:max-count +inf.0))
  (values v
          (accum-string-get! accum-str config)
          ;; We could avoid some peeks vising init-c
          ;; and having `read-digit` return its peek
          ;; result, but we don't for now
          (peek-char-or-special in)))