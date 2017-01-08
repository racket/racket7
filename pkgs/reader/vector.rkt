#lang racket/base
(require "sequence.rkt"
         "wrap.rkt"
         "error.rkt"
         "number-literal.rkt"
         racket/fixnum
         racket/flonum)

(provide read-vector)

(define (read-vector read-one opener closer in config 
                     #:mode [vector-mode 'any]
                     #:length [expected-len #f])
  (define read-one-element
    (case vector-mode
      [(any) read-one]
      [(fixnum) read-fixnum]
      [(flonum) read-flonum]))
  
  (define seq (read-unwrapped-sequence read-one-element #\( #\) in config
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
       [(expected-len . > . len)
        (reader-error in config
                      "~avector length ~a is too small, ~a values provided"
                      (case vector-mode
                        [(any) ""]
                        [(fixnum) "fx"]
                        [(flonum) "fl"])
                      expected-len len)]
       [else
        (define vec
          (case vector-mode
            [(any) (make-vector expected-len 0)]
            [(fixnum) (make-fxvector expected-len)]
            [(flonum) (make-flvector expected-len)]))
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
