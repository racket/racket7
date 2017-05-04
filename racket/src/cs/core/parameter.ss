
;; Continuation-mark key:
(define parameterization-key (gensym "parameterization-key"))

(define-record parameterization (ht))

(define empty-parameterization (make-parameterization empty-hasheq))

(define (extend-parameterization p . args)
  (unless (parameterization? p)
    (raise-argument-error 'extend-parameterization "parameterization?" p))
  (let loop ([ht (parameterization-ht p)] [args args])
    (cond
     [(null? args) (make-parameterization ht)]
     [else (loop (hamt-set ht (car args) (make-thread-cell (cadr args) #t))
                 (cddr args))])))

(define (current-parameterization)
  (continuation-mark-set-first
   #f
   parameterization-key
   empty-parameterization
   the-root-continuation-prompt-tag))

(define (parameter-cell key)
  (hamt-ref (parameterization-ht
             (current-parameterization))
            key
            #f))

(define make-parameter
  (case-lambda
    [(v) (make-parameter v #f)]
    [(v guard)
     (let ([default-c (make-thread-cell v #t)])
       (define self
         (case-lambda
           [() 
            (let ([c (or (parameter-cell self)
                         default-c)])
              (thread-cell-ref c))]
           [(v)
            (let ([c (or (parameter-cell self)
                         default-c)])
              (thread-cell-set! c (if guard
                                      (guard v)
                                      v)))]))
       self)]))
