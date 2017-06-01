
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
     [(and (parameter? (car args))
           (pair? (cdr args)))
      (let dloop ([p (car args)] [v (cadr args)])
        (if (derived-parameter? p)
            (dloop (derived-parameter-next p) ((derived-parameter-guard p) v))
            (loop (hamt-set ht p (make-thread-cell v #t))
                  (cddr args))))]
     [(parameter? (car args))
      (raise-arguments-error 'extend-parameterization
                             "missing value for parameter"
                             "parameter" (car args))]
     [else
      (raise-argument-error 'extend-parameterization "parameter?" (car args))])))
     
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

(define-record-type (parameter create-parameter parameter?)
  (fields proc))

(define-record-type (derived-parameter create-derived-parameter derived-parameter?)
  (parent parameter)
  (fields next guard))

(define make-parameter
  (case-lambda
    [(v) (make-parameter v #f)]
    [(v guard)
     (let ([default-c (make-thread-cell v #t)])
       (define self
         (create-parameter
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
                                      v)))])))
       self)]))

(define/who (make-derived-parameter p guard wrap)
  (check who parameter? p)
  (check who (procedure-arity-includes/c 1) guard)
  (check who (procedure-arity-includes/c 1) wrap)
  (create-derived-parameter (let ([self (parameter-proc p)])
                              (case-lambda
                               [(v) (self (guard v))]
                               [() (wrap (self))]))
                            p
                            guard))

(define/who (parameter-procedure=? a b)
  (check who parameter? a)
  (check who parameter? b)
  (eq? (strip-impersonator a) (strip-impersonator b)))

;; ----------------------------------------

(define/who current-inspector
  (make-parameter root-inspector
                  (lambda (v)
                    (check who inspector? v)
                    v)))
