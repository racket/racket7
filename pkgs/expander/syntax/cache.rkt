#lang racket/base
(require "../common/set.rkt")

(provide clear-resolve-cache!
         resolve-cache-get
         resolve-cache-set!)

(define cache (box (make-weak-box #f)))

(define clear-resolve-cache!
  (case-lambda
    [(sym)
     (define c (weak-box-value (unbox cache)))
     (when c
       (hash-remove! c sym))]
    [()
     (define c (weak-box-value (unbox cache)))
     (when c
       (hash-clear! c))]))

(struct entry (scs smss phase binding))

(define (resolve-cache-get sym phase scs smss)
  (define c (weak-box-value (unbox cache)))
  (and c
       (let ([v (hash-ref c sym #f)])
         (and v
              (eqv? phase (entry-phase v))
              (set=? scs (entry-scs v))
              (set=? smss (entry-smss v))
              (entry-binding v)))))

(define (resolve-cache-set! sym phase scs smss b)
  (define wb (unbox cache))
  (define c (weak-box-value wb))
  (cond
   [(not c)
    (box-cas! cache wb (make-weak-box (make-hasheq)))
    (resolve-cache-set! sym phase scs smss b)]
   [else
    (hash-set! c sym (entry scs smss phase b))]))
