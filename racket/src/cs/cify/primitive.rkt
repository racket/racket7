#lang racket/base

(provide direct-call-primitive?)

(define trouble-primitives
  (hasheq 'call-with-values #t
          'call-with-prompt #t
          'call-with-current-continuation #t
          'call-with-escape-continuation #t))

(define (direct-call-primitive? rator e)
  (not (or (hash-ref trouble-primitives rator #f)
           (case rator
             [(hash-ref)
              (and ((length e) . > . 2)
                   (immediate? (caddr e)))]
             [else #f]))))

(define (immediate? e)
  (or (boolean? e)
      (and (pair? e) (eq? 'quote (car e)))))
