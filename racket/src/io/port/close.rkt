#lang racket/base
(require "../common/check.rkt"
         "../common/atomic.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide port-closed?
         close-input-port
         close-output-port
         port-closed-evt)

(define (port-closed? p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (core-input-port-closed? p))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (core-output-port-closed? p))]
    [else
     (raise-argument-error 'close-input-port "port?" p)]))

(define/who (close-input-port p)
  (check who input-port? p)
  (let ([p (->core-input-port p)])
    (unless (core-input-port-closed? p)
      (set-core-input-port-closed?! p #t)
      (let ([s (core-input-port-closed-sema p)])
        (when s (semaphore-post s)))
      ((core-input-port-close p)))))

(define/who (close-output-port p)
  (check who output-port? p)
  (let ([p (->core-output-port p)])
    (unless (core-output-port-closed? p)
      (set-core-output-port-closed?! p #t)
      (let ([s (core-output-port-closed-sema p)])
        (when s (semaphore-post s)))
      ((core-output-port-close p)))))

(define (port-closed-evt p)
  (define sema
    (cond
      [(input-port? p)
       (let ([p (->core-output-port p)])
         (atomically
          (or (core-input-port-closed-sema p)
              (let ([s (make-semaphore)])
                (set-core-input-port-closed-sema! p s)
                (when (core-input-port-closed? p)
                  (semaphore-post s))
                s))))]
      [(output-port? p)
       (let ([p (->core-output-port p)])
         (atomically
          (or (core-output-port-closed-sema p)
              (let ([s (make-semaphore)])
                (set-core-output-port-closed-sema! p s)
                (when (core-output-port-closed? p)
                  (semaphore-post s))
                s))))]
      [else
       (raise-argument-error 'close-input-port "port?" p)]))
  (define self (wrap-evt (semaphore-peek-evt sema)
                         (lambda (v) self)))
  self)
