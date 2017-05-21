#lang racket/base
(require "../common/check.rkt"
         "output-port.rkt"
         "parameter.rkt")

(provide write-special
         write-special-avail*
         write-special-evt
         port-writes-special?)

(define (port-writes-special? o)
  (check 'port-writes-special? output-port? o)
  (let ([o (->core-output-port o)])
    (and (core-output-port-write-out-special o) #t)))

(define (do-write-special who v o #:retry? retry?)
  (check who output-port? o)
  (let ([o (->core-output-port o)])
    (define write-out-special (core-output-port-write-out-special o))
    (unless write-out-special
      (raise-arguments-error who
                             "port does not support special values"
                             "port" o))
    (cond
      [(output-port? write-out-special)
       (do-write-special who v write-out-special #:retry? retry?)]
      [else
       (let loop ()
         (define r (write-out-special v #f #f))
         (cond
           [(not r) (if retry?
                        (loop)
                        #f)]
           [(evt? r)
            (if retry?
                (void (sync r))
                #f)]
           [else
            (if retry?
                #t
                (void))]))])))

(define (write-special v [o (current-output-port)])
  (do-write-special 'write-special #:retry? #t v o))

(define (write-special-avail* v [o (current-output-port)])
  (do-write-special 'write-special-avail* #:retry? #f v o))

(define (write-special-evt v [o (current-output-port)])
  (check 'write-special-evt output-port? o)
  (let ([o (->core-output-port o)])
    (define get-write-special-evt (core-output-port-get-write-special-evt o))
    (unless get-write-special-evt
      (raise-arguments-error 'write-special-evt
                             "port does not support special-value events"
                             "port" o))
    (get-write-special-evt v)))
