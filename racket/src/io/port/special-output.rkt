#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "output-port.rkt"
         "parameter.rkt"
         "count.rkt")

(provide write-special
         write-special-avail*
         write-special-evt
         port-writes-special?)

(define/who (port-writes-special? o)
  (check who output-port? o)
  (let ([o (->core-output-port o)])
    (and (core-output-port-write-out-special o) #t)))

(define (do-write-special who v orig-o #:retry? retry?)
  (check who output-port? orig-o)
  (let port-loop ([o orig-o])
    (let ([o (->core-output-port orig-o)])
      (define write-out-special (core-output-port-write-out-special o))
      (unless write-out-special
        (raise-arguments-error who
                               "port does not support special values"
                               "port" orig-o))
      (cond
        [(output-port? write-out-special)
         (port-loop write-out-special)]
        [else
         (let loop ()
           (start-atomic)
           (define r (write-out-special v (not retry?) #f))
           (let result-loop ([r r])
             (cond
               [(not r)
                (end-atomic)
                (if retry?
                    (loop)
                    #f)]
               [(evt? r)
                (end-atomic)
                (and retry?
                     (result-loop (sync r)))]
               [else
                (port-count! o 1 #"x" 0)
                (end-atomic)
                #t])))]))))

(define/who (write-special v [o (current-output-port)])
  (do-write-special who #:retry? #t v o))

(define/who (write-special-avail* v [o (current-output-port)])
  (do-write-special who #:retry? #f v o))

(define/who (write-special-evt v [o (current-output-port)])
  (check who output-port? o)
  (let ([o (->core-output-port o)])
    (define get-write-special-evt (core-output-port-get-write-special-evt o))
    (unless get-write-special-evt
      (raise-arguments-error who
                             "port does not support special-value events"
                             "port" o))
    (get-write-special-evt v)))
