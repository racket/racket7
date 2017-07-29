#lang racket/base
(require "../common/check.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide file-position
         file-position*)

(define/who file-position
  (case-lambda
    [(p)
     (do-simple-file-position who p
                              (lambda ()
                                (raise
                                 (exn:fail:filesystem
                                  (string-append
                                   "file-position: the port's current position is not known\n port: "
                                   ((error-value->string-handler) p (error-print-width)))
                                  (current-continuation-marks)))))]
    [(p pos)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error who "port?" p))
     (check who
            (lambda (p) (or (exact-nonnegative-integer? p) (eof-object? p)))
            #:contract "(or/c exact-nonnegative-integer? eof-object?)"
            pos)
     (let ([cp (cond
                 [(input-port? p) (->core-input-port p)]
                 [else (->core-output-port p)])])
       (define file-position (core-port-file-position cp))
       (cond
         [(and (procedure? file-position) (procedure-arity-includes? file-position 1))
          (file-position pos)]
         [else
          (raise-arguments-error who
                                 "setting position allowed for file-stream and string ports only"
                                 "port" p
                                 "position" pos)]))]))

(define/who (file-position* p)
  (do-simple-file-position who p (lambda () #f)))

(define (do-simple-file-position who p fail-k)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else (raise-argument-error who "port?" p)])])
    (define file-position (core-port-file-position p))
    (cond
      [(or (input-port? file-position)
           (output-port? file-position))
       (do-simple-file-position who file-position fail-k)]
      [else
       (or (if file-position
               (file-position)
               (core-port-offset p))
           (fail-k))])))
