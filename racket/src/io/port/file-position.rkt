#lang racket/base
(require "../common/check.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide prop:file-position
         file-position
         file-position*)

(define-values (prop:file-position file-position? file-position-ref)
  (make-struct-type-property 'file-position))

(define/who file-position
  (case-lambda
    [(p)
     (do-simple-file-position who p
                              (lambda ()
                                (raise
                                 (exn:fail:filesystem
                                  (string-append
                                   "file-position: the port's current position is not known\n port: "
                                   ((error-value->string-handler) p (error-print-width)))))))]
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
       (define data (core-port-data cp))
       (cond
         [(file-position? data)
          ((core-port-on-file-position p))
          ((file-position-ref data) data pos)]
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
    (define data (core-port-data p))
    (if (file-position? data)
        ((file-position-ref data) data)
        (or (core-port-offset p)
            (fail-k)))))
