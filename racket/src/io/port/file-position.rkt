#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide prop:file-position
         file-position
         file-position*)

(define-values (prop:file-position file-position? file-position-ref)
  (make-struct-type-property 'file-position))

(define file-position
  (case-lambda
    [(p)
     (do-simple-file-position 'file-position p
                              (lambda ()
                                (raise
                                 (exn:fail:filesystem
                                  (string-append
                                   "file-position: the port's current position is not known\n port: "
                                   ((error-value->string-handler) p (error-print-width)))))))]
    [(p pos)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error 'file-position "port?" p))
     (check 'file-position exact-nonnegative-integer? pos)
     (cond
       [(input-port? p)
        (let ([p (->core-input-port p)])
          (define data (core-input-port-data p))
          (cond
            [(file-position? data)
             ((core-input-port-on-file-position p))
             ((file-position-ref data) data pos)]
            [else
             (raise-arguments-error 'file-position
                                    "setting position allowed for file-stream and string ports only"
                                    "port" p
                                    "position" pos)]))]
       [else
        (let ([p (->core-output-port p)])
          (define data (core-output-port-data p))
          (cond
            [(file-position? data)
             ((file-position-ref data) data pos)]
            [else
             (raise-arguments-error 'file-position
                                    "setting position allowed for file-stream and string ports only"
                                    "port" p
                                    "position" pos)]))])]))

(define (file-position* p)
  (do-simple-file-position 'file-position* p (lambda () #f)))

(define (do-simple-file-position who p fail-k)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (define data (core-input-port-data p))
       (if (file-position? data)
           ((file-position-ref data) data)
           (or (core-input-port-offset p)
               (fail-k))))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (define data (core-output-port-data p))
       (if (file-position? data)
           ((file-position-ref data) data)
           (or (core-output-port-offset p)
               (fail-k))))]
    [else
     (raise-argument-error who "port?" p)]))
