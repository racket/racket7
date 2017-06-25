#lang racket/base
(require "rktio.rkt"
         "evt.rkt")

(provide format-rktio-message
         raise-rktio-error
         check-rktio-error
         check-rktio-error*)

(define (format-rktio-message who err base-msg)
  (start-atomic)
  (define p (rktio_get_error_string rktio
                                    (rktio-errkind err)
                                    (rktio-errno err)))
  (define system-msg (rktio_to_bytes p))
  (end-atomic)
  (format "~a~a~a\n  system error: ~a; ~a=~a"
          (or who "")
          (if who ": " "")
          base-msg
          system-msg
          (let ([kind (rktio-errkind err)])
            (cond
              [(eqv? kind RKTIO_ERROR_KIND_POSIX) "errno"]
              [(eqv? kind RKTIO_ERROR_KIND_WINDOWS) "win_err"]
              [(eqv? kind RKTIO_ERROR_KIND_GAI) "gai_err"]
              [else "rkt_err"]))
          (rktio-errno err)))

(define (raise-rktio-error who err base-msg)
  (raise
   (exn:fail
    (format-rktio-message who err base-msg)
    (current-continuation-marks))))

(define (check-rktio-error v base-msg)
  (when (rktio-error? v)
    (raise-rktio-error #f v base-msg))
  v)

(define (check-rktio-error* v base-msg)
  (check-rktio-error v base-msg)
  (void))
