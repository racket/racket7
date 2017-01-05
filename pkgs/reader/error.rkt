#lang racket/base
(require "config.rkt")

(provide reader-error)

(define (reader-error in config
                      #:eof? [eof? #f]
                      str . args)
  (define msg (string-append "read: " (apply format str args)))
  (raise
   ((if eof? exn:fail:read:eof exn:fail:read)
    msg
    (current-continuation-marks)
    (list (port+config->srcloc in config)))))
