#lang racket/base
(require "config.rkt")

(provide reader-error)

(define (reader-error in config
                      #:eof? [eof? #f]
                      str . args)
  (define msg (string-append "read: " (apply format str args)))
  (define srcloc (port+config->srcloc in config))
  (raise
   ((if eof? exn:fail:read:eof exn:fail:read)
    (let ([s (and (error-print-source-location)
                  (srcloc->string srcloc))])
      (if s
          (string-append s ": " msg)
          msg))
    (current-continuation-marks)
    (list srcloc))))
