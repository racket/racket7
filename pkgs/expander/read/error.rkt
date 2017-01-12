#lang racket/base
(require "config.rkt")

(provide reader-error
         bad-syntax-error)

(define (reader-error in config
                      #:eof? [eof? #f]
                      str . args)
  (define msg (string-append "read"
                             (if (read-config-for-syntax? config) "-syntax" "")
                             ": "
                             (apply format str args)))
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

(define (bad-syntax-error in config str)
  (reader-error in config "bad syntax `~a`" str))
