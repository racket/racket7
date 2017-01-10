#lang racket/base
(require "config.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "string.rkt")

(provide read-regexp)

(define (read-regexp mode-c accum-str in config)
  (define c3 (read-char-or-special in))
  (define no-wrap-config (disable-wrapping config))
  
  (define rx
    (case c3
      [(#\")
       (accum-string-abandon! accum-str config)
       ((if (char=? mode-c #\r) regexp pregexp)
        (read-string in no-wrap-config))]
      [(#\#)
       (accum-string-add! accum-str c3)
       (define c4 (read-char-or-special in))
       (case c4
         [(#\")
          (accum-string-abandon! accum-str config)
          ((if (char=? mode-c #\r) byte-regexp byte-pregexp)
           (read-string in no-wrap-config #:mode '|byte string|))]
         [else
          (reader-error in config
                        "expected `\"` after `~a`"
                        (accum-string-get! accum-str config))])]
      [else
       (reader-error in config
                     "expected `\"` or `#` after `~a`"
                     (accum-string-get! accum-str config))]))
  
  (wrap rx
        in
        config
        #f))
