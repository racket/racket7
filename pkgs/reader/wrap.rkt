#lang racket/base
(require "config.rkt")

(provide wrap)

(define (wrap s-exp in config)
  (define wrap (read-config-wrap config))
  (if wrap
      (wrap s-exp (port+config->srcloc in config))
      s-exp))
