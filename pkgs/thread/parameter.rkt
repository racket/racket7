#lang racket/base
(require "engine.rkt")

(provide current-thread)

(define current-thread (make-global-parameter #f))

