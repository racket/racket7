#lang racket/base
(require "engine.rkt")

(provide current-thread)

(define current-thread (internal-make-thread-parameter #f))
