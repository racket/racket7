#lang racket/base
(require "check.rkt"
         (submod "thread.rkt" scheduling)
         (prefix-in engine: "engine.rkt"))

(provide current-process-milliseconds)

(define/who (current-process-milliseconds [scope #f])
  (cond
    [(not scope) (engine:current-process-milliseconds)]
    [(thread? scope) (thread-cpu-time scope)]
    [else
     (raise-argument-error who "(or/c #f thread?)" scope)]))
