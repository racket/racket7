#lang racket/base
(require "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide prop:buffer-mode
         file-stream-buffer-mode)

(define-values (prop:buffer-mode buffer-mode? buffer-mode-ref)
  (make-struct-type-property 'buffer-mode))

(define file-stream-buffer-mode
  (case-lambda
    [(p)
     (let ([p (cond
                [(input-port? p) (->core-input-port p)]
                [(output-port? p) (->core-output-port p)]
                [else
                 (raise-argument-error 'file-stream-buffer-mode "port?" p)])])
       (define data (core-port-data p))
       (and (buffer-mode? data)
            ((buffer-mode-ref data) data)))]
    [(p mode)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error 'file-stream-buffer-mode "port?" p))
     (unless (or (eq? mode 'none) (eq? mode 'line) (eq? mode 'block))
       (raise-argument-error 'file-stream-buffer-mode "(or/c 'none 'line 'block)" mode))
     (define (set-buffer-mode p)
       (define data (core-port-data p))
       (cond
         [(buffer-mode? data)
          ((buffer-mode-ref data) data mode)
          #t]
         [else #f]))
     (cond
       [(input-port? p)
        (or (set-buffer-mode (->core-input-port p))
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for input port"
                                   "mode" mode
                                   "input port" p))]
       [else
        (or (set-buffer-mode (->core-output-port p))
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for output port"
                                   "mode" mode
                                   "output port" p))])]))
