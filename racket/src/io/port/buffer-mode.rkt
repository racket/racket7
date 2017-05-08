#lang racket/base
(require "input-port.rkt"
         "output-port.rkt")

(provide prop:buffer-mode
         file-stream-buffer-mode)

(define-values (prop:buffer-mode buffer-mode? buffer-mode-ref)
  (make-struct-type-property 'buffer-mode))

(define file-stream-buffer-mode
  (case-lambda
    [(p)
     (cond
       [(input-port? p)
        (define data (input-port-data p))
        (and (buffer-mode? data)
             ((buffer-mode-ref data) data))]
       [(output-port? p)
        (define data (output-port-data p))
        (and (buffer-mode? data)
             ((buffer-mode-ref data) data))]
       [else
        (raise-argument-error 'file-stream-buffer-mode "port?" p)])]
    [(p mode)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error 'file-stream-buffer-mode "port?" p))
     (unless (or (eq? mode 'none) (eq? mode 'line) (eq? mode 'block))
       (raise-argument-error 'file-stream-buffer-mode "(or/c 'none 'line 'block)" mode))
     (cond
       [(input-port? p)
        (define data (input-port-data p))
        (if (buffer-mode? data)
            ((buffer-mode-ref data) data mode)
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for input port"
                                   "mode" mode
                                   "input port" p))]
       [else
        (define data (output-port-data p))
        (if (buffer-mode? data)
            ((buffer-mode-ref data) data mode)
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for output port"
                                   "mode" mode
                                   "output port" p))])]))
