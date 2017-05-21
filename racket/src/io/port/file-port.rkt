#lang racket/base
(require (only-in racket/base
                  [open-input-file host:open-input-file]
                  [open-output-file host:open-output-file])
         "../common/check.rkt"
         "../path/path.rkt"
         "../file/host.rkt"
         "host-port.rkt"
         "close.rkt"
         "parameter.rkt")

(provide open-input-file
         open-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file)

(define none (gensym))

(define (open-input-file path [mode1 none] [mode2 none])
  (check 'open-input-file path-string? path)
  (open-input-host (host:open-input-file (->host path))
                   path))

(define (open-output-file path [mode1 none] [mode2 none])
  (check 'open-output-file path-string? path)
  (open-output-host (host:open-output-file (->host path))
                    path))

(define (call-with-input-file path proc mode)
  (check 'call-with-input-file path-string? path)
  (check 'call-with-input-file (lambda (v) (and (procedure? v) (procedure-arity-includes? v 1)))
         #:contract "(procedure-arity-includes?/c 1)"
         proc)
  (define i (open-input-file path mode))
  (begin0
   (proc i)
   (close-input-port i)))

(define (call-with-output-file path proc [mode1 none] [mode2 none])
  (check 'call-with-output-file path-string? path)
  (check 'call-with-output-file (lambda (v) (and (procedure? v) (procedure-arity-includes? v 1)))
         #:contract "(procedure-arity-includes?/c 1)"
         proc)
  (define o (open-output-file path mode1 mode2))
  (begin0
   (proc o)
   (close-output-port o)))

(define (with-input-from-file path proc [mode none])
  (check 'with-input-from-file path-string? path)
  (check 'with-input-from-file (lambda (v) (and (procedure? v) (procedure-arity-includes? v 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (define i (open-input-file path mode))
  (parameterize ([current-input-port i])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-input-port i)))))

(define (with-output-to-file path proc [mode1 none] [mode2 none])
  (check 'with-output-to-file path-string? path)
  (check 'with-output-to-file (lambda (v) (and (procedure? v) (procedure-arity-includes? v 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (define o (open-output-file path mode1 mode2))
  (parameterize ([current-output-port o])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-output-port o)))))
