#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../path/path.rkt"
         "../file/parameter.rkt"
         "../file/host.rkt"
         "../file/error.rkt"
         "../format/main.rkt"
         "host-port.rkt"
         "close.rkt"
         "parameter.rkt"
         "count.rkt")

(provide open-input-file
         open-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file)

(define none (gensym))

(define/who (open-input-file path [mode1 none] [mode2 none])
  (check who path-string? path)
  (define (mode->flags mode)
    (case mode
      [(text) RKTIO_OPEN_TEXT]
      [else 0]))
  (define host-path (->host path))
  (start-atomic)
  (check-current-custodian who)
  (define fd (rktio_open rktio
                         (->rktio host-path)
                         (+ RKTIO_OPEN_READ
                            (mode->flags mode1)
                            (mode->flags mode2))))
  (when (rktio-error? fd)
    (end-atomic)
    (raise-filesystem-error who
                            fd
                            (format (string-append
                                     "cannot open input file\n"
                                     "  path: ~a")
                                    (host-> host-path))))
  (define p (open-input-host fd (host-> host-path)))
  (end-atomic)
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define/who (open-output-file path [mode1 none] [mode2 none])
  (check who path-string? path)
  (define (mode->flags mode)
    (case mode
      [(test) RKTIO_OPEN_TEXT]
      [(truncate truncate/replace) (+ RKTIO_OPEN_TRUNCATE
                                      RKTIO_OPEN_CAN_EXIST)]
      [(must-truncate) (+ RKTIO_OPEN_TRUNCATE
                          RKTIO_OPEN_MUST_EXIST)]
      [(update) RKTIO_OPEN_CAN_EXIST]
      [(must-update) RKTIO_OPEN_MUST_EXIST]
      [else 0]))
  (define (mode? v)
    (or (eq? mode1 v) (eq? mode2 v)))
  (define host-path (->host path))
  (start-atomic)
  (check-current-custodian who)
  (define fd0
    (rktio_open rktio
                (->rktio host-path)
                (+ RKTIO_OPEN_WRITE
                   (mode->flags mode1)
                   (mode->flags mode2))))
  (define fd
    (cond
      [(not (rktio-error? fd0)) fd0]
      [(and (or (racket-error? fd0 RKTIO_ERROR_EXISTS)
                (racket-error? fd0 RKTIO_ERROR_ACCESS_DENIED))
            (or (mode? 'replace) (mode? 'truncate/replace)))
       (define r (rktio_delete_file rktio
                                    (->rktio host-path)
                                    (current-force-delete-permissions)))
       (when (rktio-error? r)
         (end-atomic)
         (raise-filesystem-error who
                                 r
                                 (format (string-append
                                          "error deleting file\n"
                                          "  path: ~a")
                                         (host-> host-path))))
       (rktio_open rktio
                   (->rktio host-path)
                   (+ RKTIO_OPEN_WRITE
                      (mode->flags mode1)
                      (mode->flags mode2)))]
      [else fd0]))
  (when (rktio-error? fd)
    (end-atomic)
    (raise-filesystem-error who
                            fd
                            (format (string-append
                                     "~a\n"
                                     "  path: ~a")
                                    (cond
                                      [(racket-error? fd0 RKTIO_ERROR_EXISTS)
                                       "file exists"]
                                      [(racket-error? fd0 RKTIO_ERROR_IS_A_DIRECTORY)
                                       "path is a directory"]
                                      [else "error opening file"])
                                    (host-> host-path))))
  
  (define p (open-output-host fd (host-> host-path)))
  (end-atomic)
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define/who (call-with-input-file path proc [mode none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 1) proc)
  (define i (open-input-file path mode))
  (begin0
   (proc i)
   (close-input-port i)))

(define/who (call-with-output-file path proc [mode1 none] [mode2 none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 1) proc)
  (define o (open-output-file path mode1 mode2))
  (begin0
   (proc o)
   (close-output-port o)))

(define/who (with-input-from-file path proc [mode none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 0) proc)
  (define i (open-input-file path mode))
  (parameterize ([current-input-port i])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-input-port i)))))

(define/who (with-output-to-file path proc [mode1 none] [mode2 none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 0) proc)
  (define o (open-output-file path mode1 mode2))
  (parameterize ([current-output-port o])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-output-port o)))))
