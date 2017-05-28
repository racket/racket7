#lang racket/base
(require "check.rkt"
         "evt.rkt"
         "sync.rkt")

;; The core must provide `will-try-execute`. We implement
;; `will-execute` here, because it has to block when no will is ready.

(provide will-execute)

(define (will-execute we)
  (check 'will-execute will-executor? we)
  ;; block on a polling event:
  (sync (will-execute-evt we)))

;; Implement the polling event:
(struct will-execute-evt (we)
  #:property
  prop:evt
  (poller (lambda (evt poll-ctx)
            (define results
              (call-with-values (lambda ()
                                  (will-try-execute (will-execute-evt-we evt)
                                                    none))
                list))
            (if (and (pair? results) (eq? none (car results)))
                (values #f evt)
                (values results #f)))))

(define none (gensym 'none))
