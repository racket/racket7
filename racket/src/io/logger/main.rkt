#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "logger.rkt"
         "level.rkt"
         "wanted.rkt"
         "receiver.rkt")

(provide logger?
         logger-name
         current-logger
         make-logger
         log-level?
         log-max-level
         log-all-levels
         log-level-evt
         log-message
         log-receiver?
         make-log-receiver
         add-stderr-log-receiver!)

(define root-logger
  (create-logger #:topic #f #:parent #f #:propagate-filters 'none))

(define current-logger
  (make-parameter root-logger
                  (lambda (l)
                    (unless (logger? l)
                      (raise-argument-error 'current-logger "logger?" l))
                    l)))

(define (make-logger [topic #f] [parent #f] . filters)
  (unless (or (not topic) (symbol? topic))
    (raise-argument-error 'make-logger "(or/c symbol? #f)" topic))
  (unless (or (not parent) (logger? parent))
    (raise-argument-error 'make-logger "(or/c logger? #f)" parent))
  (create-logger #:topic topic
                 #:parent parent
                 #:propagate-filters (parse-filters 'make-logger filters #:default-level 'debug)))

(define/who (log-level? logger level [topic #f])
  (check who logger? logger)
  (check-level who level)
  (check who #:or-false symbol? topic)
  (level>=? (logger-wanted-level logger topic) level))

(define/who (log-max-level logger [topic #f])
  (check who logger? logger)
  (check who #:or-false symbol? topic)
  (logger-wanted-level logger topic))

(define/who (log-all-levels logger)
  (check who logger? logger)
  (logger-all-levels logger))

(define/who (log-level-evt logger)
  (check who logger? logger)
  (define s
    (atomically
     (cond
       [(logger-level-sema logger)
        => (lambda (s) s)]
       [else
        (define s (make-semaphore))
        (set-logger-level-sema! logger s)])))
  (semaphore-peek-evt s))

(define/who log-message
  ;; Complex dispatch based on number and whether third is a string:
  (case-lambda
    [(logger level message data)
     (define topic (and (logger? logger) (logger-name logger)))
     (do-log-message who logger level topic message data #t)]
    [(logger level topic/message message/data data/prefix?)
     (cond
       [(string? topic/message)
        (define topic (and (logger? logger) (logger-name logger)))
        (do-log-message who logger level topic topic/message message/data data/prefix?)]
       [(symbol? topic/message)
        (do-log-message who logger level topic/message message/data data/prefix? #t)]
       [else
        (check who logger? logger)
        (check-level who level)
        (raise-argument-error who "(or/c string? symbol?)" topic/message)])]
    [(logger level topic message data prefix?)
     (do-log-message who logger level topic message data prefix?)]))

(define (do-log-message who logger level topic message data prefix?)
  (check who logger? logger)
  (check-level who level)
  (check who #:or-false symbol? topic)
  (check who string? message)
  (define msg #f)
  (atomically
   (when ((logger-max-wanted-level logger) . level>=? . level)
     (let loop ([logger logger])
       (for ([r (in-list (logger-receivers logger))])
         (when ((filters-level-for-topic (log-receiver-filters r) topic) . level>=? . level)
           (unless msg
             (set! msg (vector-immutable
                        level
                        (string->immutable-string
                         (if (and prefix? topic)
                             (string-append (symbol->string topic)
                                            ": "
                                            message)
                             message))
                        data
                        topic)))
           (log-receiver-send! r msg)))
       (let ([parent (logger-parent logger)])
         (when (and parent
                    ((filters-level-for-topic (logger-propagate-filters logger) topic) . level>=? . level))
           (loop parent)))))))
