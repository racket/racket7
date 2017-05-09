#lang racket/base
(require (only-in racket/base
                  [write-bytes host:write-bytes]
                  [current-error-port host:current-error-port])
         "../common/atomic.rkt"
         "../common/check.rkt"
         "../../common/queue.rkt"
         "../host/evt.rkt"
         "../string/convert.rkt"
         "level.rkt"
         "logger.rkt")

(provide (struct-out log-receiver)
         make-log-receiver
         add-stderr-log-receiver!
         log-receiver-send!)

(struct log-receiver (filters))

(define-values (prop:receiver-send receiver-send? receiver-send-ref)
  (make-struct-type-property 'receiver-send))

;; ----------------------------------------

(struct queue-log-receiver log-receiver (msgs     ; queue of messages ready for `sync` [if `waiters` is null]
                                         waiters) ; queue of (box callback) to receive ready messages [if `msgs` is null]
  #:reflection-name 'log-receiver
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode
    (define b (queue-remove! (queue-log-receiver-waiters lr)))
    (cond
      [b
       (define select! (unbox b))
       (set-box! b msg)
       (select!)]
      [else
       (queue-add! (queue-log-receiver-msgs lr) msg)]))
  #:property
  prop:evt
  (poller (lambda (lr ctx)
            (define msg (queue-remove! (queue-log-receiver-msgs lr)))
            (cond
              [msg
               (values (list msg) #f)]
              [else
               (define b (box (poll-ctx-select-proc ctx)))
               (define n (queue-add! (queue-log-receiver-waiters lr) b))
               (values #f (control-state-evt
                           (wrap-evt (async-evt) (lambda (e) (unbox b)))
                           (lambda () (queue-remove-node! (queue-log-receiver-waiters lr) n))
                           void
                           (lambda ()
                             (define msg (queue-remove! (queue-log-receiver-msgs lr)))
                             (cond
                               [msg
                                (set-box! b msg)
                                (values msg #t)]
                               [else
                                (set! n (queue-add! (queue-log-receiver-waiters lr) b))
                                (values #f #f)]))))]))))

(define (make-log-receiver logger level . args)
  (check 'make-log-receiver logger? logger)
  (define lr (queue-log-receiver (parse-filters 'make-log-receiver (cons level args) #:default-level 'none)
                                 (make-queue)
                                 (make-queue)))
  (add-log-receiver! logger lr)
  lr)

;; ----------------------------------------

(struct stderr-log-receiver log-receiver ()
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode
    (define stderr (host:current-error-port))
    (host:write-bytes (string->bytes/utf-8 (vector-ref msg 1)) stderr)
    (host:write-bytes #"\n" stderr)))

(define (add-stderr-log-receiver! logger . args)
  (check 'add-strerr-log-receiver! logger? logger)
  (define lr (stderr-log-receiver (parse-filters 'make-stderr-log-receiver args #:default-level 'none)))
  (atomically
   (add-log-receiver! logger lr)
   (set-logger-permanent-receivers! logger (cons lr (logger-permanent-receivers logger)))))

;; ----------------------------------------

(define (add-log-receiver! logger lr)
  (atomically
   ;; Add receiver to the logger's list, purning empty boxes
   ;; every time the list length doubles (roughly):
   (cond
     [(zero? (logger-prune-counter logger))
      (set-logger-receiver-boxes! logger (cons (make-weak-box lr)
                                               (for/list ([b (in-list (logger-receiver-boxes logger))]
                                                          #:when (weak-box-value b))
                                                 b)))
      (set-logger-prune-counter! logger (max 8 (length (logger-receiver-boxes logger))))]
     [else
      (set-logger-receiver-boxes! logger (cons (make-weak-box lr) (logger-receiver-boxes logger)))
      (set-logger-prune-counter! logger (sub1 (logger-prune-counter logger)))])
   ;; Increment the timestamp, so that wanted levels will be
   ;; recomputed on demand:
   (define ts-box (logger-root-level-timestamp-box logger))
   (set-box! ts-box (add1 (unbox ts-box)))
   ;; Post a semaphore to report that wanted levels may have
   ;; changed:
   (when (logger-level-sema logger)
     (semaphore-post (logger-level-sema logger))
     (set-logger-level-sema! logger #f))))
         
(define (log-receiver-send! r msg)
  ((receiver-send-ref r) r msg))
