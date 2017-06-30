#lang racket/base
(require "../../thread/sandman-struct.rkt"
         "../common/internal-error.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt")

;; Create an extended sandman that can sleep with a rktio poll set. An
;; external-event set might be naturally implemented with a poll set,
;; except that poll sets are single-use values. So, an external-event
;; set is instead implemented as a tree of callbacks to registers with
;; a (fresh) poll set each time.

;; This sandman builds on the default one to handles timeouts. While
;; it might make sense to all threads to sleep on pollable external
;; events, we don't implement that, and it's probably simpler to
;; connect events to semaphores through a long-term poll set...

(provide sandman-add-poll-set-adder)

(struct exts (timeout-at fd-adders))

(define (sandman-add-poll-set-adder old-exts adder)
  (exts (and old-exts (exts-timeout-at old-exts))
        (cons adder (and old-exts (exts-fd-adders old-exts)))))

(void
 (current-sandman
  (let ([timeout-sandman (current-sandman)])
    (sandman
     ;; sleep
     (lambda (exts)
       (define timeout-at (exts-timeout-at exts))
       (define fd-adders (exts-fd-adders exts))
       (define ps (rktio_make_poll_set rktio))
       (let loop ([fd-adders fd-adders])
         (cond
           [(not fd-adders) (void)]
           [(pair? fd-adders)
            (loop (car fd-adders))
            (loop (cdr fd-adders))]
           [else
            (fd-adders ps)]))
       (define sleep-secs (and timeout-at
                               (/ (- timeout-at (current-inexact-milliseconds)) 1000.0)))
       (unless (and sleep-secs (sleep-secs . <= . 0.0))
         (rktio_sleep rktio
                      (or sleep-secs 0.0)
                      ps
                      rktio_NULL))
       (rktio_poll_set_forget rktio ps))
     
     ;; poll
     (lambda (mode wakeup)
       ((sandman-do-poll timeout-sandman) mode wakeup))

     ;; any-sleepers?
     (lambda ()
       ((sandman-do-any-sleepers? timeout-sandman)))

     ;; sleepers-external-events
     (lambda ()
       ((sandman-do-sleepers-external-events timeout-sandman)))

     ;; add-thread!
     (lambda (t exts)
       (unless (null? (exts-fd-adders exts))
         (internal-error "cannot sleep on fds"))
       ((sandman-do-add-thread! timeout-sandman) t (exts-timeout-at exts)))
     
     ;; remove-thread!
     (lambda (t timeout-handle)
       ((sandman-do-remove-thread! timeout-sandman) t timeout-handle))

     ;; merge-exts
     (lambda (a-exts b-exts)
       (if (and a-exts b-exts)
           (exts ((sandman-do-merge-external-event-sets
                   timeout-sandman)
                  (exts-timeout-at a-exts)
                  (exts-timeout-at b-exts))
                 (if (and (exts-fd-adders a-exts)
                          (exts-fd-adders b-exts))
                     (cons (exts-fd-adders a-exts)
                           (exts-fd-adders b-exts))
                     (or (exts-fd-adders a-exts)
                         (exts-fd-adders b-exts))))
           (or a-exts b-exts)))
     
     ;; merge-timeout
     (lambda (old-exts timeout-at)
       (exts ((sandman-do-merge-timeout timeout-sandman)
              (and old-exts
                   (exts-timeout-at old-exts))
              timeout-at)
             (and old-exts
                  (exts-fd-adders old-exts))))
     
     ;; extract-timeout
     (lambda (exts)
       (exts-timeout-at exts))))))
