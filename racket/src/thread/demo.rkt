#lang racket/base
(require "bootstrap-main.rkt")

;; Don't use exception handlers here, because the "bootstrap.rkt"
;; implementation of engines can't support it.

(define done? #f)

(call-in-main-thread
 (lambda ()
   (define-syntax-rule (check a b)
     (unless (equal? a b)
       (error 'failed "~s: ~e vs. ~e" 'b a b)))

   ;; Check semaphores
   (check #t (thread? (current-thread)))
   (check #t (evt? (current-thread)))
   (define s (make-semaphore))
   (define t0 (thread (lambda () (semaphore-wait s) (printf "__\n") (semaphore-post s))))
   (define t1 (thread (lambda () (semaphore-wait s) (printf "hi\n") (semaphore-post s))))
   (define t2 (thread (lambda () (printf "HI\n") (semaphore-post s))))
   (thread-wait t0)
   (thread-wait t1)
   (thread-wait t2)
   
   ;; Check channels
   (define ch (make-channel))
   (define ct1 (thread (lambda () (printf "1 ~a\n" (channel-get ch)))))
   (define ct2 (thread (lambda () (printf "2 ~a\n" (channel-get ch)))))
   (channel-put ch 'a)
   (channel-put ch 'b)
   
   (define cpt1 (thread (lambda () (channel-put ch 'c))))
   (define cpt2 (thread (lambda () (channel-put ch 'd))))
   (printf "3 ~a\n" (channel-get ch))
   (printf "4 ~a\n" (channel-get ch))
   
   ;; Check semaphore polling
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 s))

   ;; Check more semaphore polling
   (define s2 (make-semaphore 3))
   (check s2 (sync/timeout 0 s s2))
   (check s2 (sync/timeout 0 s2 s))
   (check 'got-s2 (sync s (wrap-evt s2 (lambda (v) (check v s2) 'got-s2))))
   (check #f (sync/timeout 0 s2 s))

   ;; Check channel and `sync`
   (void (thread (lambda () (channel-put ch 'c2))))
   (check 'c2 (sync ch))
   
   ;; Check channel-put events
   (void (thread (lambda () (check 'c3 (channel-get ch)))))
   (define pc (channel-put-evt ch 'c3))
   (check pc (sync pc))

   ;; Check guard event
   (define ok-evt (guard-evt
                   (lambda ()
                     (define ch (make-channel))
                     (thread (lambda () (channel-put ch 'ok)))
                     ch)))
   (check 'ok (sync ok-evt))
   
   ;; Check semaphore-peek events
   (semaphore-post s)
   (define sp (semaphore-peek-evt s))
   (check sp (sync/timeout 0 sp))
   (check sp (sync/timeout 0 sp))
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 sp))

   ;; Check nacks
   (define nack #f)
   (check #t (semaphore? (sync (nack-guard-evt (lambda (n) (set! nack n) (make-semaphore 1))))))
   (check #f (sync/timeout 0 nack))
   (set! nack #f)
   (let loop ()
     (check 'ok (sync (nack-guard-evt (lambda (n) (set! nack n) (make-semaphore))) ok-evt))
     (unless nack (loop)))
   (check nack (sync/timeout 0 nack))
   
   (semaphore-post s)
   (check #f (sync/timeout 0 ch (channel-put-evt ch 'oops)))
   (check sp (sync/timeout #f ch (channel-put-evt ch 'oops) sp))

   ;; Check sleeping in main thread
   (define now1 (current-inexact-milliseconds))
   (sleep 0.1)
   (check #t ((current-inexact-milliseconds) . >= . (+ now1 0.1)))

   ;; Check sleeping in other thread
   (define now2 (current-inexact-milliseconds))
   (define ts (thread (lambda () (sleep 0.1))))
   (check ts (sync ts))
   (check #t ((current-inexact-milliseconds) . >= . (+ now2 0.1)))

   ;; Check `alarm-evt`
   (define now2+ (current-inexact-milliseconds))
   (define alm (alarm-evt (+ 0.1 now2+)))
   (check alm (sync alm))
   (check #t ((current-inexact-milliseconds) . >= . (+ now2+ 0.1)))

   ;; Check system-idle event
   (define v 0)
   (thread (lambda () (set! v (add1 v))))
   (sync (system-idle-evt))
   (check 1 v)
   
   (define (check-break/kill #:kill? kill?)
     (define stop-thread (if kill? kill-thread break-thread))
     (define (report-expected-exn what)
       (unless kill?
         (printf "[That ~a was from a thread, and it's expected]\n" what)))
     (define (report-expected-break)
       (report-expected-exn "break"))

     ;; Check that a loop can be abandoned
     (define tinf (thread (lambda () (let loop () (loop)))))
     (sleep)
     (stop-thread tinf)
     (check tinf (sync tinf))
     (report-expected-break)

     ;; Check that a break exception is delayed if disabled
     (define now3 (current-inexact-milliseconds))
     (define tdelay (with-continuation-mark
                        break-enabled-key
                      (make-thread-cell #f #t)
                      (thread (lambda ()
                                (sleep 0.1)
                                (with-continuation-mark
                                    break-enabled-key
                                  (make-thread-cell #t #t)
                                  (begin
                                        ;(check-for-break)
                                    (let loop () (loop))))))))
     (stop-thread tdelay)
     (check tdelay (sync tdelay))
     (report-expected-break)
     (unless kill?
       (check #t ((current-inexact-milliseconds) . >= . (+ now3 0.1))))
     
     ;; Check that a semaphore wait can be abandoned
     (define tstuck (thread (lambda () (semaphore-wait (make-semaphore)))))
     (sync (system-idle-evt))
     (stop-thread tstuck)
     (check tstuck (sync tstuck))
     (report-expected-break)

     ;; Check that an externally abanoned `sync` posts nacks
     (define nack1 #f)
     (define nack2 #f)
     (define tstuck2 (thread (lambda ()
                               (sync (nack-guard-evt
                                      (lambda (s) (set! nack1 s) never-evt))
                                     (nack-guard-evt
                                      (lambda (s) (set! nack2 s) never-evt))))))
     (sync (system-idle-evt))
     (stop-thread tstuck2)
     (thread-wait tstuck2)
     (report-expected-break)
     (check nack1 (sync nack1))
     (check nack2 (sync nack2))
     
     ;; Make sure a `sync` can be abandoned during a guard callback
     (define tfail (thread (lambda ()
                             (sync (nack-guard-evt
                                    (lambda (s)
                                      (set! nack1 s)
                                      (if kill?
                                          (kill-thread (current-thread))
                                          (error "oops"))))))))
     (check tfail (sync tfail))
     (report-expected-exn "oops")
     (check nack1 (sync nack1))
     
     ;; Make sure nested abandoned `syncs` are ok
     (define tfail2 (thread (lambda ()
                              (sync (nack-guard-evt
                                     (lambda (s)
                                       (set! nack1 s)
                                       (sync (nack-guard-evt
                                              (lambda (s)
                                                (set! nack2 s)
                                                (if kill?
                                                    (kill-thread (current-thread))
                                                    (error "oops")))))))))))
     (check tfail2 (sync tfail2))
     (report-expected-exn "oops")
     (check nack1 (sync nack1))
     (check nack2 (sync nack2)))
   
   (check-break/kill #:kill? #f)
   (check-break/kill #:kill? #t)

   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))
