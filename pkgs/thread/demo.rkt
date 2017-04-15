#lang racket/base
(require "bootstrap-main.rkt")

(call-in-main-thread
 (lambda ()
   (define-syntax-rule (check a b)
     (unless (equal? a b)
       (error 'failed "~s: ~e vs. ~e" 'b a b)))
   
   (check #t (thread? (current-thread)))
   (define s (make-semaphore))
   (define t0 (thread (lambda () (semaphore-wait s) (printf "__\n") (semaphore-post s))))
   (define t1 (thread (lambda () (semaphore-wait s) (printf "hi\n") (semaphore-post s))))
   (define t2 (thread (lambda () (printf "HI\n") (semaphore-post s))))
   (thread-wait t0)
   (thread-wait t1)
   (thread-wait t2)
   
   (define ch (make-channel))
   (define ct1 (thread (lambda () (printf "1 ~a\n" (channel-get ch)))))
   (define ct2 (thread (lambda () (printf "2 ~a\n" (channel-get ch)))))
   (channel-put ch 'a)
   (channel-put ch 'b)
   
   (define cpt1 (thread (lambda () (channel-put ch 'c))))
   (define cpt2 (thread (lambda () (channel-put ch 'd))))
   (printf "3 ~a\n" (channel-get ch))
   (printf "4 ~a\n" (channel-get ch))
   
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 s))
   
   (define s2 (make-semaphore 3))
   (check s2 (sync/timeout 0 s s2))
   (check s2 (sync/timeout 0 s2 s))
   (check 'got-s2 (sync s (wrap-evt s2 (lambda (v) (check v s2) 'got-s2))))
   (check #f (sync/timeout 0 s2 s))
   
   (void (thread (lambda () (channel-put ch 'c2))))
   (check 'c2 (sync ch))
   
   (void (thread (lambda () (check 'c3 (channel-get ch)))))
   (define pc (channel-put-evt ch 'c3))
   (check pc (sync pc))
   
   (define ok-evt (guard-evt
                   (lambda ()
                     (define ch (make-channel))
                     (thread (lambda () (channel-put ch 'ok)))
                     ch)))
   (check 'ok (sync ok-evt))
   
   (semaphore-post s)
   (define sp (semaphore-peek-evt s))
   (check sp (sync/timeout 0 sp))
   (check sp (sync/timeout 0 sp))
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 sp))
   
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
   
   (void)))

   
