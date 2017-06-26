#lang racket/base
(require "bootstrap-thread-main.rkt")

;; Don't use exceptions here; see "../thread/demo.rkt"

(define done? #f)

(define-syntax-rule (test expect rhs)
  (let ([e expect]
        [v rhs])
    (unless (equal? e v)
      (error 'failed "~s: ~e" 'rhs v))))

(call-in-main-thread
 (lambda ()

   ;; Make `N` threads trying to write `P` copies
   ;; of each possible byte into a limited pipe, and
   ;; make `N` other threads try to read those bytes.
   (let ()
     (define N 8)
     (define M (/ 256 N))
     (define P 1)
     (define-values (in out) (make-pipe N))
     (test #f (byte-ready? in))
     (test out (sync/timeout #f out))
     (test N (write-bytes (make-bytes N 42) out))
     (test #t (byte-ready? in))
     (test #f (sync/timeout 0 out))
     (test 42 (read-byte in))
     (test #t (byte-ready? in))
     (test out (sync/timeout #f out))
     (write-byte 42 out)
     (test #f (sync/timeout 0 out))
     (test (make-bytes N 42) (read-bytes N in))
     (test #f (byte-ready? in))
     (test out (sync/timeout #f out))
     (define vec (make-vector 256))
     (define lock-vec (for/vector ([i 256]) (make-semaphore 1)))
     (define out-ths
       (for/list ([i N])
         (thread (lambda ()
                   (for ([k P])
                     (for ([j M])
                       (write-byte (+ j (* i M)) out)))))))
     (define in-ths
       (for/list ([i N])
         (thread (lambda ()
                   (for ([k P])
                     (for ([j M])
                       (define v (read-byte in))
                       (semaphore-wait (vector-ref lock-vec v))
                       (vector-set! vec v (add1 (vector-ref vec v)))
                       (semaphore-post (vector-ref lock-vec v))))))))
     (map sync out-ths)
     (map sync in-ths)
     (for ([count (in-vector vec)])
       (unless (= count P)
         (error "contended-pipe test failed"))))

   ;; Check progress events on pipes and byte strings
   (define (check-progress-on-port make-in)
     (define (check-progress dest-evt fail-dest-evt)
       (define in (make-in)) ; content = #"hello"
       (test #"he" (peek-bytes 2 0 in))
       (test #"hello" (peek-bytes 5 0 in))
       (test #"hel" (peek-bytes 3 0 in))
       (define progress1 (port-progress-evt in))
       ;(test #t (evt? progress1))
       (test #f (sync/timeout 0 progress1))
       (test #"hel" (peek-bytes 3 0 in))
       (test #f (sync/timeout 0 progress1))
       (test #f (port-commit-peeked 3 progress1 fail-dest-evt in))
       (test #"hel" (peek-bytes 3 0 in))
       (test #f (sync/timeout 0 progress1))
       (test #t (port-commit-peeked 3 progress1 dest-evt in))
       (test #"lo" (peek-bytes 2 0 in))
       (test progress1 (sync/timeout #f progress1))
       (test #f (port-commit-peeked 1 progress1 always-evt in)))
     (check-progress always-evt never-evt)
     (check-progress (make-semaphore 1) (make-semaphore 0))
     (check-progress (semaphore-peek-evt (make-semaphore 1)) (semaphore-peek-evt (make-semaphore 0)))
     (let ()
       (define ch1 (make-channel))
       (define ch2 (make-channel))
       (thread (lambda () (channel-put ch1 'ok)))
       (thread (lambda () (channel-get ch2)))
       (sync (system-idle-evt))
       (check-progress ch1 ch2)
       (check-progress (channel-put-evt ch2 'ok) (channel-put-evt ch1 'ok))))
   (check-progress-on-port
    (lambda ()
      (define-values (in out) (make-pipe))
      (write-bytes #"hello" out)
      in))
   (check-progress-on-port
    (lambda ()
      (open-input-bytes #"hello")))

   (printf "Enter to continue after confirming process sleeps...\n")
   (read-line)
   
   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))
