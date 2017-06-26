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
       (error "contended-pipe test failed")))

   (printf "Enter to continue after confirming process sleeps...\n")
   (read-line)
   
   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))
