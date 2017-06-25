#lang racket/base
(require "check.rkt"
         "tree.rkt"
         "internal-error.rkt")

;; A "sandman" manages the set of all sleeping threads that may need
;; to be awoken in response to an external event, and it implements
;; the process-wide `sleep` that waits for an external event. Timeouts
;; are the only external events recognized by the initial sandman,
;; and that is supported by the host system's `sleep` function.

;; When a thread is registered with a sandman, the sandman provides a
;; handle representing the registration. The handle can be any value
;; except #f, and it is provided back to the sandman to unregister a
;; thread. A sandman doesn't unregister threads on its own, even when
;; it detects that an external event has happened.

;; When `sync` determines that a thread should sleep, it accumulates
;; external-event specifications to provide to the sandman along with
;; the thread. For the initial sandman, this information is just a
;; maximum wake-up time, but a more sophisticated sandman might
;; support file-descriptor activity. Event implementations expect a
;; sandman that provides specific functionality, so all sandman
;; implementations need to support time.

;; All sandman functions are called in atomic mode.

(provide sandman-merge-timeout
         sandman-merge-exts
         sandman-add-sleeping-thread!
         sandman-remove-sleeping-thread!
         sandman-poll
         sandman-sleep
         sandman-any-sleepers?

         set-the-sandman!)

;; A `sandman` implements several methods, and the sandman implementation
;; gets to pick the reprsentation of <ext-evnt-set> and <handle>, except
;; that #f is the "empty" external event set and #f cannot be a <handle>.
(struct sandman (do-sleep           ; <ext-event-set> -> (void), based on <ext-event-set> plus registered threads
                 do-poll            ; (thread -> any) -> (void), calls function any any thread to wake up

                 do-any-sleepers?   ; -> boolean

                 do-add-thread!     ; <thread> <ext-event-set> -> <handle>
                 do-remove-thread!  ; <thread> <handle> -> (void)

                 do-merge-external-event-sets ; <ext-event-set> <ext-event-set> -> <ext-event-set>

                 do-merge-timeout   ; <ext-event-set> <wake-up-date-as-msecs> -> <ext-event-set>
                 do-extract-timeout ; <ext-event-set> -> <wake-up-date-as-msecs>

                 #;...) ; sandman implementations can add more methods
                 
  #:prefab)

;; in atomic mode
(define (sandman-merge-timeout exts timeout)
  ((sandman-do-merge-timeout the-sandman) exts timeout))

;; in atomic mode
(define (sandman-merge-exts a-exts b-exts)
  ((sandman-do-merge-external-event-sets the-sandman) a-exts b-exts))

;; in atomic mode
(define (sandman-add-sleeping-thread! th exts)
  ((sandman-do-add-thread! the-sandman) th exts))

;; in atomic mode
(define (sandman-remove-sleeping-thread! th h)
  ((sandman-do-remove-thread! the-sandman) th h))

;; in atomic mode
(define (sandman-poll thread-wakeup)
  ((sandman-do-poll the-sandman) thread-wakeup))

;; in atomic mode
(define (sandman-sleep exts)
  ((sandman-do-sleep the-sandman) exts))

;; in atomic mode
(define (sandman-any-sleepers?)
  ((sandman-do-any-sleepers? the-sandman)))

(define/who (set-the-sandman! sm)
  (check who sandman? sm)
  (set! the-sandman sm))

;; ----------------------------------------

;; A tree mapping times (in milliseconds) to a hash table of threads
;; to wake up at that time
(define sleeping-threads empty-tree)

(define (min* a-sleep-until b-sleep-until)
  (if (and a-sleep-until b-sleep-until)
      (min a-sleep-until b-sleep-until)
      (or a-sleep-until b-sleep-until)))

(define the-sandman
  (sandman
   ;; sleep
   (lambda (non-sleeping-timeout-at)
     (define timeout-at
       (min*
        non-sleeping-timeout-at
        (if (tree-empty? sleeping-threads)
            (distant-future)
            (let-values ([(timeout-at threads) (tree-min sleeping-threads)])
              timeout-at))))
     (sleep (/ (- timeout-at (current-inexact-milliseconds)) 1000.0)))
  
   ;; poll
   (lambda (wakeup)
     (unless (tree-empty? sleeping-threads)
       (define-values (timeout-at threads) (tree-min sleeping-threads))
       (when (timeout-at . <= . (current-inexact-milliseconds))
         (unless (null? threads)
           (for ([t (in-hash-keys threads)])
             (wakeup t))))))

   ;; any-sleepers?
   (lambda ()
     (not (tree-empty? sleeping-threads)))

   ;; add-thread!
   (lambda (t sleep-until)
     (set! sleeping-threads
           (tree-set sleeping-threads
                     sleep-until
                     (hash-set (or (tree-ref sleeping-threads sleep-until <)
                                   #hasheq())
                               t
                               #t)
                     <))
     sleep-until)
   ;; remove-thread!
   (lambda (t sleep-until)
     (define threads (tree-ref sleeping-threads sleep-until <))
     (unless threads (internal-error "thread not found among sleeping threads"))
     (define new-threads (hash-remove threads t))
     (set! sleeping-threads
           (if (zero? (hash-count new-threads))
               (tree-remove sleeping-threads sleep-until <)
               (tree-set sleeping-threads sleep-until new-threads <))))

   ;; merge-exts
   (lambda (a-sleep-until b-sleep-until)
     (min* a-sleep-until b-sleep-until))
   
   ;; merge-timeout
   (lambda (sleep-until timeout-at)
     (if sleep-until
         (min sleep-until timeout-at)
         timeout-at))
   ;; extract-timeout
   (lambda (sleep-until) sleep-until)))


;; Compute an approximation to infinity:
(define (distant-future)
  (+ (current-inexact-milliseconds)
     (* 1000.0 60 60 24 365)))
