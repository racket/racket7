(import (core))

(define (complete ticks args)
  (printf "Done!\n"))

(define (expired q)
  (lambda (new-eng) 
    (queue-add! q new-eng)))

(define q1 (make-queue))
(define q2 (make-queue))

(define (setup-work n q)
  (for-each (lambda (x)
	      (queue-add! q (make-engine
			     (lambda ()
			       (printf "I am engine ~a:~a\n" n x)
			       (let loop ([i 1000])
				 (cond
				  [(= i 0)
				   (printf "Engine ~a:~a is done.\n" n x)]
				  [else
				   (loop (- i 1))]))) #f #t)))
	    (iota 10)))

(define (round-robin n q)
  (lambda ()
    (let loop ()
      (cond
       [(queue-empty? q)
	(printf "Thread ~a is done\n" n)]
       [else
	(let ([eng (queue-remove! q)])
	  (eng 100 void complete (expired q))
	  (loop))]))))

(setup-work 1 q1)
(setup-work 2 q2)

(fork-thread (round-robin 1 q1))
(fork-thread (round-robin 2 q2))



