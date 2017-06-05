(import (core))

(define start-time (current-time 'time-duration))

(start-scheduler)

(define FUTURES-COUNT 10)

(define futures (map (lambda (x)
		       (future (lambda ()
				 (let loop ([i 200])
				   (cond
				    [(= i 0)
				     (printf "Done ~a\n" x)]
				    [else
				     (loop (- i 1))])))))
		     (iota FUTURES-COUNT)))

(printf "Launched all futures\n")

(for-each (lambda (f)
	    (touch f))
	  futures)

(printf "Touched all futures\n")
	    
(kill-scheduler)

(define end-time (current-time 'time-duration))
(let ([ss (* 1000000000 (time-second start-time))]
      [sn (time-nanosecond start-time)]
      [es (* 1000000000 (time-second end-time))]
      [en (time-nanosecond end-time)])
  (printf "Time elapsed: ~a seconds.\n" (inexact (/ (- (+ es en) (+ ss sn)) 1000000000))))

(exit)
