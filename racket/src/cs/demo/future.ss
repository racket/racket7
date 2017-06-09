(import (core))

(define f1 (future (lambda ()
		     (fprintf (current-error-port) "Starting fold-right\n")
		     (fold-right + 0 (iota 100)))))

(printf "Launched future 1\n")

(define f2 (future (lambda ()
		     (fprintf (current-error-port) "Starting map\n")
		     (map (lambda (x) (+ 1 x)) (iota 100)))))

(printf "Launched future 2\n")

(printf "Got result of f1: ~a\n" (touch f1))
(touch f2)
(printf "Got result of f2: won't print too long\n")

(exit)
