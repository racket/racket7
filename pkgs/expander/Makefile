
expander:
	../../racket/bin/raco make bootstrap-run.rkt
	../../racket/bin/racket bootstrap-run.rkt -c compiled/cache-src -O ../../racket

demo:
	../../racket/bin/raco make demo.rkt
	../../racket/bin/racket demo.rkt



.PHONY: run run-no-cache

run:
	../../racket/bin/raco make run.rkt
	../../racket/bin/racket $(RKT_ARGS) run.rkt -c compiled/cache $(ARGS)

run-src:
	../../racket/bin/raco make bootstrap-run.rkt
	../../racket/bin/racket $(RKT_ARGS) bootstrap-run.rkt -s -c compiled/cache-src $(ARGS)

run-no-cache:
	../../racket/bin/raco make run.rkt
	../../racket/bin/racket $(RKT_ARGS) run.rkt $(ARGS)
