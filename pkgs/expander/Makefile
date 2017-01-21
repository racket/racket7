
RACKET = ../../racket/bin/racket
RACO = ../../racket/bin/raco

# See "boot/read-primitive.rkt" for more info:
KNOT = ++knot read read/api.rkt \
       ++knot read read/primitive-parameter.rkt \
       ++knot read read/readtable-parameter.rkt \
       ++knot read read/readtable.rkt

expander:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -O ../../racket

expander-src:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -s -x -o compiled/exp.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

run:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt -c compiled/cache $(ARGS)

# Like `run`, but with source as compiled (as used for flattening)
run-src:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -s -c compiled/cache-src $(ARGS)

# Like `run`, but without using a cache for expanded and compiled linklets
run-no-cache:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt $(ARGS)

# Writes the extracted, compiled, decompiled expander to compiled/exp.rkt
decompile:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -s -x -D -o compiled/exp.rkt

.PHONY: expander demo run run-no-cache
