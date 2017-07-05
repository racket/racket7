
RACKET = ../../racket/bin/racket
RACO = ../../racket/bin/raco

# Tree for collections:
TREE = ../../racket

# See "boot/read-primitive.rkt" for more info:
KNOT = ++knot read read/api.rkt \
       ++knot read read/primitive-parameter.rkt \
       ++knot read read/readtable-parameter.rkt \
       ++knot read read/readtable.rkt \
       ++knot read read/number.rkt

# The linklet compiler's simple inference cannot tell that this
# module's keyword-function declarations will have no side effect, but
# we promise that it's pure:
PURE = ++pure $(TREE)/collects/racket/private/collect.rkt

expander:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) $(PURE) -O $(TREE)

expander-src:
	$(RACO) make bootstrap-run.rkt
	$(MAKE) compiled/expander.rktl

compiled/expander.rktl: compiled/main_rkt.zo
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) $(PURE) -k $(TREE) -s -x -o compiled/expander.rktl

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
