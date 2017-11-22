
RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++pure ../../collects/racket/private/collect.rkt

regexp-src:
	$(RACO) make main.rkt
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) compiled/regexp.rktl

compiled/regexp.rktl: compiled/main_rkt.zo ../expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src -k ../.. $(IGNORE) -s -x -o compiled/regexp.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

# Writes the extracted, compiled, decompiled expander to compiled/regexp.rkt
decompile:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -s -x -D -o compiled/regexp.rkt

.PHONY: regexp-src demo decompile
