
RACKET = ../../racket/bin/racket
RACO = ../../racket/bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

regexp:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -O ../../racket

regexp-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src  $(IGNORE) -s -x -o compiled/regexp.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

# Writes the extracted, compiled, decompiled expander to compiled/regexp.rkt
decompile:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -s -x -D -o compiled/regexp.rkt

.PHONY: regexp regexp-src demo decompile
