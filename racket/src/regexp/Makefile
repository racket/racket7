
RACKET = ../../bin/racket
RACO = ../../bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

io-src:
	$(RACO) make main.rkt
	$(RACO) make ../../../pkgs/expander/bootstrap-run.rkt
	$(MAKE) compiled/io.rktl

compiled/io.rktl:
	$(RACKET) ../../../pkgs/expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src  $(IGNORE) -s -x -o compiled/regexp.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

# Writes the extracted, compiled, decompiled expander to compiled/regexp.rkt
decompile:
	$(RACO) make ../../../pkgs/expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../../../pkgs/expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -s -x -D -o compiled/regexp.rkt

.PHONY: regexp-src demo decompile
