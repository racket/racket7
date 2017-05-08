
RACKET = ../../bin/racket
RACO = ../../bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

io-src:
	$(RACO) make main.rkt
	$(RACO) make ../../../pkgs/expander/bootstrap-run.rkt
	$(MAKE) compiled/io.rktl

compiled/io.rktl: compiled/main_rkt.zo ../../../pkgs/expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../../../pkgs/expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src  $(IGNORE) -s -x -o compiled/io.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: io-src demo
