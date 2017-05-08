
RACKET = ../../bin/racket
RACO = ../../bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

thread-src:
	$(RACO) make main.rkt
	$(RACO) make ../../../pkgs/expander/bootstrap-run.rkt
	$(MAKE) compiled/thread.rktl

compiled/thread.rktl: compiled/main_rkt.zo ../../../pkgs/expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../../../pkgs/expander/bootstrap-run.rkt -t main.rkt --submod main -c compiled/cache-src $(IGNORE) -s -x -o compiled/thread.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src demo
