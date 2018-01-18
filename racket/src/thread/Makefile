
RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++direct pthread ++pure ../../collects/racket/private/collect.rkt

thread-src:
	$(RACO) make main.rkt
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) compiled/thread.rktl

compiled/thread.rktl: compiled/main_rkt.zo ../expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt --submod main -c compiled/cache-src -k ../.. $(IGNORE) -s -x -o compiled/thread.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src demo
