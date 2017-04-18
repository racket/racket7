
RACKET = ../../racket/bin/racket
RACO = ../../racket/bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

thread-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt --submod main -c compiled/cache-src $(IGNORE) -s -x -o compiled/thread.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src demo
