
RACKET = ../../racket/bin/racket
RACO = ../../racket/bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

port:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -O ../../racket

port-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src  $(IGNORE) -s -x -o compiled/port.rktl

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: port port-src demo
