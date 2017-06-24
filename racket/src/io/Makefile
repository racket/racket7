
RACKET = ../../bin/racket
RACO = ../../bin/raco

# Ignoring functions from `#%read` works beause they won't
# appear in the simplified expansion:
IGNORE = ++knot read -

io-src: compiled/rktio.rktl
	$(RACO) make main.rkt
	$(RACO) make ../../../pkgs/expander/bootstrap-run.rkt
	$(MAKE) compiled/io.rktl

compiled/io.rktl: compiled/main_rkt.zo ../../../pkgs/expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../../../pkgs/expander/bootstrap-run.rkt -t main.rkt --submod main -c compiled/cache-src -k ../.. $(IGNORE) -s -x -o compiled/io.rktl

demo: compiled/rktio.rktl
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

compiled/rktio.rktl: ../rktio/parse.rkt ../rktio/rktio.h
	$(MAKE) build-rktio RACKET="`$(RACKET) ../cs/absify.rkt --exec $(RACKET)`" PREFIX="`$(RACKET) ../cs/absify.rkt ../..`"
	mkdir -p compiled
	$(RACKET) ../rktio/parse.rkt -o compiled/rktio.rktl ../rktio/rktio.h

build-rktio:
	mkdir -p ../build/so-rktio
	cd ../build/so-rktio; ../../rktio/configure --enable-standalone --prefix=$(PREFIX)
	cd ../build/so-rktio; make install-shared-object

.PHONY: io-src demo
