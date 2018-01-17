
RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Set to `skip` if the stand-alone rktio
# library is not needed
RKTIO_LIBRARY_TARGET = skip

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++pure ../../collects/racket/private/collect.rkt

io-src: compiled/rktio.rktl
	$(RACO) make main.rkt
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) compiled/io.rktl

compiled/io.rktl: compiled/main_rkt.zo ../expander/compiled/bootstrap-run_rkt.zo
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt --submod main -c compiled/cache-src -k ../.. $(IGNORE) -s -x -o compiled/io.rktl

demo: compiled/rktio.rktl
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

demo-thread: compiled/rktio.rktl
	$(RACO) make demo-thread.rkt
	$(RACKET) demo-thread.rkt


compiled/rktio.rktl: ../rktio/parse.rkt ../rktio/rktio.h
	$(MAKE) $(RKTIO_LIBRARY_TARGET)
	mkdir -p compiled
	$(RACKET) ../rktio/parse.rkt -o compiled/rktio.rktl ../rktio/rktio.h

skip:
	echo Not building stand-along rktio

rktio-library:
	$(MAKE) build-rktio RACKET="`$(RACKET) ../cs/absify.rkt --exec $(RACKET)`" PREFIX="`$(RACKET) ../cs/absify.rkt ../..`"

build-rktio:
	$(MAKE) ../build/so-rktio/Makefile
	cd ../build/so-rktio; make install-shared-object

../build/so-rktio/Makefile: ../rktio/configure ../rktio/Makefile.in ../rktio/rktio_config.h.in
	mkdir -p ../build/so-rktio
	cd ../build/so-rktio; ../../rktio/configure --enable-standalone --prefix=$(PREFIX)


.PHONY: io-src demo rktio build-rktio
