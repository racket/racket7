# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++pure ../../collects/racket/private/collect.rkt

# `$(GENERATED_RKTL)` appears as a dependency target in "$(BUILDDIR)expander.d"
GENERATED_RKTL = $(BUILDDIR)compiled/io.rktl

io-src: ../build/so-rktio/Makefile
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) $(GENERATED_RKTL)

DIRECT_DEP = ../expander/compiled/bootstrap-run_rkt.zo ../rktio/rktio.rktl
SAVE_DEPS = --depends $(GENERATED_RKTL) $(BUILDDIR)compiled/io.d

$(GENERATED_RKTL): $(DIRECT_DEP)
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt --submod main -c $(BUILDDIR)compiled/cache-src -k ../.. $(IGNORE) -s -x $(SAVE_DEPS) -o $(GENERATED_RKTL)

# Dependencies in "expander.d" are created via the "cs" makefile
-include $(BUILDDIR)expander.d
-include $(BUILDDIR)compiled/io.d

demo: compiled/rktio.rktl
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

demo-thread: compiled/rktio.rktl
	$(RACO) make demo-thread.rkt
	$(RACKET) demo-thread.rkt


../build/so-rktio/Makefile: ../rktio/configure ../rktio/Makefile.in ../rktio/rktio_config.h.in
	mkdir -p ../build/so-rktio
	$(MAKE) build-rktio RACKET="`$(RACKET) ../cs/absify.rkt --exec $(RACKET)`" PREFIX="`$(RACKET) ../cs/absify.rkt ../..`"

build-rktio:
	cd ../build/so-rktio; ../../rktio/configure --enable-standalone --prefix=$(PREFIX)
	cd ../build/so-rktio; make install-shared-object


.PHONY: io-src demo rktio build-rktio
