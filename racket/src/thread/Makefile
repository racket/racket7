# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++direct pthread ++pure ../../collects/racket/private/collect.rkt

# `$(GENERATED_RKTL)` appears as a dependency target in "$(BUILDDIR)expander.d"
GENERATED_RKTL = $(BUILDDIR)compiled/thread.rktl

thread-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) $(GENERATED_RKTL)

DIRECT_DEP = ../expander/compiled/bootstrap-run_rkt.zo
SAVE_DEPS = --depends $(GENERATED_RKTL) $(BUILDDIR)compiled/thread.d

$(GENERATED_RKTL): $(DIRECT_DEP)
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt --submod main -c $(BUILDDIR)compiled/cache-src -k ../.. $(IGNORE) -s -x $(SAVE_DEPS) -o $(BUILDDIR)compiled/thread.rktl

# Dependencies in "expander.d" are created via the "cs" makefile
-include $(BUILDDIR)expander.d
-include $(BUILDDIR)compiled/thread.d

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src demo
