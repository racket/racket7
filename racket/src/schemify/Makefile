# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion. Make annotation references direct to
# improve performance. Declaring "collect.rkt" pure works around a
# limitation of the flattener.
IGNORE = ++knot read - ++direct kernel ++pure ../../collects/racket/private/collect.rkt

# `$(GENERATED_RKTL)` appears as a dependency target in "$(BUILDDIR)expander.d"
S_GENERATED_RKTL = $(BUILDDIR)compiled/schemify.rktl
K_GENERATED_RKTL = $(BUILDDIR)compiled/known.rktl

# `MAIN_RKTL` can be selected by the "cs" makefile when driving this one
MAIN_RKTL = schemify.rktl
GENERATED_RKTL = $(BUILDDIR)compiled/$(MAIN)

schemify-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) $(S_GENERATED_RKTL)

known-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) $(K_GENERATED_RKTL)

DIRECT_DEP = ../expander/compiled/bootstrap-run_rkt.zo

S_SAVE_DEPS = --depends $(GENERATED_RKTL) $(BUILDDIR)compiled/schemify.d

$(S_GENERATED_RKTL): $(S_DIRECT_DEP)
	$(RACKET) ../expander/bootstrap-run.rkt -t main.rkt -c $(BUILDDIR)compiled/cache-src -k ../.. $(IGNORE) -s -x $(S_SAVE_DEPS) -o $(S_GENERATED_RKTL)

# \
!if 0

# Dependencies in "expander.d" are created via the "cs" makefile
-include $(BUILDDIR)expander.d
-include $(BUILDDIR)compiled/schemify.d

# \
!endif
# \
!include ..\build\expander_schemify.d
# \
!include ..\build\compiled\schemify.d

K_SAVE_DEPS = --depends $(GENERATED_RKTL) $(BUILDDIR)compiled/known.d

$(K_GENERATED_RKTL): $(DIRECT_DEP)
	$(RACKET) ../expander/bootstrap-run.rkt -t known.rkt -c $(BUILDDIR)compiled/cache-src -k ../.. $(IGNORE) -s -x $(K_SAVE_DEPS) -o $(K_GENERATED_RKTL)

# \
!if 0

# Dependencies in "expander.d" are created via the "cs" makefile
-include $(BUILDDIR)compiled/known.d

# \
!endif
# \
!include ..\build\expander_known.d
# \
!include ..\build\compiled\known.d

.PHONY: schemify-src known-src
