# This makefile can be used directly, in which case it writes to a
# "compiled" subdirectory, or it can be driven by other makefiles that
# redirect to a different build dierctory by setting `BUILDDIR` and
# other variables.

# Beware that this makefile is used both for GNU make and for nmake!

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Tree for collections:
TREE = ../..

# See "boot/read-primitive.rkt" for more info:
KNOT = ++knot read read/api.rkt \
       ++knot read read/primitive-parameter.rkt \
       ++knot read read/readtable-parameter.rkt \
       ++knot read read/readtable.rkt \
       ++knot read read/number.rkt

# When flattening, replace a dynamic lookup from a primitive table to
# a direct use of the primitive name:
DIRECT = ++direct linklet ++direct kernel

# The linklet compiler's simple inference cannot tell that this
# module's keyword-function declarations will have no side effect, but
# we promise that it's pure:
PURE = ++pure $(TREE)/collects/racket/private/collect.rkt

expander:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) $(DIRECT) $(PURE) -O $(TREE)

# Besides here, `$(GENERATED_RKTL)` appears as a dependency target
# in "$(BUILDDIR)expander.d" when that file is genererated by
# the "cs/c" makefile driving this one
GENERATED_RKTL = $(BUILDDIR)compiled/expander.rktl

expander-src:
	$(RACO) make bootstrap-run.rkt
	$(MAKE) $(GENERATED_RKTL)

DIRECT_DEP = compiled/main_rkt.zo

$(GENERATED_RKTL): $(DIRECT_DEP)
	$(RACKET) bootstrap-run.rkt -c $(BUILDDIR)compiled/cache-src $(KNOT) $(DIRECT) $(PURE) -k $(TREE) -s -x -o $(BUILDDIR)compiled/expander.rktl

# The next two lines make both GNU make and nmake happy, and they
# cause nmake to not see the `-include`. The trick is that nmake
# doesn't recognize backslash as a comment continuation.
# \
!if 0

# Dependencies in "expander.d" are created via the "cs" makefile, which
# sets `RACKET` to be a bootstrapping load; the result doubles as
# dependencies both for bootstrapping "bootstrap-run.rkt" and for the
# generated flattened expander
-include $(BUILDDIR)expander.d

# \
!endif
# \
!include ..\build\expander.d

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

run:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt -c compiled/cache $(ARGS)

# Like `run`, but with source as compiled (as used for flattening)
run-src:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -s -c compiled/cache-src $(ARGS)

# Like `run`, but without using a cache for expanded and compiled linklets
run-no-cache:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt $(ARGS)

# Writes the extracted, compiled, decompiled expander to compiled/exp.rkt
decompile:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -s -x -D -o compiled/exp.rkt

.PHONY: expander demo run run-no-cache
