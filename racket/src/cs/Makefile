RACKET = ../../bin/racket
SCHEME = scheme

# Controls whether Racket layers are built as unsafe:
UNSAFE_COMP = --unsafe

# Controls whether Racket layers are built with expression-level debugging:
DEBUG_COMP = # --debug

# Controls whether Rumble is built as unsafe:
RUMBLE_UNSAFE_COMP = --unsafe

COMPILE_FILE = $(SCHEME) --script compile-file.ss $(UNSAFE_COMP) $(DEBUG_COMP)
COMPILE_FILE_DEPS = compile-file.ss

SCHEMIFY_SRC_DEPS = schemify/schemify.rkt schemify/known.rkt schemify/match.rkt \
                    schemify/find-definition.rkt schemify/left-to-right.rkt schemify/mutated.rkt \
                    schemify/struct-type-info.rkt schemify/import.rkt schemify/mutated-state.rkt \
                    schemify/simple.rkt schemify/serialize.rkt schemify/let.rkt schemify/equal.rkt \
                    schemify/lift.rkt schemify/wrap-annotation.rkt schemify/match-annotation.rkt \
                    schemify/jitify.rkt schemify/interpret.rkt schemify/size.rkt schemify/optimize.rkt \
                    schemify/literal.rkt schemify/main.rkt
CONVERT_DEPS = convert.rkt $(SCHEMIFY_SRC_DEPS) known-primitive.rkt

CONVERT = $(RACKET) -l- raco make convert.rkt && $(RACKET) convert.rkt

THREAD_DEPS = chezpart.so rumble.so
IO_DEPS = $(THREAD_DEPS) thread.so
REGEXP_DEPS = $(IO_DEPS) io.so
SCHEMIFY_DEPS = $(REGEXP_DEPS) regexp.so known-primitive.so
LINKLET_DEPS = $(SCHEMIFY_DEPS) schemify.so
EXPANDER_DEPS = $(LINKLET_DEPS) linklet.so
MAIN_DEPS = $(EXPANDER_DEPS) expander.so

all:
	$(MAKE) rktio
	$(MAKE) rktl
	$(MAKE) expander.so

expander-demo: expander.so demo/expander.ss
	$(SCHEME) $(EXPANDER_DEPS) expander.so demo/expander.ss

run: main.so ../../bin/racket
	$(SCHEME) --script main.so ../../bin/racket $(ARGS)

setup:
	$(MAKE) run ARGS="-l- setup $(ARGS)"

setup-v:
	$(MAKE) run ARGS="-W 'info@compiler/cm info@linklet debug@GC:major error' -l- setup $(ARGS)"

run-wpo: racket.so ../../bin/racket
	$(SCHEME) --script racket.so ../../bin/racket $(ARGS)

racket.so: main.so $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) --whole-program racket.so main.wpo

main.so: $(MAIN_DEPS) main.sps $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) main.sps $(MAIN_DEPS)

strip:
	${SCHEME} --script strip.ss $(MAIN_DEPS) racket.so

rktl:
	$(MAKE) thread-rktl
	$(MAKE) io-rktl
	$(MAKE) regexp-rktl
	$(MAKE) expander-rktl

# For running without an enclosing build of the traditional Racket VM:
../../bin/racket:
	mkdir -p ../../bin
	touch ../../bin/racket

PRIMITIVES_TABLES = primitive/kernel.ss primitive/unsafe.ss primitive/flfxnum.ss \
                    primitive/paramz.ss primitive/extfl.ss primitive/network.ss \
                    primitive/futures.ss primitive/foreign.ss primitive/place.ss \
                    primitive/linklet.ss primitive/internal.ss

expander.so: expander.sls compiled/expander.scm $(PRIMITIVES_TABLES) $(EXPANDER_DEPS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) expander.sls $(EXPANDER_DEPS)

compiled/expander.scm: ../expander/compiled/expander.rktl $(CONVERT_DEPS)
	$(CONVERT) ../expander/compiled/expander.rktl compiled/expander.scm

..expander/compiled/expander.rktl:
	$(MAKE) expander-rktl

expander-rktl:
	$(MAKE) bounce BOUNCE_DIR=../expander BOUNCE_TARGET=expander-src

linklet-demo: linklet.so
	$(SCHEME) $(LINKLET_DEPS) linklet.so demo/linklet.ss

linklet.so: linklet.sls $(LINKLET_DEPS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) linklet.sls $(LINKLET_DEPS)


schemify.so: schemify.sls compiled/schemify.scm $(SCHEMIFY_DEPS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) schemify.sls $(SCHEMIFY_DEPS)

compiled/schemify.scm: compiled/schemify.rktl $(CONVERT_DEPS)
	$(CONVERT) --skip-export compiled/schemify.rktl compiled/schemify.scm

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion. Make annotation references direct to
# improve performance. Declaring "collect.rkt" pure works around a
# limitation of the flattener.
IGNORE = ++knot read - ++direct-prefixed annotation ++pure ../../collects/racket/private/collect.rkt

compiled/schemify.rktl: $(SCHEMIFY_SRC_DEPS)
	$(RACKET) -N raco -l- raco make ../expander/bootstrap-run.rkt
	$(RACKET) ../expander/bootstrap-run.rkt -t schemify/main.rkt -c compiled/cache-src -k ../.. $(IGNORE) -s -x -o compiled/schemify.rktl


known-primitive.so: known-primitive.sls $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) known-primitive.sls

known-primitive.sls: known-primitive.rkt
	$(RACKET) known-primitive.rkt


regexp-demo: regexp.so
	$(SCHEME) $(REGEXP_DEPS) regexp.so demo/regexp.ss

regexp.so: compiled/regexp.scm regexp.sls $(REGEXP_DEPS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) regexp.sls $(REGEXP_DEPS)

compiled/regexp.scm: ../regexp/compiled/regexp.rktl $(CONVERT_DEPS)
	$(CONVERT) ../regexp/compiled/regexp.rktl compiled/regexp.scm

../regexp/compiled/regexp.rktl:
	$(MAKE) regexp-rktl

regexp-rktl:
	$(MAKE) bounce BOUNCE_DIR=../regexp BOUNCE_TARGET=regexp-src


io-demo: io.so
	$(SCHEME) $(IO_DEPS) io.so demo/io.ss

io.so: compiled/io.scm io.sls $(IO_DEPS) ../io/compiled/rktio.rktl $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) io.sls $(IO_DEPS)

compiled/io.scm: ../io/compiled/io.rktl $(CONVERT_DEPS)
	$(CONVERT) ../io/compiled/io.rktl compiled/io.scm

../io/compiled/io.rktl:
	$(MAKE) io-rktl

../io/compiled/rktio.rktl:
	$(MAKE) io-rktl

io-rktl:
	$(MAKE) bounce BOUNCE_DIR=../io BOUNCE_TARGET=io-src

rktio:
	$(MAKE) bounce BOUNCE_DIR=../io BOUNCE_TARGET=rktio


thread-demo: thread.so
	$(SCHEME) $(THREAD_DEPS) thread.so demo/thread.ss

thread.so: compiled/thread.scm thread.sls $(THREAD_DEPS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) thread.sls $(THREAD_DEPS)

compiled/thread.scm: ../thread/compiled/thread.rktl $(CONVERT_DEPS)
	$(CONVERT) ../thread/compiled/thread.rktl compiled/thread.scm

../thread/compiled/thread.rktl:
	$(MAKE) thread-rktl

thread-rktl:
	$(MAKE) bounce BOUNCE_DIR=../thread BOUNCE_TARGET=thread-src


bounce:
	$(MAKE) bounce-go RACKET="`$(RACKET) absify.rkt --exec $(RACKET)`"

bounce-go:
	cd $(BOUNCE_DIR); $(MAKE) RACO="$(RACKET) -N raco -l- raco" $(BOUNCE_TARGET)


chaperone-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/chaperone.ss

hash-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/hash.ss

struct-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/struct.ss

control-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/control.ss

foreign-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/foreign.ss

will-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/will.ss

future-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/future.ss

future2-demo: rumble.so
	$(SCHEME) chezpart.so rumble.so demo/future2.ss

RUMBLE_SRCS = rumble/define.ss \
              rumble/check.ss \
	      rumble/syntax-rule.ss \
              rumble/constant.ss \
              rumble/hash-code.ss \
              rumble/struct.ss \
              rumble/prefab.ss \
              rumble/procedure.ss \
              rumble/impersonator.ss \
              rumble/equal.ss \
              rumble/object-name.ss \
              rumble/arity.ss \
              rumble/intmap.ss \
              rumble/hash.ss \
              rumble/datum.ss \
	      rumble/lock.ss \
              rumble/thread-cell.ss \
              rumble/parameter.ss \
              rumble/begin0.ss \
              rumble/pthread.ss \
              rumble/control.ss \
              rumble/interrupt.ss \
              rumble/engine.ss \
              rumble/error.ss \
              rumble/srcloc.ss \
              rumble/boolean.ss \
              rumble/bytes.ss \
              rumble/string.ss \
              rumble/char.ss \
              rumble/symbol.ss \
              rumble/list.ss \
              rumble/vector.ss \
              rumble/box.ss \
              rumble/immutable.ss \
              rumble/keyword.ss \
              rumble/mpair.ss \
              rumble/number.ss \
              rumble/random.ss \
              rumble/flvector.ss \
              rumble/correlated.ss \
              rumble/graph.ss \
              rumble/time.ss \
              rumble/memory.ss \
              rumble/ephemeron.ss \
              rumble/will-executor.ss \
              rumble/system.ss \
              rumble/unsafe.ss \
              rumble/extfl.ss \
              rumble/place.ss \
              rumble/foreign.ss \
              rumble/future.ss \
              rumble/version.ss \
              ../racket/src/schvers.h

rumble.so: chezpart.so rumble.sls $(RUMBLE_SRCS) $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) $(RUMBLE_UNSAFE_COMP) rumble.sls

chezpart.so: chezpart.sls $(COMPILE_FILE_DEPS)
	$(COMPILE_FILE) chezpart.sls

clean:
	rm -f chezpart.so rumble.so regexp.so io.so linklet.so known-primitive.so expander.so schemify.so
	rm -f chezpart.wpo rumble.wpo regexp.wpo io.wpo linklet.wpo known-primitive.wpo expander.wpo schemify.wpo
	rm -f thread.so thread.wpo main.wpo main.so
	rm -rf compiled
