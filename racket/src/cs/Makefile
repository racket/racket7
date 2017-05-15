RACKET = ../../bin/racket
SCHEME = scheme
#SCHEME = /Users/mflatt/repo/ChezScheme-if/a6osx/bin/a6osx/scheme -b /Users/mflatt/repo/ChezScheme-if/a6osx/boot/a6osx/petite.boot -b /Users/mflatt/repo/ChezScheme-if/a6osx/boot/a6osx/scheme.boot
UNSAFE = # '(optimize-level 3)'

COMP = echo '(reset-handler abort) (keyboard-interrupt-handler abort) (generate-wpo-files \#t)'

SCHEMIFY_SRC_DEPS = schemify/schemify.rkt schemify/known.rkt schemify/match.rkt \
                    schemify/find-definition.rkt schemify/left-to-right.rkt schemify/mutated.rkt \
                    schemify/struct-type-info.rkt schemify/import.rkt schemify/mutated-state.rkt \
                    schemify/simple.rkt schemify/serialize.rkt schemify/let.rkt
CONVERT_DEPS = convert.rkt $(SCHEMIFY_SRC_DEPS)

THREAD_DEPS = chezpart.so core.so
IO_DEPS = $(THREAD_DEPS) thread.so
REGEXP_DEPS = $(IO_DEPS) io.so
SCHEMIFY_DEPS = $(REGEXP_DEPS) regexp.so known-primitive.so
LINKLET_DEPS = $(SCHEMIFY_DEPS) schemify.so
EXPANDER_DEPS = $(LINKLET_DEPS) linklet.so
MAIN_DEPS = $(EXPANDER_DEPS) expander.so

expander-demo: expander.so demo/expander.ss
	env $(SCHEME) $(EXPANDER_DEPS) expander.so demo/expander.ss

run: main.so
	${SCHEME} --script main.so ../../bin/racket $(ARGS)

racket.so: main.so
	echo '(compile-whole-program "main.wpo" "racket.so" #t)' | $(SCHEME) -q

main.so: $(MAIN_DEPS) main.sps
	$(COMP) '(compile-file "main.sps")' | $(SCHEME) -q $(MAIN_DEPS)

rktl:
	$(MAKE) thread-rktl
	$(MAKE) io-rktl
	$(MAKE) regexp-rktl
	$(MAKE) expander-rktl

PRIMITIVES_TABLES = primitive/kernel.scm primitive/unsafe.scm primitive/flfxnum.scm

expander.so: expander.sls compiled/expander.scm expander-compat.scm $(PRIMITIVES_TABLES) $(EXPANDER_DEPS)
	$(COMP) '(compile-file "expander.sls")' | $(SCHEME) -q $(EXPANDER_DEPS)

compiled/expander.scm: ../../../pkgs/expander/compiled/expander.rktl $(CONVERT_DEPS)
	$(RACKET) convert.rkt ../../../pkgs/expander/compiled/expander.rktl compiled/expander.scm

../../../pkgs/expander/compiled/expander.rktl:
	$(MAKE) expander-rktl

expander-rktl:
	$(MAKE) bounce BOUNCE_DIR=../../../pkgs/expander BOUNCE_TARGET=expander-src

linklet-demo: linklet.so
	$(SCHEME) $(LINKLET_DEPS) linklet.so demo/linklet.ss

linklet.so: linklet.sls $(LINKLET_DEPS)
	$(COMP) '(compile-file "linklet.sls")' | $(SCHEME) -q $(LINKLET_DEPS)


schemify.so: schemify.sls compiled/schemify.scm $(SCHEMIFY_DEPS)
	$(COMP) '(compile-file "schemify.sls")' | $(SCHEME) -q $(SCHEMIFY_DEPS)

compiled/schemify.scm: compiled/schemify.rktl $(CONVERT_DEPS)
	$(RACKET) convert.rkt --skip-export compiled/schemify.rktl compiled/schemify.scm

compiled/schemify.rktl: $(SCHEMIFY_SRC_DEPS)
	$(RACKET) -N raco -l- raco make ../../../pkgs/expander/bootstrap-run.rkt
	$(RACKET) ../../../pkgs/expander/bootstrap-run.rkt -t schemify/schemify.rkt -c compiled/cache-src -k ../.. ++knot read - -s -x -o compiled/schemify.rktl


known-primitive.so: known-primitive.sls
	$(COMP) '(compile-file "known-primitive.sls")' | $(SCHEME) -q

known-primitive.sls: known-primitive.rkt
	$(RACKET) known-primitive.rkt


regexp-demo: regexp.so
	$(SCHEME) $(REGEXP_DEPS) regexp.so demo/regexp.ss

regexp.so: compiled/regexp.scm regexp.sls $(REGEXP_DEPS)
	$(COMP) '(compile-file "regexp.sls")' | $(SCHEME) -q $(REGEXP_DEPS)

compiled/regexp.scm: ../regexp/compiled/regexp.rktl $(CONVERT_DEPS)
	$(RACKET) convert.rkt ../regexp/compiled/regexp.rktl compiled/regexp.scm

../regexp/compiled/regexp.rktl:
	$(MAKE) regexp-rktl

regexp-rktl:
	$(MAKE) bounce BOUNCE_DIR=../regexp BOUNCE_TARGET=regexp-src


io-demo: io.so
	$(SCHEME) $(IO_DEPS) io.so demo/io.ss

io.so: compiled/io.scm io.sls $(IO_DEPS)
	$(COMP) '(compile-file "io.sls")' | $(SCHEME) -q $(IO_DEPS)

compiled/io.scm: ../io/compiled/io.rktl $(CONVERT_DEPS)
	$(RACKET) convert.rkt ../io/compiled/io.rktl compiled/io.scm

../io/compiled/io.rktl:
	$(MAKE) io-rktl

io-rktl:
	$(MAKE) bounce BOUNCE_DIR=../io BOUNCE_TARGET=io-src


thread-demo: thread.so
	$(SCHEME) $(THREAD_DEPS) thread.so demo/thread.ss

thread.so: compiled/thread.scm thread.sls $(THREAD_DEPS)
	$(COMP) '(compile-file "thread.sls")' | $(SCHEME) -q $(THREAD_DEPS)

compiled/thread.scm: ../thread/compiled/thread.rktl $(CONVERT_DEPS)
	$(RACKET) convert.rkt ../thread/compiled/thread.rktl compiled/thread.scm

../thread/compiled/thread.rktl:
	$(MAKE) thread-rktl

thread-rktl:
	$(MAKE) bounce BOUNCE_DIR=../thread BOUNCE_TARGET=thread-src


bounce:
	$(MAKE) bounce-go RACKET="`$(RACKET) absify.rkt --exec $(RACKET)`"

bounce-go:
	cd $(BOUNCE_DIR); $(MAKE) RACO="$(RACKET) -N raco -l- raco" $(BOUNCE_TARGET)


chaperone-demo: core.so
	$(SCHEME) chezpart.so core.so demo/chaperone.ss

hash-demo: core.so
	$(SCHEME) chezpart.so core.so demo/hash.ss

struct-demo: core.so
	$(SCHEME) chezpart.so core.so demo/struct.ss

control-demo: core.so
	$(SCHEME) chezpart.so core.so demo/control.ss

CORE_SRCS = core/constant.ss \
            core/hash-code.ss \
            core/struct.ss \
            core/procedure.ss \
            core/chaperone.ss \
            core/equal.ss \
            core/object-name.ss \
            core/hamt.ss \
            core/hash.ss \
	    core/lock.ss \
            core/thread-cell.ss \
            core/parameter.ss \
            core/begin0.ss \
            core/control.ss \
            core/interrupt.ss \
            core/engine.ss \
            core/error.ss \
            core/bytes.ss \
            core/string.ss \
            core/symbol.ss \
            core/list.ss \
            core/vector.ss \
            core/box.ss \
            core/immutable.ss \
            core/keyword.ss \
            core/mpair.ss \
            core/number.ss \
            core/flvector.ss \
            core/correlated.ss \
            core/time.ss \
            core/memory.ss \
            core/system.ss \
            core/unsafe.ss

core.so: chezpart.so core.sls $(CORE_SRCS)
	$(COMP) $(UNSAFE) '(compile-file "core.sls")' | $(SCHEME) -q

chezpart.so: chezpart.sls
	$(COMP) '(compile-file "chezpart.sls")' | $(SCHEME) -q

clean:
	rm -f chezpart.so core.so regexp.so io.so immutable-hash.so linklet.so known-primitive.so linklet.so expander.so schemify.so
	rm -rf compiled
