#lang racket/base
(require racket/cmdline
	 racket/path
	 racket/file
	 compiler/find-exe
	 racket/system
	 "cs/prep.rkt")

(define scheme-dir "..\\build\\ChezScheme")
(define machine (if (= 32 (system-type 'word))
		    "ti3nt"
		    "ta6nt"))

(command-line
 #:once-each
 [("--scheme-dir") dir "Select the Chez Scheme build directory"
  (set! scheme-dir dir)]
 [("--machine") mach "Seelct the Chez Scheme machine name"
  (set! machine mach)]
 #:args
 ()
 (void))

(define (system*! prog . args)
  (printf "{in ~a}\n" (current-directory))
  (printf "~a" prog)
  (for ([arg (in-list args)])
    (printf " [~a]" arg))
  (newline)
  (unless (apply system*
		 (if (string? prog)
		     (find-executable-path (path-add-extension prog #".exe"))
		     prog)
		 args)
    (error 'csbuild "command failed")))

(define (system! cmd)
  (printf "{in ~a}\n" (current-directory))
  (printf "~a\n" cmd)
  (unless (system cmd)
    (error 'csbuild "command failed")))

;; ----------------------------------------

(unless (directory-exists? scheme-dir)
  (system*! "git"
	    "clone"
	    "git@github.com:mflatt/ChezScheme"
	    scheme-dir))

(unless (file-exists? (build-path scheme-dir "zlib" "Makefile"))
  (parameterize ([current-directory scheme-dir])
    (system*! "git" "submodule" "init")
    (system*! "git" "submodule" "update")))

(prep-chez-scheme scheme-dir machine)

(parameterize ([current-directory (build-path scheme-dir machine "c")])
  (system*! "nmake" (format "Makefile.~a" machine)))

;; ----------------------------------------

;; Run Racket in directories that reach here with "../worksp".
;; By using a relative path, we avoid problems with spaces in path names.
(define rel-racket (build-path 'up "worksp" (find-relative-path (current-directory) (find-exe))))

(define chain-racket
  (format "~a -W info@compiler/cm -l- setup --chain ../setup-go.rkt ../build/compiled"
	  rel-racket))

(define build-dir (path->directory-path (build-path 'up "build")))

;; ----------------------------------------

(define (build-layer name
		     #:dir [dir name]
		     #:and-expander? [and-expander? #t]
		     #:skip-make? [skip-make? #f])
  (parameterize ([current-directory (build-path 'up dir)])
    (make-directory* (build-path build-dir "compiled"))
    (define name.rktl (build-path build-dir "compiled" (format "~a.rktl" name)))
    (define name.d (build-path build-dir "compiled" (format "~a.d" name)))
    (define exp-name.d (if and-expander?
			   (build-path build-dir "compiled" (format "expander_~a.d" name))
			   name.d))
    (unless (file-exists? name.d) (call-with-output-file name.d void))
    (unless (file-exists? exp-name.d) (call-with-output-file exp-name.d void))
    (unless skip-make?
      (system*! "nmake"
		name.rktl
		(format "BUILDDIR=~a" build-dir)
		(format "RACKET=~a ~a ~a" chain-racket name.rktl exp-name.d)
		"DIRECT_DEP="))))

(build-layer "expander" #:and-expander? #f)
(build-layer "thread")
(build-layer "io")
(build-layer "regexp")

(build-layer "known" #:dir "schemify" #:skip-make? #t) ; to prep dependency files
(build-layer "schemify")
(build-layer "known" #:dir "schemify")

;; ----------------------------------------

(define scheme (build-path scheme-dir machine "bin" machine "scheme.exe"))
(define rel-scheme (build-path 'up "worksp"
			       (if (relative-path? scheme)
				   scheme
				   (find-relative-path (current-directory) scheme))))

;; Environment variable used by ".sls" files to find ".scm" files
(putenv "COMPILED_SCM_DIR" "../build/compiled/")

(parameterize ([current-directory (build-path 'up "cs")])
  (define convert.d (build-path build-dir "convert.d"))
  (system*! "nmake"
	    (build-path "../build/racket.so") ; need forward slashes
	    (format "RACKET=~a" rel-racket)
	    (format "SCHEME=~a" rel-scheme)
	    (format "BUILDDIR=../build/") ; need forward slashes
	    (format "CONVERT_RACKET=~a convert ~a" chain-racket convert.d)))
	    
;; ----------------------------------------

(system! "rktio.bat")

;; ----------------------------------------

;; The library name changes with the version:
(define scheme-lib
  (parameterize ([current-directory (build-path scheme-dir machine "boot" machine)])
    (for/or ([f (in-list (directory-list))]
	     #:when (regexp-match? #rx"^csv.*mt.lib$" f))
      f)))

(define rel2-scheme-dir (build-path 'up
				    (if (relative-path? scheme-dir)
					scheme-dir
					(find-relative-path (current-directory) scheme-dir))))

(parameterize ([current-directory "cs"])
  (system*! "nmake"
	    "..\\..\\build\\raw_racketcs.exe"
	    (format "SCHEME_DIR=~a" rel2-scheme-dir)
	    (format "MACHINE=~a" machine)
	    (format "SCHEME_LIB=~a" scheme-lib)))

;; ----------------------------------------

(system*! (find-exe)
	  "../cs/c/embed-boot.rkt"
	  "../build/raw_racketcs.exe"
	  "../../RacketCS.exe"
	  (build-path scheme-dir machine "boot" machine)
	  "../build/racket.so")
