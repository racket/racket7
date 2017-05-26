(library (expander)
  (export find-library-collection-links
          current-library-collection-links
          current-library-collection-paths
          find-library-collection-paths
          use-compiled-file-paths
          current-command-line-arguments
          executable-yield-handler
          flush
          call-in-main-thread
          set-exec-file!
          set-run-file!
          version
          exit)
  (import (chezpart)
          (rename (core)
                  ;; These names are not public primitives, so "expander.scm"
                  ;; can define them:
                  [correlated? core:correlated?]
                  [correlated-source core:correlated-source]
                  [correlated-line core:correlated-line]
                  [correlated-column core:correlated-column]
                  [correlated-position core:correlated-position]
                  [correlated-span core:correlated-span]
                  [correlated-e core:correlated-e]
                  [correlated->datum core:correlated->datum]
                  [datum->correlated core:datum->correlated]
                  [correlated-property core:correlated-property]
                  [correlated-property-symbol-keys core:correlated-property-symbol-keys])
          (thread)
          (regexp)
          (io)
          (linklet))

  ;; The expander needs various tables to set up primitive modules, and
  ;; the `primitive-table` function is the bridge between worlds

  (define (primitive-table key)
    (case key
      [(|#%linklet|) linklet-table]
      [(|#%kernel|) kernel-table]
      [(|#%read|) (make-hasheq)]
      [(|#%paramz|) paramz-table]
      [(|#%unsafe|) unsafe-table]
      [(|#%foreign|) foreign-table]
      [(|#%futures|) futures-table]
      [(|#%place|) place-table]
      [(|#%flfxnum|) flfxnum-table]
      [(|#%extfl|) extfl-table]
      [(|#%network|) network-table]
      [else #f]))
  
  (define-syntax make-primitive-table
    (syntax-rules ()
      [(_ prim ...)
       (let ([ht (make-hasheq)])
         (hash-primitive-set! ht prim)
         ...
         ht)]))

  (define-syntax hash-primitive-set!
    (syntax-rules ()
      [(_ ht [local prim]) (hash-set! ht 'prim local)]
      [(_ ht prim) (hash-set! ht 'prim prim)]))

  (include "expander-compat.scm")

  (include "primitive/kernel.scm")
  (include "primitive/unsafe.scm")
  (include "primitive/flfxnum.scm")
  (include "primitive/paramz.scm")
  (include "primitive/extfl.scm")
  (include "primitive/network.scm")
  (include "primitive/futures.scm")
  (include "primitive/place.scm")
  (include "primitive/foreign.scm")
  (include "primitive/linklet.scm")

  ;; ----------------------------------------

  (include "compiled/expander.scm")

  ;; ----------------------------------------

  ;; The environment is used to evaluate linklets, so all
  ;; primitives need to be imported (prefered) or defined
  ;; (less efficient to access) there
  (eval `(import (rename (core)
                         [correlated? syntax?]
                         [correlated-source syntax-source]
                         [correlated-line syntax-line]
                         [correlated-column syntax-column]
                         [correlated-position syntax-position]
                         [correlated-span syntax-span]
                         [correlated-e syntax-e]
                         [correlated->datum syntax->datum]
                         [datum->correlated datum->syntax]
                         [correlated-property syntax-property]
                         [correlated-property-symbol-keys syntax-property-symbol-keys])
                 (thread)
                 (io)
                 (regexp)
                 (linklet)))

  (eval `(define primitive-table ',primitive-table))
  
  (let ([install-table
         (lambda (table)
           (hash-for-each table
                          (lambda (k v)
                            (eval `(define ,k ',v)))))])
    (install-table compat-table))
  
  ;; ----------------------------------------

  ;; `install-reader!` is from the `io` library, where the
  ;; given functions are used by the default port read handler
  (install-reader! 1/read 1/read-syntax))
