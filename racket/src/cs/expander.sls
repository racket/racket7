(library (expander)
  (export current-command-line-arguments
          executable-yield-handler
          load-on-demand-enabled
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

  ;; Set to `#t` to make compiled code compatible with changes to
  ;; primitive libraries
  (define compile-as-independent? #t)

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
  (include "primitive/internal.scm")

  ;; ----------------------------------------

  (include "compiled/expander.scm")

  ;; ----------------------------------------

  ;; The environment is used to evaluate linklets, so all primitives
  ;; need to be there imported (prefered) or defined (less efficient,
  ;; but less tied to library implementations)
  (unless compile-as-independent?
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
                   (linklet))))
                         
  (eval `(define primitive-table ',primitive-table))
  
  (let ([install-table
         (lambda (table)
           (hash-for-each table
                          (lambda (k v)
                            ;; Avoid redefining some primitives that we
                            ;; don't have to replace:
                            (unless (memq k '(vector
                                              list cons car cdr
                                              eq?
                                              values call-with-values))
                              (eval `(define ,k ',v))))))])
    (install-table compat-table)
    (when compile-as-independent?
      (install-table kernel-table)
      (install-table unsafe-table)
      (install-table flfxnum-table)
      (install-table paramz-table)
      (install-table extfl-table)
      (install-table network-table)
      (install-table futures-table)
      (install-table place-table)
      (install-table foreign-table)
      (install-table linklet-table)
      (install-table internal-table)))

  (when compile-as-independent?
    ;; Copied of macros provided by `core`
    (eval '(define-syntax with-continuation-mark
             (syntax-rules ()
               [(_ key val body)
                (call/cm key val (lambda () body))])))
    (eval '(define-syntax begin0
             (syntax-rules ()
               [(_ expr0 expr ...)
                (call-with-values (lambda ()
                                    (call-with-values (lambda () expr0)
                                      (case-lambda
                                       [(x) (values x #f)]
                                       [args (values args #t)])))
                  (lambda (l apply?)
                    expr ...
                    (if apply?
                        (#%apply values l)
                        l)))])))
    (eval '(define-syntax (|#%app| stx)
             (syntax-case stx ()
               [(_ rator rand ...)
                (with-syntax ([n-args (length #'(rand ...))])
                  #'((extract-procedure rator n-args) rand ...))])))
    (eval `(define raise-binding-result-arity-error ',raise-binding-result-arity-error)))

  ;; ----------------------------------------

  ;; `install-reader!` is from the `io` library, where the
  ;; given functions are used by the default port read handler
  (install-reader! 1/read 1/read-syntax)

  ;; `set-string->number?!` is also from the `io` library, where
  ;; the printer needs to check whether a string parses as a number
  ;; for deciding wheter to quote the string
  (set-string->number?! (lambda (str)
                          (not (not (1/string->number str 10 'read)))))

  ;; `set-maybe-raise-missing-module!` is also from the `io` library
  (set-maybe-raise-missing-module! maybe-raise-missing-module))
