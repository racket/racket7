(library (linklet)
  (export linklet?
          compile-linklet
          recompile-linklet
          eval-linklet
          read-compiled-linklet
          instantiate-linklet
              
          linklet-import-variables
          linklet-export-variables
          
          instance?
          make-instance
          instance-name
          instance-data
          instance-variable-names
          instance-variable-value
          instance-set-variable-value!
          instance-unset-variable!

          linklet-directory?
          hash->linklet-directory
          linklet-directory->hash

          linklet-bundle?
          hash->linklet-bundle
          linklet-bundle->hash
          
          variable-reference?
          variable-reference->instance
          variable-reference-constant?)
  (import (chezpart)
          (only (chezscheme) printf)
          (core)
          (only (io)
                path?
                path->string)
          (regexp)
          (schemify))

  (define (primitive->compiled-position prim) #f)
  (define (compiled-position->primitive pos) #f)

  (define show-on? (getenv "PLT_LINKLET_SHOW"))
  (define (show what v)
    (when show-on?
      (printf ";; ~a ---------------------\n" what)
      (pretty-print v))
    v)

  (define (compile-to-bytevector s)
    (let-values ([(o get) (open-bytevector-output-port)])
      (compile-to-port (list s) o)
      (get)))

  (define (eval-from-bytevector bv)
    ;; HACK: This probably always works for the `lambda` or
    ;; `let`+`lambda` forms that we compile as linklets, but we need a
    ;; better function from the host Scheme:
    ((cdr (vector-ref (fasl-read (open-bytevector-input-port bv)) 1))))

  ;; A linklet is implemented as a procedure that takes an argument
  ;; for each import plus an `variable` for each export, and calling
  ;; the procedure runs the linklet body.

  ;; A source linklet has a list of list of imports; those are all
  ;; flattened into a sequence of arguments for the linklet procedure,
  ;; followed by the arguments to receive the export `variable`s. Each
  ;; import is either a `variable` or the variable's value as
  ;; indicated by the "ABI" (which is based on information about which
  ;; exports of an imported linklet are constants).

  ;; A linklet also has a table of information about its 

  (define-record linklet (code           ; the procedure
                          compiled?      ; whether the procedure is in source or value form
                          importss-abi   ; ABI for each import, in parallel to `importss`
                          exports-info   ; hash(sym -> known) for info about each export; see "known.rkt"
                          name           ; name of the linklet (for debugging purposes)
                          importss       ; list of list of import symbols
                          exports))      ; list of export symbols

  (define compile-linklet
    (case-lambda
     [(c) (compile-linklet c #f #f (lambda (key) (values #f #f)) #t)]
     [(c name) (compile-linklet c name #f (lambda (key) (values #f #f)) #t)]
     [(c name import-keys) (compile-linklet c name import-keys (lambda (key) (values #f #f)) #t)]
     [(c name import-keys get-import) (compile-linklet c name import-keys get-import #t)]
     [(c name import-keys get-import serializable?)
      ;; Convert the linklet S-expression to a `lambda` S-expression:
      (define-values (impl-lam importss-abi exports-info)
        (schemify-linklet (show "linklet" c)
                          serializable?
                          convert-to-annotation
                          unannotate
                          prim-knowns
                          ;; Callback to get a specific linklet for a
                          ;; given import:
                          (lambda (index)
                            (lookup-linklet get-import import-keys index))))
      ;; Create the linklet:
      (let ([lk (make-linklet ((if serializable? compile-to-bytevector compile)
                               (show "schemified" (remove-annotation-boundary impl-lam)))
                              (not serializable?)
                              importss-abi
                              exports-info
                              name
                              (map (lambda (ps)
                                     (map (lambda (p) (if (pair? p) (car p) p))
                                          ps))
                                   (cadr c))
                              (map (lambda (p) (if (pair? p) (cadr p) p))
                                   (caddr c)))])
        ;; In general, `compile-linklet` is allowed to extend the set
        ;; of linklet imports if `import-keys` is provided (e.g., for
        ;; cross-linklet optimization where inlining needs a new
        ;; direct import) - but we don't do that, currently
        (if import-keys
            (values lk import-keys)
            lk))]))

  (define (lookup-linklet get-import import-keys index)
    ;; Use the provided callback to get an linklet for the
    ;; import at `index`
    (and get-import
         import-keys
         (let ([key (vector-ref import-keys index)])
           (and key
                (let-values ([(lnk/inst more-import-keys) (get-import key)])
                  (and (linklet? lnk/inst)
                       (linklet-exports-info lnk/inst)))))))

  (define (recompile-linklet lnk . args) lnk)

  ;; Intended to speed up reuse of a linklet in exchange for not being
  ;; able to serialize anymore
  (define (eval-linklet linklet)
    (if (linklet-compiled? linklet)
        linklet
        (make-linklet (eval-from-bytevector (linklet-code linklet))
                      #t
                      (linklet-importss-abi linklet)
                      (linklet-exports-info linklet)
                      (linklet-name linklet)
                      (linklet-importss linklet)
                      (linklet-exports linklet))))

  (define (read-compiled-linklet in)
    (read in))

  (define instantiate-linklet
    (case-lambda
     [(linklet import-instances)
      (instantiate-linklet linklet import-instances #f #f)]
     [(linklet import-instances target-instance)
      (instantiate-linklet linklet import-instances target-instance #f)]
     [(linklet import-instances target-instance use-prompt?)
      (cond
       [target-instance
        ;; Instantiate into the given instance and return the
        ;; result of the linklet body:
        (apply
         (if (linklet-compiled? linklet)
             (linklet-code linklet)
             (eval-from-bytevector (linklet-code linklet)))
         (make-variable-reference target-instance #f)
         (append (apply append
                        (map extract-variables
                             import-instances
                             (linklet-importss linklet)
                             (linklet-importss-abi linklet)))
                 (create-variables target-instance
                                   (linklet-exports linklet))))]
       [else
        ;; Make a fresh instance, recur, and return the instance
        (let ([i (make-instance (linklet-name linklet))])
          (instantiate-linklet linklet import-instances i use-prompt?)
          i)])]))
              
  (define (linklet-import-variables linklet)
    (linklet-importss linklet))

  (define (linklet-export-variables linklet)
    (linklet-exports linklet))

  ;; ----------------------------------------

  ;; A potentially mutable import or definition is accessed through
  ;; the indirection of a `variable`; accessing a variable may include
  ;; a check for undefined, since going through a `variable`
  ;; sacrifices the undefined check of the host Scheme
    
  (define-record variable (val name))

  (define (variable-set! var val)
    ;; More is needed here to make sure that a constant is not
    ;; redefined
    (set-variable-val! var val))

  (define (variable-ref var)
    (define v (variable-val var))
    (if (eq? v unsafe-undefined)
        (raise
         (exn:fail:contract:variable
          (string-append (symbol->string (variable-name var))
                         ": undefined;\n cannot reference undefined identifier")
          (current-continuation-marks)
          (variable-name var)))
        v))

  (define (variable-ref/no-check var)
    (variable-val var))

  ;; Find variables or values needed from an instance for a linklet's
  ;; imports
  (define (extract-variables inst syms imports-abi)
    (define ht (instance-hash inst))
    (map (lambda (sym import-abi)
           (let ([var (or (hash-ref ht sym #f)
                          (raise-arguments-error 'instantiate-linklet
                                                 "variable not found in imported instance"
                                                 "instance" inst
                                                 "name" sym))])
             (if import-abi
                 (variable-val var)
                 var)))
         syms
         imports-abi))

  ;; Create the variables needed for a linklet's exports
  (define (create-variables inst syms)
    (define ht (instance-hash inst))
    (map (lambda (sym)
           (or (hash-ref ht sym #f)
               (let ([var (make-variable unsafe-undefined sym)])
                 (hash-set! ht sym var)
                 var)))
         syms))

  ;; ----------------------------------------

  ;; An instance represents the instantiation of a linklet
  (define-record-type (instance new-instance instance?)
    (fields name data hash))

  (define make-instance
    (case-lambda
     [(name) (make-instance name #f)]
     [(name data . content)
      (let ([ht (make-hasheq)])
        (let loop ([content content])
          (cond
           [(null? content) (void)]
           [(null? (cdr content))
            (raise-arguments-error 'make-instance "odd number of arguments")]
           [else
            (hash-set! ht (car content) (make-variable (cadr content) (car content))) 
            (loop (cddr content))]))
        (new-instance name data ht))]))

  (define (instance-variable-names i)
    (hash-map (instance-hash i) (lambda (k v) k)))

  (define instance-variable-value
    (case-lambda
     [(i sym fail-k)
      (define var (hash-ref (instance-hash i) sym unsafe-undefined))
      (define v (if (eq? var unsafe-undefined)
                    unsafe-undefined
                    (variable-val var)))
      (if (eq? v unsafe-undefined)
          (fail-k)
          v)]
     [(i sym)
      (instance-variable-value i
                               sym
                               (lambda ()
                                 (raise-argument-error
                                  'instance-variable-value
                                  "instance variable not found"
                                  "name" sym)))]))

  (define instance-set-variable-value!
    (case-lambda
     [(i k v) (instance-set-variable-value! i k v #f)]
     [(i k v mode)
      (let ([var (or (hash-ref (instance-hash i) k #f)
                     (let ([var (make-variable unsafe-undefined k)])
                       (hash-set! (instance-hash i) k var)
                       var))])
        (set-variable-val! var v))]))

  (define (instance-unset-variable! i k)
    (let ([var (hash-ref (instance-hash i) k #f)])
      (when var
        (set-variable-val! var unsafe-undefined))))

  ;; --------------------------------------------------

  (define-record-type linklet-directory (fields hash))

  (define (hash->linklet-directory ht)
    (make-linklet-directory ht))
  
  (define (linklet-directory->hash ld)
    (linklet-directory-hash ld))

  (define-record-type linklet-bundle (fields hash))

  (define (hash->linklet-bundle ht)
    (make-linklet-bundle ht))

  (define (linklet-bundle->hash b)
    (linklet-bundle-hash b))

  (define-record variable-reference (instance var-or-info))
              
  (define (variable-reference->instance vr)
    (variable-reference-instance vr))

  (define (variable-reference-constant? vr)
    (eq? (variable-reference-var-or-info vr) 'constant))

  (define (make-instance-variable-reference vr v)
    (make-variable-reference (variable-reference-instance vr) v))

  ;; --------------------------------------------------

  ;; Used to wrap a term that isn't annotated, but also doesn't have
  ;; correlated objects or nested annotations:
  (define-record boundary (e stripped-e))
  
  (define (convert-to-annotation old-term new-term)
    (let-values ([(e stripped-e) (remove-annotation-boundary* new-term)])
      (make-boundary (if (correlated? old-term)
                         (transfer-srcloc old-term e stripped-e)
                         e)
                     stripped-e)))

  (define (remove-annotation-boundary term)
    (let-values ([(e stripped-e) (remove-annotation-boundary* term)])
      e))

  (define (unannotate term)
    (let-values ([(e stripped-e) (remove-annotation-boundary* term)])
      stripped-e))

  (define (remove-annotation-boundary* v)
    (cond
     [(boundary? v) (values (boundary-e v)
                            (boundary-stripped-e v))]
     [(pair? v) (let-values ([(a stripped-a) (remove-annotation-boundary* (car v))]
                             [(d stripped-d) (remove-annotation-boundary* (cdr v))])
                  (if (and (eq? a (car v))
                           (eq? d (cdr v)))
                      (values v v)
                      (values (cons a d)
                              (cons stripped-a stripped-d))))]
     [(correlated? v) (let-values ([(e stripped-e) (remove-annotation-boundary* (correlated-e v))])
                        (values (transfer-srcloc v e stripped-e)
                                stripped-e))]
     ;; correlated or boundary will be nested only in pairs
     ;; with current expander and schemifier
     [else (values v v)]))

  (define (transfer-srcloc v e stripped-e)
    (let ([src (correlated-source v)]
          [pos (correlated-position v)]
          [span (correlated-span v)])
      (if (and pos span (or (path? src) (string? src)))
          (make-annotation e (make-source-object (source->sfd src) pos (+ pos span)) stripped-e)
          e)))

  (define sfd-cache (make-weak-hash))
  ;; FIXME: Using an empty port means that file positions will be
  ;; reported instead of line and column numbers. Using actual file
  ;; content at this point raises all sorts of issues, though.
  (define empty-port (open-bytevector-input-port '#vu8()))

  (define (source->sfd src)
    (or (hash-ref sfd-cache src #f)
        (let ([str (if (path? src)
                       (path->string src)
                       src)])
          (let ([sfd (make-source-file-descriptor str empty-port)])
            (hash-set! sfd-cache src sfd)
            sfd))))

  ;; --------------------------------------------------

  ;; Intentionally indirect, so that the compiler doesn't
  ;; spend effort inlining:
  (eval `(define variable-set! ',variable-set!))
  (eval `(define variable-ref ',variable-ref))
  (eval `(define variable-ref/no-check ',variable-ref/no-check))
  (eval `(define make-instance-variable-reference ',make-instance-variable-reference))

  (void))
