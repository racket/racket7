(library (linklet)
  (export linklet?
          compile-linklet
          recompile-linklet
          eval-linklet
          read-compiled-linklet
          instantiate-linklet

          read-on-demand-source

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
          variable-reference-constant?

          compile-enforce-module-constants
          compile-context-preservation-enabled
          compile-allow-set!-undefined
          eval-jit-enabled
          load-on-demand-enabled

          jit-mode? ; not exported to racket
          linklet-performance-init!   ; not exported to racket
          linklet-performance-report! ; not exported to racket

          install-linklet-primitive-tables!  ; not exported to racket
          
          ;; schemify glue:
          variable-set!
          variable-ref
          variable-ref/no-check
          make-instance-variable-reference
          unbox/check-undefined
          set-box!/check-undefined
          jit-extract-closed
          jit-extract
          schemify-table)
  (import (chezpart)
          (only (chezscheme) printf)
          (rumble)
          (only (io)
                path?
                complete-path?
                path->string
                string->bytes/utf-8
                bytes->string/utf-8
                prop:custom-write
                write-bytes
                read-byte
                read-bytes
                open-output-bytes
                get-output-bytes
                file-position)
          (regexp)
          (schemify))

  (define jit-mode?
    (cond
     [(getenv "PLT_CS_JIT") #t]
     [(getenv "PLT_CS_MACH") #f]
     [else #f]))

  (define (primitive->compiled-position prim) #f)
  (define (compiled-position->primitive pos) #f)

  (define omit-debugging? (not (getenv "PLT_CS_DEBUG")))
  (define measure-performance? (getenv "PLT_LINKLET_TIMES"))

  (define gensym-on? (getenv "PLT_LINKLET_SHOW_GENSYM"))
  (define pre-lift-on? (getenv "PLT_LINKLET_SHOW_PRE_LIFT"))
  (define pre-jit-on? (getenv "PLT_LINKLET_SHOW_PRE_JIT"))
  (define jit-demand-on? (getenv "PLT_LINKLET_SHOW_JIT_DEMAND"))
  (define show-on? (or gensym-on?
                       pre-jit-on?
                       pre-lift-on?
                       jit-demand-on?
                       (getenv "PLT_LINKLET_SHOW")))
  (define show
    (case-lambda
     [(what v) (show show-on? what v)]
     [(on? what v)
      (when on?
        (printf ";; ~a ---------------------\n" what)
        (call-with-system-wind
         (lambda ()
           (parameterize ([print-gensym gensym-on?]
                          [print-extended-identifiers #t])
             (pretty-print (strip-jit-wrapper
                            (strip-nested-annotations
                             (remove-annotation-boundary
                              (convert-to-annotation #f v)))))))))
      v]))

  (define region-times (make-eq-hashtable))
  (define region-counts (make-eq-hashtable))
  (define region-memories (make-eq-hashtable))
  (define current-start-time 0)
  (define-syntax performance-region
    (syntax-rules ()
      [(_ label e ...) (measure-performance-region label (lambda () e ...))]))
  (define (measure-performance-region label thunk)
    (cond
     [measure-performance?
      (let ([old-start current-start-time])
        (set! current-start-time (current-inexact-milliseconds))
        (begin0
         (thunk)
         (let ([delta (- (current-inexact-milliseconds) current-start-time)])
           (hashtable-update! region-times label (lambda (v) (+ v delta)) 0)
           (hashtable-update! region-counts label add1 0)
           (set! current-start-time (+ old-start delta)))))]
     [else (thunk)]))
  (define (add-performance-memory! label delta)
    (when measure-performance?
      (hashtable-update! region-memories label (lambda (v) (+ v delta)) 0)))
  (define (linklet-performance-init!)
    (hashtable-set! region-times 'boot
                    (let ([t (sstats-cpu (statistics))])
                      (+ (* 1000.0 (time-second t))
                         (/ (time-nanosecond t) 1000000.0)))))
  (define (linklet-performance-report!)
    (when measure-performance?
      (let ([total 0])
        (define (report label n units extra)
          (define (pad v w)
            (let ([s (chez:format "~a" v)])
              (string-append (make-string (max 0 (- w (string-length s))) #\space)
                             s)))
          (chez:printf ";; ~a: ~a ~a~a\n"
                       (pad label 15)
                       (pad (round (inexact->exact n)) 5)
                       units
                       extra))
        (define (ht->sorted-list ht)
          (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                     (hash-table-map ht cons)))
        (for-each (lambda (p)
                    (let ([label (car p)]
                          [n (cdr p)])
                      (set! total (+ total n))
                      (report label n 'ms (let ([c (hashtable-ref region-counts label 0)])
                                            (if (zero? c)
                                                ""
                                                (chez:format " ; ~a times" c))))))
                  (ht->sorted-list region-times))
        (report 'total total 'ms "")
        (chez:printf ";;\n")
        (for-each (lambda (p) (report (car p) (/ (cdr p) 1024 1024) 'MB ""))
                  (ht->sorted-list region-memories)))))

  ;; `compile`, `interpret`, etc. have `dynamic-wind`-based state
  ;; that need to be managed correctly when swapping Racket
  ;; engines/threads.
  (define (compile* e)
    (call-with-system-wind (lambda () (compile e))))
  (define (interpret* e)
    (call-with-system-wind (lambda () (interpret e))))
  (define (fasl-write* s o)
    (call-with-system-wind (lambda () (fasl-write s o))))
  (define (compile-to-port* s o)
    (call-with-system-wind (lambda () (compile-to-port s o))))

  (define primitives (make-hasheq))
  (define (install-linklet-primitive-tables! . tables)
    (for-each
     (lambda (table)
       (hash-for-each table (lambda (k v) (hash-set! primitives k v))))
     tables))
  
  (define (outer-eval s)
    (if jit-mode?
        (interpret-linklet s primitives variable-ref variable-ref/no-check variable-set!)
        (compile* s)))

  (define (compile-to-bytevector s)
    (let-values ([(o get) (open-bytevector-output-port)])
      (cond
       [jit-mode? (fasl-write* s o)]
       [else (compile-to-port* (list `(lambda () ,s)) o)])
      (bytevector-compress (get))))

  (define (eval-from-bytevector c-bv)
    (add-performance-memory! 'uncompress (bytevector-length c-bv))
    (let* ([bv (performance-region
                'uncompress
                (bytevector-uncompress c-bv))]
           [i (open-bytevector-input-port bv)])
      (add-performance-memory! 'faslin (bytevector-length bv))
      (cond
       [jit-mode?
        (let ([r (performance-region
                  'faslin
                  (fasl-read i))])
          (performance-region
           'outer
           (outer-eval r)))]
       [else
        ;; HACK: This probably always works for the `lambda` or
        ;; `let`+`lambda` forms that we compile as linklets, but we need a
        ;; better function from the host Scheme:
        (let ([v (performance-region
                  'faslin
                  (fasl-read i))])
          (performance-region
           'outer
           (((cdr (if (vector? v) (vector-ref v 1) v))))))])))

  (define-record-type wrapped-annotation
    (fields (mutable content)
            arity-mask
            name)
    (nongenerative #{wrapped-annotation p6o2m72rgmi36pm8vy559b-0}))

  (define (force-wrapped-annotation wa)
    (let ([f (wrapped-annotation-content wa)])
      (if (procedure? f)
          f
          (performance-region
           'on-demand
           (let* ([f (compile* (wrapped-annotation-content wa))])
             (when jit-demand-on?
               (show "JIT demand" (strip-nested-annotations (wrapped-annotation-content wa))))
             (wrapped-annotation-content-set! wa f)
             f)))))

  (define (jit-extract-closed wa)
    (let ([f (wrapped-annotation-content wa)])
      (if (#2%procedure? f)
          ;; previously JITted, so no need for a wrapper
          f
          ;; make a wrapper that has the right arity and name
          ;; and that compiles when called:
          (make-jit-procedure (lambda () (force-wrapped-annotation wa))
                              (wrapped-annotation-arity-mask wa)
                              (wrapped-annotation-name wa)))))

  (define (jit-extract wa)
    (let ([f (wrapped-annotation-content wa)])
      (if (#2%procedure? f)
          ;; previously JITted, so no need for a wrapper
          f
          ;; make a wrapper that has the right arity and name
          ;; and that compiles when called:
          (lambda free-vars
            (make-jit-procedure (lambda ()
                                  (apply (force-wrapped-annotation wa)
                                         free-vars))
                                (wrapped-annotation-arity-mask wa)
                                (wrapped-annotation-name wa))))))

  (define (strip-jit-wrapper p)
    (cond
     [(wrapped-annotation? p)
      (vector (strip-jit-wrapper (strip-nested-annotations (wrapped-annotation-content p)))
              (wrapped-annotation-arity-mask p)
              (wrapped-annotation-name p))]
     [(pair? p)
      (cons (strip-jit-wrapper (car p)) (strip-jit-wrapper (cdr p)))]
     [else p]))
  
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

  (define-record-type linklet
    (fields (mutable code) ; the procedure
            (mutable format) ; 'faslable, 'faslable-strict, 'callable, or 'lazy
            importss-abi   ; ABI for each import, in parallel to `importss`
            exports-info   ; hash(sym -> known) for info about each export; see "known.rkt"
            name           ; name of the linklet (for debugging purposes)
            importss       ; list of list of import symbols
            exports)       ; list of export symbols
    (nongenerative #{linklet cuquy0g9bh5vmeespyap4g-0}))

  (define (set-linklet-code linklet code format)
    (make-linklet code
                  format
                  (linklet-importss-abi linklet)
                  (linklet-exports-info linklet)
                  (linklet-name linklet)
                  (linklet-importss linklet)
                  (linklet-exports linklet)))

  (define compile-linklet
    (case-lambda
     [(c) (compile-linklet c #f #f (lambda (key) (values #f #f)) #t)]
     [(c name) (compile-linklet c name #f (lambda (key) (values #f #f)) #t)]
     [(c name import-keys) (compile-linklet c name import-keys (lambda (key) (values #f #f)) #t)]
     [(c name import-keys get-import) (compile-linklet c name import-keys get-import #t)]
     [(c name import-keys get-import serializable?)
      (performance-region
       'compile
       (define importss
         (map (lambda (ps)
                (map (lambda (p) (if (pair? p) (car p) p))
                     ps))
              (cadr c)))
       (define exports
         (map (lambda (p) (if (pair? p) (cadr p) p))
              (caddr c)))
       ;; Convert the linklet S-expression to a `lambda` S-expression:
       (define-values (impl-lam importss-abi exports-info)
         (schemify-linklet (show "linklet" c)
                           serializable?
                           jit-mode? ; immediate installation of variables needed for jitify
                           convert-to-annotation
                           unannotate
                           prim-knowns
                           ;; Callback to get a specific linklet for a
                           ;; given import:
                           (lambda (index)
                             (lookup-linklet-or-instance get-import import-keys index))))
       (define impl-lam/lifts
         (lift-in-schemified-linklet (show pre-lift-on? "pre-lift" (remove-annotation-boundary impl-lam))
                                     reannotate
                                     unannotate))
       (define impl-lam/jitified
         (if jit-mode?
             (jitify-schemified-linklet (show pre-jit-on? "pre-JIT" impl-lam/lifts)
                                        (lambda (expr arity-mask name)
                                          (make-wrapped-annotation expr arity-mask name))
                                        reannotate
                                        unannotate)
             impl-lam/lifts))
       (define impl-lam/interpable
         (let ([l (show "schemified" impl-lam/jitified)])
           (if jit-mode?
               (interpretable-jitified-linklet l unannotate)
               l)))
       ;; Create the linklet:
       (let ([lk (make-linklet (call-with-system-wind
                                (lambda ()
                                  ((if serializable? compile-to-bytevector outer-eval)
                                   impl-lam/interpable)))
                               (if serializable? 'faslable 'callable)
                               importss-abi
                               exports-info
                               name
                               importss
                               exports)])
         (show "compiled" 'done)
         ;; In general, `compile-linklet` is allowed to extend the set
         ;; of linklet imports if `import-keys` is provided (e.g., for
         ;; cross-linklet optimization where inlining needs a new
         ;; direct import) - but we don't do that, currently
         (if import-keys
             (values lk import-keys)
             lk)))]))

  (define (lookup-linklet-or-instance get-import import-keys index)
    ;; Use the provided callback to get an linklet for the
    ;; import at `index`
    (cond
     [(and get-import
           import-keys
           (vector-ref import-keys index))
      => (lambda (key)
           (let-values ([(lnk/inst more-import-keys) (get-import key)])
             (cond
              [(linklet? lnk/inst)
               (values (linklet-exports-info lnk/inst)
                       ;; No conversion needed:
                       #f)]
              [(instance? lnk/inst)
               (values (instance-hash lnk/inst)
                       variable->known)]
              [else (values #f #f)])))]
     [else (values #f #f)]))

  (define (recompile-linklet lnk . args) lnk)

  ;; Intended to speed up reuse of a linklet in exchange for not being
  ;; able to serialize anymore
  (define (eval-linklet linklet)
    (case (linklet-format linklet)
      [(faslable)
       (set-linklet-code linklet (linklet-code linklet) 'lazy)]
      [(faslable-strict)
       (set-linklet-code linklet (eval-from-bytevector (linklet-code linklet)) 'callable)]
      [else
       linklet]))
     
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
        (call/cc
         (lambda (k)
           (register-linklet-instantiate-continuation! k (instance-name target-instance))
           (when (eq? 'lazy (linklet-format linklet))
             ;; Trigger lazy conversion of code from bytevector
             (let ([code (eval-from-bytevector (linklet-code linklet))])
               (with-interrupts-disabled
                (when (eq? 'lazy (linklet-format linklet))
                  (linklet-code-set! linklet code)
                  (linklet-format-set! linklet 'callable)))))
           ;; Call the linklet:
           (performance-region
            'instantiate
            (apply
             (if (eq? 'callable (linklet-format linklet))
                 (linklet-code linklet)
                 (eval-from-bytevector (linklet-code linklet)))
             (make-variable-reference target-instance #f)
             (append (apply append
                            (map extract-variables
                                 import-instances
                                 (linklet-importss linklet)
                                 (linklet-importss-abi linklet)))
                     (create-variables target-instance
                                       (linklet-exports linklet)))))))]
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
    
  (define-record variable (val
                           name
                           constance)) ; #f (mutable), 'constant, or 'consistent (always the same shape)

  (define (variable-set! var val constance)
    (cond
     [(variable-constance var)
      (raise
       (|#%app|
        exn:fail:contract:variable
        (string-append (symbol->string (variable-name var))
                       ": cannot modify constant")
        (current-continuation-marks)
        (variable-name var)))]
     [else
      (set-variable-val! var val)
      (when constance
        (set-variable-constance! var constance))]))

  (define (variable-ref var)
    (define v (variable-val var))
    (if (eq? v unsafe-undefined)
        (raise
         (|#%app|
          exn:fail:contract:variable
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
               (let ([var (make-variable unsafe-undefined sym #f)])
                 (hash-set! ht sym var)
                 var)))
         syms))

  (define (variable->known var)
    (let ([constance (variable-constance var)])
      (cond
       [(not constance) #f]
       [(and (eq? constance 'consistent)
             (#%procedure? (variable-val var)))
        a-known-procedure]
       [else a-known-constant])))

  ;; ----------------------------------------

  ;; An instance represents the instantiation of a linklet
  (define-record-type (instance new-instance instance?)
    (fields name
            data
            hash)) ; symbol -> variable

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
            (hash-set! ht (car content) (make-variable (cadr content) (car content) #f))
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
                     (let ([var (make-variable unsafe-undefined k #f)])
                       (hash-set! (instance-hash i) k var)
                       var))])
        (variable-set! var v mode))]))

  (define (instance-unset-variable! i k)
    (let ([var (hash-ref (instance-hash i) k #f)])
      (when var
        (set-variable-val! var unsafe-undefined))))

  ;; --------------------------------------------------

  (define-record-type linklet-directory
    (fields hash)
    (nongenerative #{linklet-directory cvqw30w53xy6hsjsc5ipep-0}))

  (define (hash->linklet-directory ht)
    (make-linklet-directory ht))
  
  (define (linklet-directory->hash ld)
    (linklet-directory-hash ld))

  (define-record-type (linklet-bundle make-linklet-bundle linklet-bundle?)
    (fields (immutable hash))
    (nongenerative #{linklet-bundle chqh4u4pk0me3osmzzx8pq-0}))

  (define (install-linklet-bundle-write!)
    (struct-property-set! prop:custom-write (record-type-descriptor linklet-bundle) write-linklet-bundle)
    (struct-property-set! prop:custom-write (record-type-descriptor linklet-directory) write-linklet-directory))

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

  ;; ----------------------------------------

  (define (write-linklet-bundle b port mode)
    ;; Various tools expect a particular header:
    ;;   "#~"
    ;;   length of version byte string (< 64) as one byte
    ;;   version byte string
    ;;   "B"
    ;;   20 bytes of SHA-1 hash
    (write-bytes '#vu8(35 126) port)
    (let ([vers (string->bytes/utf-8 (version))])
      (write-bytes (bytes (bytes-length vers)) port)
      (write-bytes vers port))
    (write-bytes '#vu8(66) port)
    (write-bytes (make-bytes 20 0) port)
    ;; The rest is whatever we want. We'll simply fasl the bundle.
    (let-values ([(o get) (open-bytevector-output-port)])
      (fasl-write* b o)
      (let ([bstr (get)])
        (write-int (bytes-length bstr) port)
        (write-bytes bstr port))))

  (define (linklet-bundle->bytes b)
    (let ([o (open-output-bytes)])
      (write-linklet-bundle b o #t)
      (get-output-bytes o)))

  (define (write-linklet-directory ld port mode)
    ;; Various tools expect a particular header:
    ;;   "#~"
    ;;   length of version byte string (< 64) as one byte
    ;;   version byte string
    ;;   "D"
    ;;   bundle count as 4-byte integer
    ;;   binary tree:
    ;;     bundle-name length as 4-byte integer
    ;;     bundle name [encoding decribed below]
    ;;     bundle offset as 4-byte integer
    ;;     bundle size as 4-byte integer
    ;;     left-branch offset as 4-byte integer
    ;;     right-branch offset as 4-byte integer
    ;; A bundle name corresponds to a list of symbols. Each symbol in the list is
    ;; prefixed with either: its length as a byte if less than 255; 255 followed by
    ;; a 4-byte integer for the length.
    (write-bytes '#vu8(35 126) port)
    (let ([vers (string->bytes/utf-8 (version))])
      (write-bytes (bytes (bytes-length vers)) port)
      (write-bytes vers port)
      (write-bytes '#vu8(68) port)
      ;; Flatten a directory of bundles into a vector of pairs, where
      ;; each pair has the encoded bundle name and the bundle bytes
      (let* ([bundles (list->vector (flatten-linklet-directory ld '() '()))]
             [len (vector-length bundles)]
             [initial-offset (+ 2 ; "#~"
                                1 ; version length
                                (bytes-length vers)
                                1 ; D
                                4)]) ; bundle count
        (write-int len port) ; bundle count
        (chez:vector-sort! (lambda (a b) (bytes<? (car a) (car b))) bundles)
        ;; Compute bundle offsets
        (let* ([btree-size (compute-btree-size bundles len)]
               [node-offsets (compute-btree-node-offsets bundles len initial-offset)]
               [bundle-offsets (compute-bundle-offsets bundles len (+ initial-offset btree-size))])
          (write-directory-btree bundles node-offsets bundle-offsets len port)
          ;; Write the bundles
          (let loop ([i 0])
            (unless (fx= i len)
              (write-bytes (cdr (vector-ref bundles i)) port)
              (loop (fx1+ i))))))))

  ;; Flatten a tree into a list of `(cons _name-bstr _bundle-bstr)`
  (define (flatten-linklet-directory ld rev-name-prefix accum)
    (let ([ht (linklet-directory-hash ld)])
      (let loop ([i (hash-iterate-first ht)] [accum accum] [saw-bundle? #f])
        (cond
         [(not i)
          (if saw-bundle?
              accum
              (cons (cons (encode-name rev-name-prefix)
                          '#vu8(35 102))
                    accum))]
         [else
          (let-values ([(key value) (hash-iterate-key+value ht i)])
            (cond
             [(eq? key #f)
              (loop (hash-iterate-next ht i)
                    (cons (cons (encode-name rev-name-prefix)
                                (linklet-bundle->bytes value))
                          accum)
                    #t)]
             [else
              (loop (hash-iterate-next ht i)
                    (flatten-linklet-directory value (cons key rev-name-prefix) accum)
                    saw-bundle?)]))]))))

  ;; Encode a bundle name (as a reversed list of symbols) as a single
  ;; byte string
  (define (encode-name rev-name)
    (define (encode-symbol s)
      (let* ([bstr (string->bytes/utf-8 (symbol->string s))]
             [len (bytes-length bstr)])
        (if (< len 255)
            (list (bytes len) bstr)
            (list (bytes 255) (integer->integer-bytes len 4 #f #f) bstr))))
    (let loop ([rev-name rev-name] [accum '()])
      (cond
       [(null? rev-name) (apply bytes-append accum)]
       [else
        (loop (cdr rev-name) (append (encode-symbol (car rev-name))
                                     accum))])))

  ;; Figure out how big the binary tree will be, which depends
  ;; on the size of bundle-name byte strings
  (define (compute-btree-size bundles len)
    (let loop ([i 0] [size 0])
      (if (= i len)
          size
          (let ([nlen (bytes-length (car (vector-ref bundles i)))])
            ;; 5 numbers: name length, bundle offset, bundles size, lef, and right
            (loop (fx1+ i) (+ size nlen (* 5 4)))))))

  ;; Compute the offset where each node in the binary tree will reside
  ;; relative to the start of the bundle directory's "#~"
  (define (compute-btree-node-offsets bundles len initial-offset)
    (let ([node-offsets (make-vector len)])
      (let loop ([lo 0] [hi len] [offset initial-offset])
        (cond
         [(= lo hi) offset]
         [else
          (let* ([mid (quotient (+ lo hi) 2)])
            (vector-set! node-offsets mid offset)
            (let* ([nlen (bytes-length (car (vector-ref bundles mid)))]
                   [offset (+ offset 4 nlen 4 4 4 4)])
              (let ([offset (loop lo mid offset)])
                (loop (add1 mid) hi offset))))]))
      node-offsets))

  ;; Compute the offset where each bundle will reside relative
  ;; to the start of the bundle directory's "#~"
  (define (compute-bundle-offsets bundles len offset)
    (let ([bundle-offsets (make-vector len)])
      (let loop ([i 0] [offset offset])
        (unless (= i len)
          (vector-set! bundle-offsets i offset)
          (loop (fx1+ i) (+ offset (bytes-length (cdr (vector-ref bundles i)))))))
      bundle-offsets))

  ;; Write the binary tree for the directory:
  (define (write-directory-btree bundles node-offsets bundle-offsets len port)
    (let loop ([lo 0] [hi len])
      (cond
       [(= lo hi) (void)]
       [else
        (let* ([mid (quotient (+ lo hi) 2)]
               [p (vector-ref bundles mid)]
               [nlen (bytes-length (car p))])
          (write-int nlen port)
          (write-bytes (car p) port)
          (write-int (vector-ref bundle-offsets mid) port)
          (write-int (bytes-length (cdr p)) port)
          (cond
           [(> mid lo)
            (let ([left (quotient (+ lo mid) 2)])
              (write-int (vector-ref node-offsets left) port))]
           [else
            (write-int 0 port)])
          (cond
           [(< (fx1+ mid) hi)
            (let ([right (quotient (+ (fx1+ mid) hi) 2)])
              (write-int (vector-ref node-offsets right) port))]
           [else
            (write-int 0 port)])
          (loop lo mid)
          (loop (fx1+ mid) hi))])))

  (define (write-int n port)
    (write-bytes (integer->integer-bytes n 4 #f #f) port))

  ;; --------------------------------------------------

  (define (read-compiled-linklet in)
    (read-compiled-linklet-or-directory in #t))
  
  (define (read-compiled-linklet-or-directory in initial?)
    ;; `#~` has already been read
    (let* ([start-pos (- (file-position in) 2)]
           [vers-len (min 63 (read-byte in))]
           [vers (read-bytes vers-len in)])
      (unless (equal? vers (string->bytes/utf-8 (version)))
        (raise-arguments-error 'read-compiled-linklet
                               "version mismatch"
                               "expected" (version)
                               "found" (bytes->string/utf-8 vers #\?)))
      (let ([tag (read-byte in)])
        (cond
         [(equal? tag (char->integer #\B))
          (let ([sha-1 (read-bytes 20 in)])
            (let ([len (read-int in)])
              (let ([bstr (read-bytes len in)])
                (let ([b (fasl-read (open-bytevector-input-port bstr))])
                  (add-hash-code (adjust-linklet-bundle-laziness
                                  (if initial?
                                      (strip-submodule-references b)
                                      b))
                                 sha-1)))))]
         [(equal? tag (char->integer #\D))
          (unless initial?
            (raise-argument-error 'read-compiled-linklet
                                  "expected a linklet bundle"))
          (read-bundle-directory in start-pos)]
         [else
          (raise-arguments-error 'read-compiled-linklet
                                 "expected a `B` or `D`")]))))

  (define (read-int in)
    (integer-bytes->integer (read-bytes 4 in) #f #f))
  
  (define (read-bundle-directory in pos)
    (let ([count (read-int in)])
      (let ([position-to-name
             (let loop ([count count] [accum (hasheqv)])
               (cond
                [(zero? count) accum]
                [else
                 (let ([bstr (read-bytes (read-int in) in)])
                   (let* ([offset (read-int in)]
                          [len (read-int in)])
                     (read-int in) ; left
                     (read-int in) ; right
                     (loop (fx1- count)
                           (hash-set accum offset bstr))))]))])
        (let loop ([count count] [accum '()])
          (cond
           [(zero? count)
            (list->bundle-directory accum)]
           [else
            (let ([name (hash-ref position-to-name (- (file-position in) pos) #f)])
              (unless name
                (raise-arguments-error 'read-compiled-linklet
                                       "bundle not at an expected file position"))
              (let ([bstr (read-bytes 2 in)])
                (let ([bundle
                       (cond
                        [(equal? '#vu8(35 126) bstr)
                         (read-compiled-linklet in)]
                        [(equal? '#vu8(35 102) bstr)
                         #f]
                        [else
                         (raise-arguments-error 'read-compiled-linklet
                                                "expected a `#~` or `#f` for a bundle")])])
                  (loop (fx1- count)
                        (cons (cons (decode-name name pos) bundle) accum)))))])))))

  (define (decode-name bstr pos)
    (cond
     [(= pos (bytes-length bstr))
      '()]
     [else
      (let ([len (bytes-ref bstr pos)])
        (if (= len 255)
            (let ([len (integer-bytes->integer bstr #f #f (fx1+ pos) (fx+ pos 5))])
              (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (fx+ pos 5) (+ pos 5 len)) #\?))
                    (decode-name bstr (+ pos 5 len))))
            (cons (string->symbol (bytes->string/utf-8 (subbytes bstr (fx1+ pos) (+ pos 1 len)) #\?))
                  (decode-name bstr (+ pos 1 len)))))]))

  ;; Convert a post-order list into a tree
  (define (list->bundle-directory l)
    ;; The bundles list is in post-order, so we can build directories
    ;; bottom-up
    (let loop ([l l] [prev-len 0] [stack '()] [accum (hasheq)])
      (when (null? l)
        (raise-arguments-error 'read-compiled-linklet
                               "invalid bundle sequence"))
      (let* ([p (car l)]
             [path (car p)]
             [v (cdr p)]
             [len (length path)])
        (when (< len prev-len)
          (raise-arguments-error 'read-compiled-linklet
                                 "invalid bundle sequence"))
        (let sloop ([prev-len prev-len] [stack stack] [accum accum])
          (cond
           [(> len (fx1+ prev-len))
            (sloop (fx1+ prev-len)
                   (cons accum stack)
                   (hasheq))]
           [else
            (let ([path (list-tail path (fxmax 0 (fx1- prev-len)))])
              (cond
               [(= len prev-len)
                (let ([accum (if v
                                 (hash-set accum #f v)
                                 accum)])
                  (if (zero? len)
                      (make-linklet-directory accum)
                      (loop (cdr l)
                            (fx1- prev-len)
                            (cdr stack)
                            (hash-set (car stack) (car path) (make-linklet-directory accum)))))]
               [else
                (let ([path (if (positive? prev-len)
                                (cdr path)
                                path)])
                  (loop (cdr l)
                        prev-len
                        stack
                        (hash-set accum
                                  (car path)
                                  (make-linklet-directory (if v
                                                              (hasheq #f v)
                                                              (hasheq))))))]))])))))

  ;; When a bundle is loaded by itself, remove any 'pre and 'post
  ;; submodule descriptions:
  (define (strip-submodule-references b)
    (make-linklet-bundle (hash-remove (hash-remove (linklet-bundle-hash b) 'pre) 'post)))

  ;; If the bundle has a non-zero hash code, record it with the
  ;; 'hash-code key to enable module caching
  (define (add-hash-code b sha-1)
    (if (bytevector=? sha-1 '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        b
        (make-linklet-bundle (hash-set (linklet-bundle-hash b) 'hash-code sha-1))))

  (define read-on-demand-source
    (make-parameter #f
                    (lambda (v)
                      (unless (or (eq? v #t) (eq? v #f) (and (path? v)
                                                             (complete-path? v)))
                        (raise-argument-error 'read-on-demand-source
                                              "(or/c #f #t (and/c path? complete-path?))"
                                              v))
                      v)))

  (define (adjust-linklet-bundle-laziness b)
    (make-linklet-bundle
     (let ([ht (linklet-bundle-hash b)])
       (let loop ([i (hash-iterate-first ht)])
         (cond
          [(not i) (hasheq)]
          [else
           (let-values ([(key val) (hash-iterate-key+value ht i)])
             (hash-set (loop (hash-iterate-next ht i))
                       key
                       (if (linklet? val)
                           (adjust-linklet-laziness val)
                           val)))])))))
                  
  (define (adjust-linklet-laziness linklet)
    (set-linklet-code linklet
                      (linklet-code linklet)
                      (if (|#%app| read-on-demand-source)
                          'faslable
                          'faslable-strict)))
  
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
          [line (correlated-line v)]
          [column (correlated-column v)]
          [span (correlated-span v)])
      (if (and pos span (or (path? src) (string? src)))
          (let ([pos (sub1 pos)]) ; Racket positions are 1-based; host Scheme positions are 0-based
            (make-annotation e
                             (if (and line column)
                                 ;; Racket columns are 0-based; host-Scheme columns are 1-based
                                 (make-source-object (source->sfd src) pos (+ pos span) line (add1 column))
                                 (make-source-object (source->sfd src) pos (+ pos span)))
                             stripped-e))
          e)))

  (define sfd-cache (make-weak-hash))

  (define (source->sfd src)
    (or (hash-ref sfd-cache src #f)
        (let ([str (if (path? src)
                       (path->string src)
                       src)])
          ;; We'll use a file-position object in source objects, so
          ;; the sfd checksum doesn't matter
          (let ([sfd (source-file-descriptor str 0)])
            (hash-set! sfd-cache src sfd)
            sfd))))

  ;; --------------------------------------------------
  
  (define (reannotate old-term new-term)
    (if (annotation? old-term)
        (make-annotation new-term
                         (annotation-source old-term)
                         (strip-nested-annotations new-term))
        new-term))

  (define (strip-nested-annotations s)
    (cond
     [(annotation? s) (annotation-stripped s)]
     [(pair? s)
      (let ([a (strip-nested-annotations (car s))]
            [d (strip-nested-annotations (cdr s))])
        (if (and (eq? a (car s)) (eq? d (cdr s)))
            s
            (cons a d)))]
     [else s]))

  ;; --------------------------------------------------

  (define (unbox/check-undefined b name)
    (check-not-unsafe-undefined (#3%unbox b) name))

  (define (set-box!/check-undefined b v name)
    (check-not-unsafe-undefined/assign (unbox b) name)
    (#3%set-box! b v))
  
  ;; --------------------------------------------------
  
  (define compile-enforce-module-constants
    (make-parameter #t (lambda (v) (and v #t))))

  (define compile-context-preservation-enabled
    (make-parameter #f (lambda (v) (and v #t))))

  (define compile-allow-set!-undefined
    (make-parameter #f (lambda (v) (and v #t))))

  (define eval-jit-enabled
    (make-parameter #t (lambda (v) (and v #t))))
  
  (define load-on-demand-enabled
    (make-parameter #t (lambda (v) (and v #t))))

  ;; --------------------------------------------------

  (define-syntax primitive-table
    (syntax-rules ()
      [(_ id ...)
       (let ([ht (make-hasheq)])
         (hash-set! ht 'id id) ...
         ht)]))

  (define schemify-table
    (primitive-table
     variable-set!
     variable-ref
     variable-ref/no-check
     make-instance-variable-reference
     unbox/check-undefined
     set-box!/check-undefined
     jit-extract
     jit-extract-closed))

  ;; --------------------------------------------------

  (when omit-debugging?
    (generate-inspector-information (not omit-debugging?))
    (generate-procedure-source-information #t))

  (install-linklet-bundle-write!))
