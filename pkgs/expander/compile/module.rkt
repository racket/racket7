#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../common/phase.rkt"
         "../namespace/core.rkt"
         "../namespace/module.rkt"
         "../common/module-path.rkt"
         "../common/performance.rkt"
         "../expand/parsed.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         "../host/linklet.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "form.rkt"
         "compiled-in-memory.rkt")

(provide compile-module)

;; Compiles module to a set of linklets that is returned as a
;; `compiled-in-memory` --- or a hash table containing S-expression
;; linklets if `to-source?` is true.
(define (compile-module p cctx
                        #:with-submodules? [with-submodules? #t]
                        #:as-submodule? [as-submodule? #f]
                        #:serializable? [serializable? with-submodules?]
                        #:to-source? [to-source? #f]
                        #:modules-being-compiled [modules-being-compiled (and with-submodules?
                                                                              (make-hasheq))])
  (performance-region
   ['compile 'module]
   
   ;; Some information about a module is commuicated here through syntax
   ;; propertoes, such as 'module-requires
   (define enclosing-self (compile-context-module-self cctx))
   (define self (parsed-module-self p))
   (define full-module-name (let ([parent-full-name (compile-context-full-module-name cctx)]
                                  [name (syntax-e (parsed-module-name-id p))])
                              (if parent-full-name
                                  (append (if (list? parent-full-name)
                                              parent-full-name
                                              (list parent-full-name))
                                          (list name))
                                  name)))
   (define requires (parsed-module-requires p))
   (define provides (parsed-module-provides p))
   (define encoded-root-expand-ctx-box (box (parsed-module-encoded-root-ctx p))) ; for `module->namespace`
   (define body-context-simple? (parsed-module-root-ctx-simple? p))
   (define language-info (filter-language-info (syntax-property (parsed-s p) 'module-language)))
   (define bodys (parsed-module-body p))
   
   (define empty-result-for-module->namespace? #f)

   (define mpis (make-module-path-index-table))
   
   (define body-cctx (struct-copy compile-context cctx
                                  [phase 0]
                                  [self self]
                                  [module-self self]
                                  [full-module-name full-module-name]
                                  [lazy-syntax-literals? #t]))
   
   (define cross-phase-persistent? #f)
   
   ;; Callback to track phases that have side effects
   (define side-effects (make-hasheqv))
   (define (check-side-effects! e ; compiled expression
                                expected-results ; number of expected results, or #f if any number is ok
                                phase
                                required-reference?)
     (unless (hash-ref side-effects phase #f)
       (when (any-side-effects? e expected-results required-reference?)
         (hash-set! side-effects phase #t))))

   ;; Compile submodules; each list is (cons linklet-directory-key compiled-in-memory)
   (define pre-submodules (compile-submodules #:star? #f
                                              #:bodys bodys
                                              #:with-submodules? with-submodules?
                                              #:serializable? serializable?
                                              #:to-source? to-source?
                                              #:cctx body-cctx
                                              #:modules-being-compiled modules-being-compiled))

   ;; Compile the sequence of body forms:
   (define-values (body-linklets
                   min-phase
                   max-phase
                   phase-to-link-module-uses
                   phase-to-link-module-uses-expr
                   phase-to-link-extra-inspectorsss
                   syntax-literals
                   root-ctx-pos)
     (compile-forms bodys body-cctx mpis
                    #:body-imports `([,get-syntax-literal!-id]
                                     [,set-transformer!-id])
                    #:body-suffix-forms '((void)) ; otherwise, compiler always preserves last form
                    #:force-phases '(0) ; minor hack for more consistent compilation
                    #:encoded-root-expand-ctx-box encoded-root-expand-ctx-box
                    #:root-ctx-only-if-syntax? body-context-simple?
                    #:compiled-expression-callback check-side-effects!
                    #:other-form-callback (lambda (body cctx)
                                            (cond
                                              [(parsed-#%declare? body)
                                               (define-match m (parsed-s body) '(_ kw ...))
                                               (for ([kw (in-list (m 'kw))])
                                                 (when (eq? (syntax-e kw) '#:cross-phase-persistent)
                                                   (set! cross-phase-persistent? #t))
                                                 (when (eq? (syntax-e kw) '#:empty-namespace)
                                                   (set! empty-result-for-module->namespace? #t)
                                                   (set-box! encoded-root-expand-ctx-box #f)))
                                               #f]
                                              [else #f]))
                    #:get-module-linklet-info (lambda (mod-name phase)
                                                (define ht (and modules-being-compiled
                                                                (hash-ref modules-being-compiled mod-name #f)))
                                                (and ht (hash-ref ht phase #f)))
                    #:to-source? to-source?))
   
   (when with-submodules?
     ;; Record this module's linklets for cross-module inlining among (sub)modules
     ;; that are compiled together
     (hash-set! modules-being-compiled
                (module-path-index-resolve self) 
                (for/hasheq ([(phase linklet) (in-hash body-linklets)])
                  (values phase
                          (module-linklet-info linklet
                                               (hash-ref phase-to-link-module-uses phase #f)
                                               self)))))
   
   ;; Compile submodules; each list is (cons linklet-directory-key compiled-in-memory)
   (define post-submodules (compile-submodules #:star? #t
                                               #:bodys bodys
                                               #:with-submodules? with-submodules?
                                               #:serializable? serializable?
                                               #:to-source? to-source?
                                               #:cctx body-cctx
                                               #:modules-being-compiled modules-being-compiled))

   ;; Generate module-declaration info, which includes linking
   ;; information for each phase
   (define declaration-body
     `((define-values (self-mpi) ,(add-module-path-index! mpis self))
       (define-values (requires) ,(generate-deserialize requires mpis #:syntax-support? #f))
       (define-values (provides) ,(generate-deserialize provides mpis #:syntax-support? #f))
       (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)))

   ;; Assemble the declaration linking unit, which is instanted
   ;; once for a module declaration and shared among instances
   (define declaration-linklet
     ((if to-source? values (lambda (s) (performance-region
                                    ['compile 'module 'linklet]
                                    (compile-linklet s 'decl))))
      `(linklet
        ;; imports
        (,deserialize-imports
         [,mpi-vector-id])
        ;; exports
        (self-mpi
         requires
         provides
         phase-to-link-modules)
        ;; body
        ,@declaration-body)))
   
   ;; Assemble a linklet that shifts syntax objects on demand.
   ;; Include an encoding of the root expand context, if any, so that
   ;; `module->namespace` can have the same scopes as literal syntax
   ;; objects in the module.
   (define syntax-literals-linklet
     (and (not (syntax-literals-empty? syntax-literals))
          ((if to-source? values (lambda (s) (performance-region
                                         ['compile 'module 'linklet]
                                         (compile-linklet s 'syntax-literals))))
           `(linklet
             ;; imports
             (,deserialize-imports
              [,mpi-vector-id]
              [,deserialized-syntax-vector-id
               ,@(if serializable?
                     `(,deserialize-syntax-id)
                     '())]
              ,instance-imports)
             ;; exports
             (,get-syntax-literal!-id
              get-encoded-root-expand-ctx)
             ;; body
             ,@(generate-lazy-syntax-literals! syntax-literals mpis self
                                               #:skip-deserialize? (not serializable?))
             (define-values (get-encoded-root-expand-ctx)
               ,(cond
                 [root-ctx-pos
                  `(lambda ()
                    ,(generate-lazy-syntax-literal-lookup root-ctx-pos))]
                 [empty-result-for-module->namespace?
                  ;; We also attach this information directly to the bundle,
                  ;; in case this linklet is not included (due to an empty
                  ;; set of syntax literals)
                  `'empty]
                 [else
                  `'#f]))))))
   
   ;; Assemble a linklet that deserializes unshifted syntax objects on
   ;; demand. An instance of this linklet is shared for all
   ;; instantiations of the module, like the data linklet. It's
   ;; separate from the data linklet so that the data linklet can be
   ;; instantiated for information that just depends on module path
   ;; indexes, such as required modules.
   (define syntax-literals-data-linklet
     (and serializable?
          (not (syntax-literals-empty? syntax-literals))
          ((if to-source? values (lambda (s) (performance-region
                                         ['compile 'module 'linklet]
                                         (compile-linklet s 'syntax-literals-data))))
           `(linklet
             ;; imports
             (,deserialize-imports
              [,mpi-vector-id])
             ;; exports
             (,deserialized-syntax-vector-id
              ,deserialize-syntax-id)
             ;; body
             (define-values (,deserialized-syntax-vector-id)
               (make-vector ,(syntax-literals-count syntax-literals) #f))
             ,@(performance-region
                ['compile 'module 'serialize]
                (generate-lazy-syntax-literals-data! syntax-literals mpis))))))

   ;; The data linklet houses deserialized data for use by the
   ;; declaration and module-body linklets. Its instance is shared
   ;; across module instances.
   (define data-linklet
     (and serializable?
          ((if to-source? values (lambda (s) (performance-region
                                         ['compile 'module 'linklet]
                                         (compile-linklet s 'data))))
           `(linklet
             ;; imports
             (,deserialize-imports)
             ;; exports
             (,mpi-vector-id)
             ;; body
             (define-values (,inspector-id) (current-code-inspector))
             (define-values (,mpi-vector-id)
               ,(generate-module-path-index-deserialize mpis))))))
   
   ;; Combine linklets with other metadata as the bundle:
   (define bundle
     (let* ([bundle (hash-set body-linklets 'name full-module-name)]
            [bundle (hash-set bundle 'decl declaration-linklet)]
            [bundle (if data-linklet
                        (hash-set bundle 'data data-linklet)
                        bundle)]
            [bundle (if syntax-literals-linklet
                        (hash-set bundle 'stx syntax-literals-linklet)
                        bundle)]
            [bundle (if syntax-literals-data-linklet
                        (hash-set bundle 'stx-data syntax-literals-data-linklet)
                        bundle)]
            [bundle (if (null? pre-submodules)
                        bundle
                        (hash-set bundle 'pre (map car pre-submodules)))]
            [bundle (if (null? post-submodules)
                        bundle
                        (hash-set bundle 'post (map car post-submodules)))]
            [bundle (if cross-phase-persistent?
                        (hash-set bundle 'cross-phase-persistent? #t)
                        bundle)]
            [bundle (if language-info
                        (hash-set bundle 'language-info language-info)
                        bundle)]
            [bundle (if (zero? min-phase)
                        bundle
                        (hash-set bundle 'min-phase min-phase))]
            [bundle (if (zero? max-phase)
                        bundle
                        (hash-set bundle 'max-phase max-phase))]
            [bundle (if (hash-count side-effects)
                        (hash-set bundle 'side-effects (sort (hash-keys side-effects) <))
                        bundle)]
            [bundle (if empty-result-for-module->namespace?
                        (hash-set bundle 'module->namespace 'empty)
                        bundle)])
       (hash->linklet-bundle bundle)))

   ;; Combine with submodules in a linklet directory
   (define ld
     (cond
      [(and (null? pre-submodules)
            (null? post-submodules)
            (not as-submodule?))
       ;; Just use the bundle representation directly:
       bundle]
      [else
       ((if to-source? values hash->linklet-directory)
        (for/fold ([ht (hasheq #f bundle)]) ([sm (in-list (append pre-submodules post-submodules))])
          (hash-set ht
                    (car sm)
                    ((if to-source? values compiled-in-memory-linklet-directory)
                     (cdr sm)))))]))

   (cond
    [to-source? ld]
    [else
     ;; Save mpis and syntax for direct evaluation, instead of unmarshaling:
     (compiled-in-memory ld
                         phase-to-link-module-uses
                         (current-code-inspector)
                         phase-to-link-extra-inspectorsss
                         (mpis-as-vector mpis)
                         (syntax-literals-as-vector syntax-literals)
                         (map cdr pre-submodules)
                         (map cdr post-submodules)
                         #f     ; no namespace scopes
                         #f)]))) ; not purely functional, since it declares a module

;; ----------------------------------------

;; Walk though body to extract and compile submodules that are
;; declared with `form-name` (which is 'module or 'module*)
(define (compile-submodules #:star? star?
                            #:bodys bodys
                            #:with-submodules? with-submodules?
                            #:serializable? serializable?
                            #:to-source? to-source?
                            #:cctx body-cctx
                            #:modules-being-compiled modules-being-compiled)
  (cond
   [(not with-submodules?)
    null]
   [else
    (let loop ([bodys bodys]
               [phase 0])
      (cond
       [(null? bodys) null]
       [else
        (define body (car bodys))
        (cond
         [(and (parsed-module? body)
               (eq? (and star? #t) (and (parsed-module-star? body) #t)))
          (cons (cons (syntax-e (parsed-module-name-id body))
                      (compile-module body body-cctx
                                      #:as-submodule? #t
                                      #:serializable? serializable?
                                      #:to-source? to-source?
                                      #:modules-being-compiled modules-being-compiled))
                (loop (cdr bodys) phase))]
         [(parsed-begin-for-syntax? body)
          (append (loop (parsed-begin-for-syntax-body body) (add1 phase))
                  (loop (cdr bodys) phase))]
         [else
          (loop (cdr bodys) phase)])]))]))

;; ----------------------------------------

(define (filter-language-info li)
  (and (vector? li)
       (= 3 (vector-length li))
       (module-path? (vector-ref li 0))
       (symbol? (vector-ref li 1))
       li))
