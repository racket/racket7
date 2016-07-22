
(module stxparam '#%kernel
  (#%require "define.rkt"
             (for-syntax '#%kernel 
                         "stx.rkt" "stxcase-scheme.rkt" 
                         "small-scheme.rkt" 
                         "stxloc.rkt" "stxparamkey.rkt"))

  (#%provide (for-syntax do-syntax-parameterize)
             let-local-keys)

  (define-for-syntax (do-syntax-parameterize stx let-syntaxes-id empty-body-ok?)
    (syntax-case stx ()
      [(_ ([id val] ...) body ...)
       (let ([ids (syntax->list #'(id ...))])
	 (with-syntax ([((gen-id local-key must-be-renamer?) ...)
                    (map (lambda (id)
                           (unless (identifier? id)
                             (raise-syntax-error
                              #f
                              "not an identifier"
                              stx
                              id))
                           (let ([sp (syntax-parameter-local-value id)])
                             (unless (syntax-parameter? sp)
                               (raise-syntax-error
                                #f
                                "not bound as a syntax parameter"
                                stx
                                id))
                             (list
                              (car (generate-temporaries '(stx-param)))
                              (syntax-parameter-key sp)
                              (rename-transformer-parameter? sp))))
                         ids)])
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate binding"
		stx
		dup)))
           (unless empty-body-ok?
             (when (null? (syntax-e #'(body ...)))
               (raise-syntax-error
                #f
                "missing body expression(s)"
                stx)))
           (with-syntax ([let-syntaxes let-syntaxes-id])
             (syntax/loc stx
               (let-syntaxes ([(gen-id) (convert-renamer must-be-renamer? val)]
                              ...)
                 (let-local-keys ([local-key gen-id] ...)
                   body ...))))))]))
  
  (define-syntax (let-local-keys stx)
    (if (eq? 'expression (syntax-local-context))
        (let-values ([(expr opaque-expr)
                      (syntax-case stx ()
                        [(_ ([local-key id] ...) body ...)
                         (syntax-local-expand-expression/extend-environment
                          #'(let-values () body ...)
                          (syntax->datum #'(local-key ...))
                          (syntax->list #'(id ...)))])])
          opaque-expr)
        (with-syntax ([stx stx])
          #'(#%expression stx)))))
