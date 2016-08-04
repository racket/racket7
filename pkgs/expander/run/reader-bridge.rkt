#lang racket/base
(require (prefix-in new: "../syntax/syntax.rkt")
         (prefix-in new: "../syntax/scope.rkt")
         "../host/syntax-to-reader-syntax.rkt"
         "../host/reader-syntax-to-syntax.rkt"
         "host-syntax-to-syntax.rkt")

(provide synthesize-reader-bridge-module)

(define orig-eval (current-eval))
(define orig-compile (current-compile))
(define orig-resolver (current-module-name-resolver))

;; Given a reader for the hosted module system and syntax,
;; declare a module in the host module system that provides
;; a reader for the host's syntax system
(define (synthesize-reader-bridge-module mod-path synth-mod-path reader)
  (define name (module-path-index-resolve (module-path-index-join synth-mod-path #f)))
  (unless (module-declared? name)
    (define (root-of n) (if (pair? n) (car n) n))
    (parameterize ([current-module-declare-name (make-resolved-module-path
                                                 (root-of (resolved-module-path-name name)))]
                   [current-eval orig-eval]
                   [current-compile orig-compile]
                   [current-module-name-resolver orig-resolver])
      (eval `(,(namespace-module-identifier) mod '#%kernel
              (define-values (read-syntax)
                ,(if (procedure-arity-includes? reader 6)
                     (lambda (name in modname line col pos)
                       (syntax->reader-syntax (reader name in (host-syntax->syntax modname) line col pos)))
                     (lambda (name in)
                       (syntax->reader-syntax (reader name in)))))
              (#%provide read-syntax))))))
