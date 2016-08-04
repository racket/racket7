#lang racket/base
(require "../syntax/api.rkt"
         "main.rkt"
         "reflect.rkt"
         "../namespace/api.rkt")

(provide with-module-reading-parameterization
         raise-wrong-module-name
         check-module-form)

(define (with-module-reading-parameterization thunk)
  (call-with-default-reading-parameterization
   (lambda ()
     (parameterize ([read-accept-reader #t]
                    [read-accept-lang #t]
                    [read-accept-compiled #t])
       (thunk)))))

(define (raise-wrong-module-name filename expected-name name)
  (error 'load-handler
         "expected a `module' declaration for `~a' in ~s, found: ~a"
         expected-name filename name))

(define (check-module-form exp filename)
  (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
         (and filename
              (error 'load-handler
                     (string-append "expected a `module' declaration, but found end-of-file\n"
                                    "  file: ~a")
                     filename))]
        [(compiled-module-expression? (syntax-e exp))
         ;; It's fine:
         exp]
        [(and (syntax? exp)
              (pair? (syntax-e exp))
              (eq? 'module (syntax-e (car (syntax-e exp))))
              (let* ([r (cdr (syntax-e exp))]
                     [r (if (syntax? r) (syntax-e r) r)])
                (and (pair? r)
                     (identifier? (car r)))))
         ;; It's ok; need to install a specific `module' binding:
         (datum->syntax exp
                        (cons (namespace-module-identifier)
                              (cdr (syntax-e exp)))
                        exp
                        exp)]
        [else
         (and filename
              (error 'default-load-handler
                     (string-append "expected a `module' declaration, but found something else\n"
                                    "  file: ~a")
                     filename))]))
