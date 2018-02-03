#lang racket/base
(require (only-in '#%linklet
                  primitive-table
                  primitive-in-category?)
         racket/cmdline
         "../../schemify/schemify.rkt"
         "../../schemify/serialize.rkt"
         "../../schemify/known.rkt"
         "../../schemify/lift.rkt"
         "../../cify/main.rkt"
         "help-startup.rkt")

(define dest "cstartup.inc")
(define version-line (format "/* version: ~a */" (version)))

(define-values (src vers deps)
  (command-line
   #:args (src-file vers-file . dep)
   (values src-file vers-file dep)))

(when (and (let ([dest-timestamp (file-or-directory-modify-seconds dest #f (lambda () -inf.0))])
             (for/and ([dep (cons src deps)])
               (dest-timestamp . > . (file-or-directory-modify-seconds dep #f (lambda () +inf.0)))))
           (call-with-input-file*
            dest
            (lambda (i)
              (equal? (read-line i) version-line))))
  ;; No need to rebuild
  (exit 0))

(define content (get-linklet src))
(define version-comparisons (get-version-comparisons vers))
                       
(define l (cdddr content))

(define (arity->mask a)
  (cond
   [(exact-nonnegative-integer? a)
    (arithmetic-shift 1 a)]
   [(arity-at-least? a)
    (bitwise-xor -1 (sub1 (arithmetic-shift 1 (arity-at-least-value a))))]
   [(list? a)
    (let loop ([mask 0] [l a])
      (cond
       [(null? l) mask]
       [else
        (let ([a (car l)])
          (cond
           [(or (exact-nonnegative-integer? a)
                (arity-at-least? a))
            (loop (bitwise-ior mask (arity->mask a)) (cdr l))]
           [else #f]))]))]
   [else #f]))

(define prim-knowns
  (for*/hash ([table-name '(#%linklet #%kernel
                                      #%paramz #%unsafe #%foreign
                                      #%futures #%place
                                      #%flfxnum #%extfl #%network)]
              [(name v) (in-hash (primitive-table table-name))])
    (values name
            (cond
              [(procedure? v)
               (define arity-mask (arity->mask (procedure-arity v)))
               (cond
                 [(primitive-in-category? name 'omitable)
                  (known-procedure/succeeds arity-mask)]
                 [else
                  (known-procedure arity-mask)])]
              [else
               a-known-constant]))))

(printf "Serializable...\n")
(define-values (bodys/constants-lifted lifted-constants)
  (time (convert-for-serialize l #t)))

(printf "Schemify...\n")
(define body
  (time
   (schemify-body bodys/constants-lifted (lambda (old-v new-v) new-v) prim-knowns #hasheq() #hasheq() #t)))

(printf "Lift...\n")
(define lifted-body
  (time
   (lift-in-schemified-body body (lambda (old new) new))))

(define converted-body
  (append (for/list ([p (in-list lifted-constants)])
            (cons 'define p))
          lifted-body))

(cify dest (caddr content) `(begin . ,converted-body) prim-knowns
      #:preamble (list version-line
                       (format "#if 0 ~a" version-comparisons)
                       "#include \"startup.inc\""
                       "#else"
                       "# include \"startup-glue.inc\"")
      #:postamble (list (format "#endif")))
