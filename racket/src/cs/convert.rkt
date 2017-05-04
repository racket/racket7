#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/match
         racket/file
         "schemify/schemify.rkt"
         "schemify/known.rkt"
         "known-primitive.rkt")

(define skip-export? #f)

(define-values (in-file out-file)
  (command-line
   #:once-each
   [("--skip-export") "Don't generate an `export` form"
    (set! skip-export? #t)]
   #:args
   (in-file out-file)
   (values in-file out-file)))

(define content (call-with-input-file* in-file read))
(define l (cdddr content))

(define lifts (make-hash))
(define ordered-lifts null)

(define (lift-set! k v)
  (unless (hash-ref lifts k #f)
    (hash-set! lifts k v)
    (set! ordered-lifts (cons k ordered-lifts))))

;; Ad hoc patterns to deal with a special case in "expander.rktl":
(define (quote? v)
  (and (pair? v)
       (eq? (car v) 'quote)
       (pair? (cdr v))
       (null? (cddr v))))
(define (nested-hash? v)
  (and (pair? v)
       (eq? #f (car v))
       (hash? (cdr v))))
(define (list-of-keywords? v)
  (and (pair? v)
       (list? v)
       (andmap keyword? v)))

;; Gather all literal regexps and hash tables
(define (lift v)
  (cond
   [(or (regexp? v) (byte-regexp? v))
    (define s (gensym 'rx))
    (lift-set! v s)]
   [(or (pregexp? v) (byte-pregexp? v))
    (define s (gensym 'px))
    (lift-set! v s)]
   [(hash? v)
    (define s (gensym 'hash))
    (lift-set! v s)]
   [(and (quote? v)
         (nested-hash? (cadr v)))
    (define s (gensym 'nhash))
    (lift-set! (cadr v) s)]
   [(keyword? v)
    (define s (gensym 'kw))
    (lift-set! v s)]
   [(and (quote? v)
         (list-of-keywords? (cadr v)))
    (define s (gensym 'kws))
    (lift-set! (cadr v) s)]
   [(pair? v)
    (lift (car v))
    (lift (cdr v))]))

(lift l)

(define prim-known-procs
  ;; Register primitives:
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/unsafe/ops)
      (namespace-require 'racket/flonum)
      (namespace-require 'racket/fixnum))
    (for/fold ([prim-knowns (hasheq)]) ([s (in-list (namespace-mapped-symbols ns))])
      (with-handlers ([exn:fail? (lambda (exn) prim-knowns)])
        (cond
         [(procedure? (eval s ns))
          (hash-set prim-knowns s a-known-procedure)]
         [else
          (hash-set prim-knowns s a-known-constant)])))))

(define prim-knowns
  (let* ([prim-knowns (for/fold ([prim-knowns (hasheq)]) ([s (in-list known-procedures)])
                        (hash-set prim-knowns s a-known-procedure))]
         [prim-knowns (for/fold ([prim-knowns prim-knowns]) ([s+c (in-list known-constructors)])
                        (hash-set prim-knowns (car s+c) (known-constructor (car s+c) (cadr s+c))))]
         [prim-knowns (for/fold ([prim-knowns prim-knowns]) ([s (in-list known-struct-type-property/immediate-guards)])
                        (hash-set prim-knowns s a-known-struct-type-property/immediate-guard))]
         [prim-knowns (for/fold ([prim-knowns prim-knowns]) ([s (in-list known-constants)])
                        (hash-set prim-knowns s a-known-constant))])
    prim-knowns))

;; Convert:
(define schemified-body
  (schemify-body l (lambda (old-v new-v) new-v) values prim-knowns #hasheq() #hasheq()))

;; ----------------------------------------

;; Set a hook to redirect literal regexps and
;; hash tables to lifted bindings
(pretty-print-size-hook
 (lambda (v display? out)
   (cond
    [(and (pair? v)
          (pair? (cdr v))
          (eq? 'quote (car v))
          (or (regexp? (cadr v))
              (byte-regexp? (cadr v))
              (pregexp? (cadr v))
              (byte-pregexp? (cadr v))
              (hash? (cadr v))
              (nested-hash? (cadr v))
              (keyword? (cadr v))
              (list-of-keywords? (cadr v))))
     10]
    [(bytes? v) (* 3 (bytes-length v))]
    [(and (symbol? v) (regexp-match? #rx"#" (symbol->string v)))
     (+ 2 (string-length (symbol->string v)))]
    [(char? v) 5]
    [(or (keyword? v)
         (regexp? v)
         (pregexp? v)
         (hash? v))
     (error 'lift "value that needs lifting is in an unrecognized context: ~v" v)]
    [else #f])))

;; This hook goes with `pretty-print-size-hook`
(pretty-print-print-hook
 (lambda (v display? out)
   (cond
    [(and (pair? v)
          (eq? 'quote (car v))
          (or (regexp? (cadr v))
              (byte-regexp? (cadr v))
              (pregexp? (cadr v))
              (byte-pregexp? (cadr v))
              (hash? (cadr v))
              (nested-hash? (cadr v))
              (keyword? (cadr v))
              (list-of-keywords? (cadr v))))
     (write (hash-ref lifts (cadr v)) out)]
    [(bytes? v)
     (display "#vu8")
     (write (bytes->list v) out)]
    [(symbol? v)
     (write-string (format "|~a|" v) out)]
    [(char? v)
     (write-string (format "#\\x~x" (char->integer v)) out)]
    [else #f])))

;; ----------------------------------------

(make-parent-directory* out-file)

(with-handlers ([void (lambda (exn)
                        (when (file-exists? out-file)
                          (with-handlers ([void (lambda (exn)
                                                  (log-error "delete failed: ~s" exn))])
                            (delete-file out-file)))
                        (raise exn))])
  (with-output-to-file
   out-file
   #:exists 'truncate
   (lambda ()
     (unless skip-export?
       ;; Write out exports
       (pretty-write
        `(export (rename ,@(caddr content)))))
     ;; Write out lifted regexp and hash-table literals
     (for ([k (in-list (reverse ordered-lifts))])
       (define v (hash-ref lifts k))
       (pretty-write
        `(define ,v
          ,(let loop ([k k])
             (cond
              [(or (regexp? k)
                   (byte-regexp? k))
               `(,(cond [(byte-regexp? k)  'byte-regexp]
                        [(byte-pregexp? k) 'byte-pregexp]
                        [(pregexp? k)      'pregexp]
                        [else              'regexp])
                 ,(object-name k))]
              [(hash? k)
               `(,(cond
                   [(hash-equal? k) 'hash]
                   [(hash-eqv? k) 'hasheqv]
                   [else 'hasheq])
                 ,@(for*/list ([(k v) (in-hash k)]
                               [e (in-list (list k v))])
                     `(quote ,e)))]
              [(pair? k)
               `(cons ,(loop (car k)) ,(loop (cdr k)))]
              [(keyword? k)
               `(string->keyword ,(keyword->string k))]
              [(null? k) ''()]
              [else k])))))

     ;; Write out converted forms
     (for ([v (in-list schemified-body)])
       (unless (equal? v '(void))
         (let loop ([v v])
           (match v
             [`(begin ,vs ...)
              (for-each loop vs)]
             [else
              (pretty-write v)])))))))
