#lang racket/base
(require "config.rkt"
         "readtable-parameter.rkt")

(provide readtable-delimiter-ht
         make-readtable
         current-readtable
         effective-char
         readtable-handler
         readtable-dispatch-handler
         readtable-apply)

(struct readtable (symbol-parser ; parser for default token handling: symbol-or-number
                   ;; The character table maps characters to either a
                   ;; parsing function or another character whose
                   ;; default to use
                   char-ht
                   ;; The dispatch table maps character for `#` dispatch
                   dispatch-ht
                   ;; The delimter table maps a character to 'delimit,
                   ;; 'no-delimit, or a character whose default to use;
                   ;; absence of a mapping is the default for that character
                   delimiter-ht)
        #:property prop:readtable #t)

(define (make-readtable rt . args)
  (unless (or (not rt) (readtable? rt))
    (raise-argument-error 'make-readtable "(or/c readtable? #f)" rt))
  (let loop ([args args]
             [symbol-parser (and rt (readtable-symbol-parser rt))]
             [char-ht (if rt (readtable-char-ht rt) #hasheqv())]
             [dispatch-ht (if rt (readtable-dispatch-ht rt) #hasheqv())]
             [delimiter-ht (if rt (readtable-delimiter-ht rt) #hasheqv())])
    (cond
     [(null? args) (readtable symbol-parser char-ht dispatch-ht delimiter-ht)]
     [else
      ;; Key is a character or #f
      (define key (car args))
      (unless (or (not key) (char? key))
        (raise-argument-error 'make-readtable "(or/c char? #f)" key))
      
      ;; Mode determines how the key is mapped
      (when (null? args)
        (cond
         [key (raise-arguments-error 'make-readtable
                                     (string-append "expected 'terminating-macro, 'non-terminating-macro, 'dispatch-macro,"
                                                    " or character argument after character argument")
                                     "character" key)]
         [else (raise-arguments-error 'make-readtable
                                      "expected 'non-terminating-macro after #f")]))
      (define mode (cadr args))
      (cond
       [key
        (unless (or (eq? mode 'terminating-macro)
                    (eq? mode 'non-terminating-macro)
                    (eq? mode 'dispatch-macro)
                    (char? mode))
          (raise-argument-error 'make-readtable
                                "(or/c 'terminating-macro 'non-terminating-macro 'dispatch-macro char?)"
                                mode))]
       [else
        (unless (eq? mode ''non-terminating-macro)
          (raise-arguments-error 'make-readtable
                                 "expected 'non-terminating-macro after #f"))])
      
      ;; Target is what the key is mapped to
      (when (null? (cddr args))
        (raise-arguments-error 'make-readtable
                               (if key
                                   "expected readtable or #f argument after character argument"
                                   "expected procedure argument after symbol argument")
                               "given" mode))
      (define target (caddr args))
      
      ;; Update the readtable
      (define rest-args (cdddr args))
      (cond
       [(not key)
        ;; Update symbol parser
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (loop rest-args target char-ht dispatch-ht delimiter-ht)]
       [(eq? mode 'dispatch-macro)
        ;; Update `#`-triggered dispatch table
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (loop rest-args symbol-parser char-ht (hash-set dispatch-ht key target) delimiter-ht)]
       [(char? mode)
        ;; Update main character table with a character alias
        (unless (or (not target) (readtable? target))
          (raise-argument-error 'make-readtable "(or/c readtable? #f)" target))
        (define actual-target (or (and target (hash-ref (readtable-char-ht target) mode #f))
                                  mode))
        (define new-char-ht (if actual-target
                                (hash-set char-ht key actual-target)
                                (hash-remove char-ht key)))
        (define new-delimiter-ht (hash-set delimiter-ht
                                           key
                                           (if target
                                               (hash-ref (readtable-delimiter-ht target) mode mode)
                                               mode)))
        (loop rest-args symbol-parser new-char-ht dispatch-ht new-delimiter-ht)]
       [else
        ;; Update main character table with a new handler
        (unless (and (procedure? target) (procedure-arity-includes? target 6))
          (raise-argument-error 'make-readtable "(procedure-arity-includes/c 6)" target))
        (define new-char-ht (hash-set char-ht key target))
        (define new-delimiter-ht (hash-set delimiter-ht key (if (eq? mode 'terminating-macro)
                                                                'delimit
                                                                'no-delimit)))
        (loop rest-args symbol-parser new-char-ht dispatch-ht new-delimiter-ht)])])))

;; Map a character to another character (if any) whose default
;; treatment should be used; be sure to map non-characters like
;; EOF to themselves
(define (effective-char c config)
  (cond
   [(not (char? c)) c]
   [else
    (define rt (read-config-readtable config))
    (cond
     [(not rt) c]
     [else
      (define target (hash-ref (readtable-char-ht rt) c #f))
      (if (char? target)
          target
          c)])]))

;; Map a character to a handler, if any:
(define (readtable-handler config c)
  (define rt (read-config-readtable config))
  (and rt
       (let ([target (hash-ref (readtable-char-ht rt) c #f)])
         (and target
              (not (char? target))
              target))))

;; Map a character after `#` to a handler, if any:
(define (readtable-dispatch-handler config c)
  (define rt (read-config-readtable config))
  (and rt
       (hash-ref (readtable-dispatch-ht rt) c #f)))

(define (readtable-apply handler c in config line col pos)
  (cond
   [(not (read-config-for-syntax? config))
    (if (procedure-arity-includes? handler 2)
        (handler c in)
        (handler c in #f #f #f #f))]
   [else
    (handler c in (read-config-source config) line col pos)]))