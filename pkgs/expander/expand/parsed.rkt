#lang racket/base

(provide (all-defined-out))

;; A fully expanded form can be parsed into an AST. In principle,
;; parsing could be a pass separate from the expander. As an important
;; shortcut, however, we fuse the expander and parser; the
;; `to-parsed?` field in an `expand-context` indicates whether the
;; expander should produce a syntax object or a `parsed` structure.

(struct parsed (s) #:transparent)

(struct parsed-id parsed (binding))
(struct parsed-top-id parsed-id ())

(struct parsed-lambda parsed (keys body))
(struct parsed-case-lambda parsed (clauses))
(struct parsed-app parsed (rator+rands))
(struct parsed-if parsed (tst thn els))
(struct parsed-set! parsed (id rhs))
(struct parsed-with-continuation-mark parsed (key val body))
(struct parsed-#%variable-reference parsed (id))
(struct parsed-begin parsed (body))
(struct parsed-begin0 parsed (body))
(struct parsed-quote parsed (datum))
(struct parsed-quote-syntax parsed (datum))

(struct parsed-let_-values parsed (idss clauses body))
(struct parsed-let-values parsed-let_-values ())
(struct parsed-letrec-values parsed-let_-values ())

(struct parsed-define-values parsed (ids syms rhs))
(struct parsed-define-syntaxes parsed (ids syms rhs))
(struct parsed-begin-for-syntax parsed (body))

(struct parsed-#%declare parsed ())
(struct parsed-require parsed ())

(struct parsed-#%module-begin parsed (body))
(struct parsed-module parsed (star?
                              name-id
                              self
                              requires
                              provides
                              root-ctx-simple?
                              encoded-root-ctx
                              body))
