#lang racket/base
(require "../common/list-tilde.rkt"
         "../syntax/syntax.rkt"
         "../syntax/binding.rkt")

(provide free-id-set
         empty-free-id-set
         free-id-set-member?
         free-id-set-empty-or-just-module*?)

;; A free-id-set is a hash: sym -> list of id

(define (free-id-set phase ids)
  (for/fold ([ht #hasheq()]) ([id (in-list ids)])
    (define sym (identifier-binding-symbol id phase))
    (hash-set ht sym (cons~ id (hash-ref ht sym null)))))

(define empty-free-id-set (free-id-set 0 null))

(define (free-id-set-member? fs phase given-id)
  (if (zero? (hash-count fs))
      #f
      (for/or ([id (in-list~ (hash-ref fs
                                       (identifier-binding-symbol given-id phase)
                                       null))])
        (free-identifier=? id given-id phase phase))))

(define (free-id-set-empty-or-just-module*? fs)
  (define c (hash-count fs))
  ;; If any identifier other than `module*` is present, then many
  ;; identifiers are present
  (c . <= . 1))
