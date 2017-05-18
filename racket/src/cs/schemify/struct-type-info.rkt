#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "mutated-state.rkt"
         "simple.rkt")

(provide (struct-out struct-type-info)
         struct-type-info-rest-properties-list-pos
         make-struct-type-info
         pure-properties-list?)

(struct struct-type-info (name parent immediate-field-count field-count pure-constructor? authentic? rest))
(define struct-type-info-rest-properties-list-pos 0)

;; Parse `make-struct-type` forms, returning a `struct-type-info`
;; if the parse succeed:
(define (make-struct-type-info v prim-knowns knowns imports mutated)
  (match v
    [`(make-struct-type (quote ,name) ,parent ,fields 0 #f . ,rest)
     ;; Note: auto-field count must be zero, because a non-zero count involves
     ;; an arity-reduced procedure
     (let ([u-name (unwrap name)]
           [u-parent (unwrap parent)])
       (and (symbol? u-name)
            (or (not u-parent)
                (known-struct-type? (hash-ref prim-knowns u-parent #f))
                (and (known-struct-type? (hash-ref-either knowns imports u-parent))
                     (simple-mutated-state? (hash-ref mutated u-parent #f))))
            (exact-nonnegative-integer? fields)
            ;; The inspector argument cannot be 'insp:
            (match rest
              [`() #t]
              [`(,_) #t]
              [`(,_ #f . ,_) #t]
              [`(,_ (current-inspector) . ,_) #t]
              [`,_ #f])
            (struct-type-info name
                              parent
                              fields
                              (+ fields (if parent
                                            (known-struct-type-field-count
                                             (hash-ref-either knowns imports u-parent))
                                            0))
                              ;; no guard => pure constructor
                              (or ((length rest) . < . 4)
                                  (not (list-ref rest 3)))
                              ;; look for `prop:authentic`
                              (and (pair? rest)
                                   (match (car rest)
                                     [`(list (cons ,props ,vals) ...)
                                      (for/or ([prop (in-list props)])
                                        (eq? (unwrap prop) 'prop:authentic))]
                                     [`,_ #f]))
                              rest)))]
    [`(let-values () ,body)
     (make-struct-type-info body prim-knowns knowns imports mutated)]
    [`,_ #f]))

;; Check whether `e` has the shape of a property list that uses only
;; properties where the property doesn't have a guard or won't invoke
;; a guarded procedure
(define (pure-properties-list? e prim-knowns knowns imports mutated)
  (match e
    [`(list (cons ,props ,vals) ...)
     (for/and ([prop (in-list props)]
               [val (in-list vals)])
       (let ([u-prop (unwrap prop)])
         (and (symbol? u-prop)
              (or (known-struct-type-property/immediate-guard? (hash-ref prim-knowns u-prop #f))
                  (known-struct-type-property/immediate-guard? (hash-ref-either knowns imports u-prop)))
              (simple? val prim-knowns knowns imports mutated))))]
    [`null #t]
    [`'() #t]
    [`,_ #f]))
