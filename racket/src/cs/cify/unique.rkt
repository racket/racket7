#lang racket/base
(require "match.rkt")

;; Some passes reuse identiers, breaking the original input's property
;; of having a unique symbol for every binding. The `re-unique` function
;; recovers that property.

(provide re-unique)

(define (re-unique e)
  (define all-ids (make-hasheq))

  ;; Give precedence to top-level names so they
  ;; don't change:
  (define (get-top-names e)
    (match e
      [`(define ,id ,rhs)
       (hash-set! all-ids id id)]
      [`(define-values ,ids ,rhs)
       (for ([id (in-list ids)])
         (hash-set! all-ids id id))]
      [`(begin . ,es)
       (for ([e (in-list es)])
         (get-top-names e))]
      [`,_ (void)]))

  (define (select-unique ids)
    (cond
      [(null? ids) '()]
      [(symbol? ids)
       (cond
         [(hash-ref all-ids ids #f)
          (define new-id (gensym ids))
          (hash-set! all-ids ids new-id)
          new-id]
         [else
          (hash-set! all-ids ids ids)
          ids])]
      [else
       (cons (select-unique (car ids))
             (select-unique (cdr ids)))]))

  (define (re-unique e)
    (match e
      [`(define ,id ,rhs)
       `(define ,id ,(re-unique rhs))]
      [`(define-values ,ids ,rhs)
       `(define-values ,ids ,(re-unique rhs))]
      [`(begin . ,body)
       `(begin . ,(re-unique-body body))]
      [`(begin0 . ,body)
       `(begin0 . ,(re-unique-body body))]
      [`(lambda ,ids . ,body)
       (define new-ids (select-unique ids))
       `(lambda ,new-ids . ,(re-unique-body body))]
      [`(case-lambda [,idss . ,bodys] ...)
       `(case-lambda
         ,@(for/list ([ids (in-list idss)]
                      [body (in-list bodys)])
             (define new-ids (select-unique ids))
             `[,new-ids . ,(re-unique-body body)]))]
      [`(if ,tst ,thn ,els)
       `(if ,(re-unique tst) ,(re-unique thn) ,(re-unique els))]
      [`(with-continuation-mark ,key ,val ,body)
       `(with-continuation-mark ,(re-unique key) ,(re-unique val) ,(re-unique body))]
      [`(let . ,_) (re-unique-let e)]
      [`(letrec . ,_) (re-unique-let e)]
      [`(letrec* . ,_) (re-unique-let e)]
      [`(set! ,id ,rhs)
       `(set! ,(re-unique id) ,(re-unique rhs))]
      [`(,rator ,rands ...)
       (re-unique-body e)]
      [`,_
       (cond
         [(symbol? e)
          (hash-ref all-ids e e)]
         [else e])]))

  (define (re-unique-body body)
    (for/list ([e (in-list body)])
      (re-unique e)))
  
  (define (re-unique-let e)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define rec? (not (eq? let-id 'let)))
       (define new-rhss (if rec?
                            rhss
                            (for/list ([rhs (in-list rhss)])
                              (re-unique rhs))))
       (define new-ids (select-unique ids))
       `(,let-id ,(for/list ([id (in-list new-ids)]
                             [rhs (in-list new-rhss)])
                    `[,id ,(if rec? (re-unique rhs) rhs)])
                 . ,(re-unique-body body))]))

  (get-top-names e)
  (re-unique e))
