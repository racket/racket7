#lang racket/base
(require (for-syntax racket/base)
         "wrap.rkt")

;; One more time, still yet another pattern matching library again...
(provide match

         define-match)

(define-syntax-rule (define-match match
                      unwrap unwrap-list
                      wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
                      wrap-eq? wrap-equal?
                      in-wrap-list)
  (...
   (begin
     (define-for-syntax (extract-pattern-variables pattern)
       (syntax-case pattern (unquote ?)
         [(unquote (? pred?))
          null]
         [(unquote bind-id)
          (if (free-identifier=? #'bind-id #'_)
              null
              (list #'bind-id))]
         [(p1 . p2) (append (extract-pattern-variables #'p1)
                            (extract-pattern-variables #'p2))]
         [else null]))

     (define-for-syntax (check-one id pattern)
       (syntax-case pattern (unquote ?)
         [(unquote (? pred?))
          #`(pred? #,id)]
         [(unquote bind-id) #`#t]
         [(pat ellipses)
          (and (identifier? #'ellipses)
               (free-identifier=? #'ellipses (quote-syntax ...)))
          (if (syntax-case #'pat (unquote)
                [(unquote bind-id) #t]
                [_ #f])
              #`(wrap-list? #,id)
              #`(and (wrap-list? #,id)
                     (for/and ([v (in-wrap-list #,id)])
                       #,(check-one #'v #'pat))))]
         [(p1 . p2)
          #`(let ([p (unwrap #,id)])
              (and (pair? p)
                   (let ([a (car p)])
                     #,(check-one #'a #'p1))
                   (let ([d (cdr p)])
                     #,(check-one #'d #'p2))))]
         [_
          #`(wrap-equal? (quote #,pattern) #,id)]))

     (define-for-syntax (extract-one id pattern)
       (syntax-case pattern (unquote ?)
         [(unquote (? pred?))
          #`(values)]
         [(unquote bind-id)
          (if (free-identifier=? #'bind-id #'_)
              #'(values)
              id)]
         [(pat ellipses)
          (and (identifier? #'ellipses)
               (free-identifier=? #'ellipses (quote-syntax ...)))
          (syntax-case #'pat (unquote)
            [(unquote bind-id)
             (if (free-identifier=? #'bind-id #'_)
                 #'(values)
                 #`(unwrap-list #,id))]
            [_
             (with-syntax ([pat-ids (extract-pattern-variables #'pat)])
               #`(for/lists pat-ids ([v (in-wrap-list #,id)])
                   #,(extract-one #'v #'pat)))])]
         [(p1 . p2)
          (let ([ids1 (extract-pattern-variables #'p1)]
                [ids2 (extract-pattern-variables #'p2)])
            (cond
              [(and (null? ids1) (null? ids2))
               #'(values)]
              [(null? ids1)
               #`(let ([d (cdr (unwrap #,id))])
                   #,(extract-one #'d #'p2))]
              [(null? ids2)
               #`(let ([a (car (unwrap #,id))])
                   #,(extract-one #'a #'p1))]
              [else
               #`(let ([p (unwrap #,id)])
                   (let-values ([#,ids1 (let ([a (car p)])
                                          #,(extract-one #'a #'p1))]
                                [#,ids2 (let ([d (cdr p)])
                                          #,(extract-one #'d #'p2))])
                     (values #,@ids1 #,@ids2)))]))]
         [_
          #'(values)]))

     (define-syntax (match stx)
       (syntax-case stx (quasiquote)
         [(_ expr [`pattern body0 body ...] ...)
          #`(let ([v expr])
              #,(let loop ([patterns (syntax->list #'(pattern ...))]
                           [bodys (syntax->list #'((body0 body ...) ...))])
                  (cond
                    [(null? patterns)
                     #'(error 'match "failed ~e" v)]
                    [else
                     (define ids (extract-pattern-variables (car patterns)))
                     #`(if #,(check-one #'v (car patterns))
                           (let-values ([#,ids #,(extract-one #'v (car patterns))])
                             . #,(car bodys))
                           #,(loop (cdr patterns) (cdr bodys)))])))])))))


(define-match match
  unwrap unwrap-list
  wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
  wrap-eq? wrap-equal?
  in-wrap-list)
