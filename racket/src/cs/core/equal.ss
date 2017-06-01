
;; Re-implement `equal?` to support impersonators and chaperones

(define (do-equal? orig-a orig-b stop-at? eql?)
  (let equal? ([orig-a orig-a] [orig-b orig-b] [ctx #f])
    (let loop ([a orig-a] [b orig-b])
      (or (eq? a b)
          (cond
           [(and (hash-impersonator? a)
                 (hash-impersonator? b))
            ;; For immutable hashes, it's ok for the two objects to not be eq,
            ;; as long as the interpositions are the same and the underlying
            ;; values are `{impersonator,chaperone}-of?`:
            (and (eq? (hash-impersonator-procs a)
                      (hash-impersonator-procs b))
                 (loop (impersonator-next a)
                       (impersonator-next b)))]
           [(and (hash-chaperone? a)
                 (hash-chaperone? b))
            ;; Same as above
            (and (eq? (hash-chaperone-procs a)
                      (hash-chaperone-procs b))
                 (loop (impersonator-next a)
                       (impersonator-next b)))]
           [(props-impersonator? b)
            (loop a (impersonator-next b))]
           [(props-chaperone? b)
            (loop a (impersonator-next b))]
           [(impersonator? a)
            (loop (impersonator-next a) b)]
           [(stop-at? b)
            #f]
           [(impersonator? b)
            (loop a (impersonator-next b))]
           [(#%vector? a)
            (and (#%vector? b)
                 (let ([len (#%vector-length a)])
                   (and (fx= len (#%vector-length b))
                        (or
                         (check-union-find ctx a b)
                         (let ([ctx (deeper-context ctx)])
                           (let loop ([i 0])
                             (or (fx= i len)
                                 (and (if eql?
                                          (eql? (vector-ref orig-a i)
                                                (vector-ref orig-b i))
                                          (equal? (vector-ref orig-a i)
                                                  (vector-ref orig-b i)
                                                  ctx))
                                      (loop (fx1+ i))))))))))]
           [(pair? a)
            (and (pair? b)
                 (or (check-union-find ctx a b)
                     (if eql?
                         (and (eql? (car a) (car b))
                              (eql? (cdr a) (cdr b)))
                         (let ([ctx (deeper-context ctx)])
                           (and
                            (equal? (car a) (car b) ctx)
                            (equal? (cdr a) (cdr b) ctx))))))]
           [(#%box? a)
            (and (#%box? b)
                 (or (check-union-find ctx a b)
                     (if eql?
                         (eql? (unbox orig-a) (unbox orig-b))
                         (let ([ctx (deeper-context ctx)])
                           (equal? (unbox orig-a) (unbox orig-b) ctx)))))]
           [(record? a)
            (and (record? b)
                 (let ([rec-equal? (record-equal-procedure a b)])
                   (and rec-equal?
                        (or (check-union-find ctx a b)
                            (if eql?
                                (rec-equal? orig-a orig-b eql?)
                                (let ([ctx (deeper-context ctx)])
                                  (rec-equal? orig-a orig-b
                                              (lambda (a b)
                                                (equal? a b ctx)))))))))]
           [else
            (#%equal? a b)])))))

(define (equal? a b) (do-equal? a b (lambda (x) #f) #f))
(define (impersonator-of? a b) (do-equal? a b impersonator? #f))
(define (chaperone-of? a b) (do-equal? a b chaperone? #f))

(define/who (equal?/recur a b eql?)
  (check who (procedure-arity-includes/c 2) eql?)
  (do-equal? a b (lambda (x) #f) eql?))

;; ----------------------------------------

;; Use a hash table to detect cycles and sharing,
;; but only start using it if a comparison goes
;; deep enough.

(define (deeper-context ctx)
  (cond
   [ctx
    (let ([v (#%unbox ctx)])
      (when (fixnum? v)
        (if (fx= v 0)
            (#%set-box! ctx (make-eq-hashtable))
            (#%set-box! ctx (fx1- v)))))
    ctx]
   [else (box 32)]))

(define (check-union-find ctx a b)
  (cond
   [(and ctx
         (hashtable? (#%unbox ctx)))
    (let ([ht (#%unbox ctx)])
      (let ([av (union-find ht a)]
            [bv (union-find ht b)])
        (or (eq? av bv)
            (begin
              (hashtable-set! ht av bv)
              #f))))]
   [else #f]))

(define (union-find ht a)
  (define av
    (let loop ([a a])
      (let ([next-a (hashtable-ref ht a #f)])
        (if next-a
            (loop next-a)
            a))))
  (unless (eq? av a)
    (let loop ([a a])
      (let ([next-a (hashtable-ref ht a #f)])
        (unless (eq? next-a av)
          (hashtable-set! ht a next-a)
          (loop next-a)))))
  av)
