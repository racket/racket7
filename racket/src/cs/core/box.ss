(define (box-cas! b v1 v2)
  (unless (mutable-box? b)
    (raise-argument-error 'box-cas! "(and/c box? (not/c immutable?))" b))
  (unsafe-box-cas! b v1 v2))

(define (unsafe-box-cas! b v1 v2)
  (with-interrupts-disabled
   (and (eq? v1 (#3%unbox b))
        (#3%set-box! b v2)
        #t)))

;; ----------------------------------------

(define-record box-chaperone chaperone (ref set))
(define-record box-impersonator impersonator (ref set))

(define (box? v)
  (or (#%box? v)
      (and (impersonator? v)
           (#%box? (impersonator-val v)))))

(define (unbox b)
  (if (#%box? b)
      (#3%unbox b)
      (pariah (impersonate-unbox b))))

(define (unsafe-unbox b)
  (unbox b))

(define (set-box! b v)
  (if (#%box? b)
      (#3%set-box! b v)
      (pariah (impersonate-set-box! b v))))

(define (unsafe-set-box! b v)
  (set-box! b v))

(define (chaperone-box b ref set . props)
  (unless (box? b)
    (raise-argument-error 'chaperone-box "box?" b))
  (do-impersonate-box 'chaperone-box make-box-chaperone b ref set
                         make-props-chaperone props))

(define (impersonate-box b ref set . props)
  (unless (box? b)
    (raise-argument-error 'impersonate-box "box?" b))
  (do-impersonate-box 'impersonate-box make-box-impersonator b ref set
                      make-props-chaperone props))

(define (do-impersonate-box who make-box-impersonator b ref set
                            make-props-impersonator props)
  (unless (and (procedure? ref)
               (procedure-arity-includes? ref 2))
    (raise-argument-error who "(procedure-arity-includes/c 2)" ref))
  (unless (and (procedure? set)
               (procedure-arity-includes? set 2))
    (raise-argument-error who "(procedure-arity-includes/c 2)" set))
  (let ([val (if (impersonator? b)
                 (impersonator-val b)
                 b)]
        [props (add-impersonator-properties who
                                            props
                                            (if (impersonator? b)
                                                (impersonator-props b)
                                                empty-hasheq))])
    (make-box-impersonator val b props ref set)))

(define (impersonate-unbox orig)
  (if (and (impersonator? orig)
           (#%box? (impersonator-val orig)))
      (let loop ([o orig])
        (cond
         [(#%box? o) (#%unbox o)]
         [(box-chaperone? o)
          (let* ([val (loop (impersonator-next o))]
                 [new-val ((box-chaperone-ref o) o val)])
            (unless (chaperone-of? new-val val)
              (raise-arguments-error 'unbox
                                     "chaperone produced a result that is not a chaperone of the original result"
                                     "chaperone result" new-val
                                     "original result" val))
            new-val)]
         [(box-impersonator? o)
          (let ([val  (loop (impersonator-next o))])
            ((box-impersonator-ref o) o val))]
         [else (loop (impersonator-next o))]))
      ;; Let primitive report the error:
      (#2%unbox orig)))

(define (impersonate-set-box! orig val)
  (cond
   [(not (and (impersonator? orig)
              (mutable-box? (impersonator-val orig))))
    ;; Let primitive report the error:
    (#2%set-box! orig val)]
   [else
    (let loop ([o orig] [val val])
      (cond
       [(#%box? o) (#2%set-box! o val)]
       [else
        (let ([next (impersonator-next o)])
          (cond
           [(box-chaperone? o)
            (let ([new-val ((box-chaperone-set o) o val)])
              (unless (chaperone-of? new-val val)
                (raise-arguments-error 'set-box!
                                       "chaperone produced a result that is not a chaperone of the original result"
                                       "chaperone result" new-val
                                       "original result" val))
              (loop next val))]
           [(box-impersonator? o)
            (loop next ((box-impersonator-set o) o val))]
           [else (loop next val)]))]))]))

(define (set-box-impersonator-hash!)
  (record-type-hash-procedure (record-type-descriptor box-chaperone)
                              (lambda (c hash-code)
                                (hash-code (impersonator-next c))))
  (record-type-hash-procedure (record-type-descriptor box-impersonator)
                              (lambda (i hash-code)
                                (hash-code (box (unbox i))))))

;; ----------------------------------------

(define (make-weak-box v) (weak-cons v #t))

(define (weak-box? v) (weak-pair? v))

(define (weak-box-value v)
  (unless (weak-pair? v)
    (raise-argument-error 'weak-box-value "weak-box?" v))
  (let ([c (car v)])
    (if (eq? c #!bwp)
        #f
        c)))
