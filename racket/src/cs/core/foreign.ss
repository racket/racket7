
(define (cpointer? v)
  (or (authentic-cpointer? v)
      (not v)
      (bytes? v)
      (has-cpointer-property? v)))

;; A cpointer's `addr` is either an address (i.e., a number),
;; a vector, or a byte string. A vector is used for non-atomic memory.
(define-record-type (cpointer make-cpointer authentic-cpointer?)
  (fields addr (mutable tags)))
(define-record-type cpointer+offset
  (parent cpointer)
  (fields (mutable offset)))

(define-values (prop:cpointer has-cpointer-property? cpointer-property-ref)
  (make-struct-type-property 'cpointer
                             (lambda (v info)
                               (cond
                                [(exact-nonnegative-integer? v)
                                 (unless (< v (list-ref info 1))
                                   (raise-arguments-error 'prop:cpointer
                                                          "index is out of range"
                                                          "index" v))
                                 (unless (chez:memv v (list-ref info 5))
                                   (raise-arguments-error 'prop:cpointer
                                                          "index does not refer to an immutable field"
                                                          "index" v))
                                 (+ v (let ([p (list-ref info 6)])
                                        (if p
                                            (struct-type-field-count p)
                                            0)))]
                                [(and (procedure? v)
                                      (procedure-arity-includes? v 1))
                                 v]
                                [(cpointer? v) v]
                                [else
                                 (raise-argument-error 'prop:cpointer
                                                       (string-append
                                                        "(or/c exact-nonnegative-integer?\n"
                                                        "      (procedure-arity-includes/c 1)\n"
                                                        "      cpointer?)")
                                                       v)]))))

(define (extract-cpointer p)
  (cond
   [(authentic-cpointer? p) p]
   [(not p) p]
   [(bytes? p) p]
   [else (let ([v (cpointer-property-ref p)])
           (cond
            [(exact-nonnegative-integer? v)
             (let ([v (unsafe-struct-ref p v)])
               (if (cpointer? v)
                   (extract-cpointer v)
                   #f))]
            [(procedure? v)
             (let ([p2 (v p)])
               (unless (cpointer? p2)
                 (raise-result-error 'prop:cpointer-accessor
                                     "cpointer?"
                                     p2))
               (extract-cpointer p2))]
            [else
             (extract-cpointer v)]))]))

(define (offset-ptr? p)
  (unless (cpointer? p)
    (raise-argument-error 'offset-ptr? "cpointer?" p))
  (cpointer+offset? p))

(define (set-cpointer-tag! p t)
  (unless (authentic-cpointer? p)
    (if (cpointer? p)
        (raise-arguments-error 'set-cpointer-tag!
                               "cannot set tag on given cpointer"
                               "given" p
                               "tag" t)
        (raise-argument-error 'set-cpointer-tag! "cpointer?" p)))
  (cpointer-tags-set! p t))

(define (cpointer-tag p)
  (unless (authentic-cpointer? p)
    (if (cpointer? p)
        (raise-arguments-error 'cpointer-tag
                               "cannot get tag from given cpointer"
                               "given" p)
        (raise-argument-error 'cpointer-tag "cpointer?" p)))
  (cpointer-tags p))

;; ----------------------------------------

;; Hack: use `s_fxmul` as an identity function
;; to corece a bytevector's start to an address:
(define bytevector->addr
  (foreign-procedure "(cs)fxmul"
    (u8* uptr)
    uptr))
(define object->addr
  (foreign-procedure "(cs)fxmul"
    (scheme-object uptr)
    uptr))
(define addr->object
  (foreign-procedure "(cs)fxmul"
    (uptr uptr)
    scheme-object))

(define vector-content-offset
  ;; Hack: we rely on the implementation detail of bytevectors and vectors
  ;; having the same offset from the address to the content.
  (let ([s (make-bytevector 1)])
    ;; Disable interrupts to avoid a GC:
    (with-interrupts-disabled
     (- (bytevector->addr s 1)
        (object->addr s 1)))))

(define addr-scratch (foreign-alloc (foreign-sizeof 'void*)))

(define (cpointer-address p)
  (cond
   [(not p) 0]
   [(bytes? p) (addr-address p)]
   [(cpointer+offset? p)
    (let ([addr (cpointer-addr p)])
      (+ (addr-address addr) (cpointer+offset-offset p)))]
   [(authentic-cpointer? p)
    (addr-address (cpointer-addr p))]
   [else
    (let ([p (extract-cpointer p)])
      (cpointer-address p))]))

(define (addr-address addr)
  (cond
   [(integer? addr) addr]
   [(bytes? addr) (bytevector->addr addr 1)]
   [else
    (+ (object->addr addr 1)
       vector-content-offset)]))

;; ----------------------------------------

(define (ptr-equal? p1 p2)
  (unless (cpointer? p1)
    (raise-argument-error 'ptr-equal? "cpointer?" p1))
  (unless (cpointer? p2)
    (raise-argument-error 'ptr-equal? "cpointer?" p2))
  (= (cpointer-address p1) (cpointer-address p2)))

(define (ptr-offset p)
  (unless (cpointer? p)
    (raise-argument-error 'ptr-offset "cpointer?" p))
  (if (cpointer+offset? p)
      (cpointer+offset-offset p)
      0))

(define (set-ptr-offset! p n)
  (unless (cpointer+offset? p)
    (raise-argument-error 'ptr-offset "(and/c cpointer? ptr-offset?)" p))
  (unless (exact-integer? n)
    (raise-argument-error 'ptr-offset "exact-integer?" n))
  (cpointer+offset-offset-set! p n))

(define ptr-add
  (case-lambda
   [(p n type)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-add "cpointer?" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add "exact-integer?" n))
    (unless (ctype? type)
      (raise-argument-error 'ptr-add "ctype?" type))
    (do-ptr-add p (* n (ctype-sizeof type)))]
   [(p n)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-add "cpointer?" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add "exact-integer?" n))
    (do-ptr-add p n)]))

(define (do-ptr-add p n)
  (cond
   [(authentic-cpointer? p)
    (make-cpointer+offset (cpointer-addr p) (cpointer-tag p) (+ n (if (cpointer+offset? p)
                                                                      (cpointer+offset-offset p)
                                                                      0)))]
   [(has-cpointer-property? p)
    (do-ptr-add (extract-cpointer p) n)]
   [else
    (make-cpointer+offset p #f n)]))

(define ptr-add!
  (case-lambda
   [(p n type)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (unless (ctype? type)
      (raise-argument-error 'ptr-add! "ctype?" type))
    (do-ptr-add! p (* n (ctype-sizeof type)))]
   [(p n)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (do-ptr-add! p n)]))

(define (do-ptr-add! p n)
  (unless (cpointer+offset? p)
    (raise-arguments-error 'ptr-add!
                           "given cpointer does not have an offset"
                           "given" p))
  (cpointer+offset-offset-set! p (+ n (cpointer+offset-offset p))))

;; ----------------------------------------

(define-record-type (ctype create-ctype ctype?)
  (fields s-exp root-basetype basetype scheme->c c->scheme))

(define (make-ctype type racket-to-c c-to-racket)
  (unless (ctype? type)
    (raise-argument-error 'make-ctype "ctype?" type))
  (create-ctype (ctype-s-exp type)
                (ctype-root-basetype type)
                type
                racket-to-c
                c-to-racket))

(define (s->c type v)
  (let* ([racket-to-c (ctype-scheme->c type)]
         [v (if racket-to-c
                (racket-to-c v)
                v)]
         [next (ctype-basetype type)])
    (if (symbol? next)
        v
        (s->c next v))))

(define (c->s type v)
  (let* ([next (ctype-basetype type)]
         [v (if (symbol? next)
                v
                (c->s next v))]
         [c-to-racket (ctype-c->scheme type)])
    (if c-to-racket
        (c-to-racket v)
        v)))

;; ----------------------------------------

(define-syntax define-ctype
  (syntax-rules ()
    [(_ id s-exp basetype)
     (define id (create-ctype s-exp basetype basetype #f #f))]
    [(_ id s-exp basetype s->c)
     (define id (create-ctype s-exp basetype basetype s->c #f))]
    [(_ id s-exp basetype s->c c->s)
     (define id (create-ctype s-exp basetype basetype s->c c->s))]))

(define-ctype _bool 'boolean 'bool)
(define-ctype _double 'double 'double)
(define-ctype _fixnum 'fixnum 'fixnum)
(define-ctype _float 'float 'float)
(define-ctype _int8 'integer-8 'int8)
(define-ctype _int16 'integer-16 'int16)
(define-ctype _int32 'integer-32 'int32)
(define-ctype _int64 'integer-64 'int64)
(define-ctype _uint8 'unsigned-8 'uint8)
(define-ctype _uint16 'unsigned-16 'uint16)
(define-ctype _uint32 'unsigned-32 'uint32)
(define-ctype _uint64 'unsigned-64 'uint64)
(define-ctype _scheme 'scheme-object 'scheme)
(define-ctype _string/ucs-4 (if (system-big-endian?) 'utf-32be 'utf-32le) 'string/ucs-4)
(define-ctype _string/utf-16 (if (system-big-endian?) 'utf-16be 'utf-16le) 'string/utf-16)
(define-ctype _void 'void 'void)

(define (bad-ctype-value type-name v)
  (raise-arguments-error 'apply
                         "bad value for conversion"
                         "ctype" type-name
                         "value" v))

(define-ctype _bytes 'void* 'bytes
  (lambda (x) x)
  (lambda (x) (let loop ([i 0])
                (if (fx= 0 (foreign-ref 'unsigned-8 x i))
                    (let ([bstr (make-bytes i)])
                      (let loop ([j 0])
                        (cond
                         [(= j i) bstr]
                         [else
                          (bytes-set! bstr j (foreign-ref 'unsigned-8 x j))
                          (loop (add1 j))])))
                    (loop (add1 i))))))

(define-ctype _short_bytes 'void* 'bytes
  (lambda (x) x)
  (lambda (x) (let loop ([i 0])
                (if (fx= 0 (foreign-ref 'unsigned-8 x i))
                    (let ([bstr (make-bytes i)])
                      (let loop ([j 0])
                        (cond
                         [(= j i) bstr]
                         [else
                          (bytes-set! bstr j (foreign-ref 'unsigned-8 x j))
                          (loop (add1 j))])))
                    (loop (+ i 2))))))

(define-ctype _double* 'double 'double
  (lambda (x) (if (real? x)
                  (exact->inexact x)
                  (bad-ctype-value '_double* x))))
(define-ctype _ufixnum 'fixnum 'fixnum
  (lambda (x) (if (and (fixnum? x) (fx> x 0))
                  x
                  (bad-ctype-value x '_ufixnum))))
(define-ctype _fixint 'integer 'fixint
  (lambda (x) (if (fixnum? x)
                  x
                  (bad-ctype-value x '_fixint))))
(define-ctype _ufixint 'unsigned 'ufixint
  (lambda (x) (if (fixnum? x)
                  x
                  (bad-ctype-value x '_ufixint))))

(define-ctype _symbol 'string 'string
  (lambda (x) (if (symbol? x)
                  (symbol->string x)
                  (bad-ctype-value '_symbol x)))
  (lambda (s) (string->symbol s)))

;; FIXME:
(define-ctype _path 'string 'path)

(define-ctype _longdouble 'double 'double
  (lambda (x) (bad-ctype-value '_longdouble x)))

(define-ctype _pointer 'void* 'pointer
  (lambda (v) v) ; resolved to an address with the GC disabled
  (lambda (x) (cond
               [(zero? x) #f]
               [else (make-cpointer x #f)])))

;; FIXME:
(define-ctype _fpointer 'void* 'fpointer)

(define-ctype _gcpointer 'void* 'gcpointer
  (lambda (v) (cond
               [(cpointer? v) (cpointer-addr v)]
               [(not v) 0]
               [(bytes? v) v]))
  (lambda (x) (cond
               [(zero? x) #f]
               [else (make-cpointer x #f)])))

;; FIXME:
(define-ctype _stdbool 'integer-8 'stdbool
  (lambda (x) (and x 0))
  (lambda (v) (not (zero? v))))

(define make-cstruct-type
  (case-lambda
   [(types) (make-cstruct-type types #f #f)]
   [(types abi) (make-cstruct-type types abi #f)]
   [(types abi alignment)
    (create-ctype 'struct
                  'struct
                  types
                  (lambda (s) (raise-unsupported-error 'struct-s->c))
                  (lambda (c) (raise-unsupported-error 'struct-c->s)))]))

(define (compiler-sizeof sl)
  (define (rest sl) (if (pair? sl) (cdr sl) '()))
  (unless (or (symbol? sl)
              (list? sl))
    (raise-argument-error 'compiler-sizeof
                          "(or/c ctype-symbol? (listof ctype-symbol?))"
                          sl))
  (let loop ([sl sl] [base-type #f] [star? #f] [size #f])
    (cond
     [(null? sl)
      (cond
       [(eq? base-type 'void)
        (when size
          (raise-arguments-error 'compiler-sizeof "cannot qualify 'void"))
        (if star?
            (foreign-sizeof 'void*)
            (raise-arguments-error 'compiler-sizeof "cannot use 'void without a '*"))]
       [(or (not base-type)
            (eq? base-type 'int))
        (if star?
            (foreign-sizeof 'void*)
            (foreign-sizeof (or size 'int)))]
       [(eq? base-type 'double)
        (case size
          [(long)
           (if star?
               (foreign-sizeof 'void*)
               ;; FIXME:
               (foreign-sizeof 'double))]
          [(#f)
           (if star?
               (foreign-sizeof 'void*)
               (foreign-sizeof 'double))]
          [else
           (raise-arguments-error 'compiler-sizeof "bad qualifiers for 'double")])]
       [(eq? base-type 'float)
        (case size
          [(#f)
           (if star?
               (foreign-sizeof 'void*)
               (foreign-sizeof 'float))]
          [else
           (raise-arguments-error 'compiler-sizeof "bad qualifiers for 'float")])]
       [size
        (raise-arguments-error 'compiler-sizeof (format "cannot qualify '~a" base-type))])]
     [else
      (let ([s (if (pair? sl) (car sl) sl)])
        (case s
          [(int char float double void)
           (cond
            [base-type
             (raise-arguments-error 'compiler-sizeof
                                    (format "extraneous type: '~a" s))]
            [else
             (loop (rest sl) s star? size)])]
          [(short)
           (case size
             [(short)
              (raise-arguments-error 'compiler-sizeof
                                     "cannot handle more than one 'short")]
             [(long)
              (raise-arguments-error 'compiler-sizeof
                                     "cannot use both 'short and 'long")]
             [(#f) (loop (rest sl) base-type star? 'short)])]
          [(long)
           (case size
             [(short)
              (raise-arguments-error 'compiler-sizeof
                                     "cannot use both 'short and 'long")]
             [(long-long)
              (raise-arguments-error 'compiler-sizeof
                                     "cannot handle more than two 'long")]
             [(long)
              (loop (rest sl) base-type star? 'long-long)]
             [(#f)
              (loop (rest sl) base-type star? 'long)])]
          [(*)
           (if star?
               (raise-arguments-error 'compiler-sizeof
                                      "cannot handle more than one '*")
               (loop (rest sl) base-type #t size))]
          [else
           (raise-argument-error 'compiler-sizeof
                                 "(or/c ctype-symbol? (listof ctype-symbol?))"
                                 sl)]))])))

(define (ctype-malloc-mode c)
  (let ([t (ctype-root-basetype c)])
    (if (or (eq? t 'gcpointer) (eq? t 'scheme))
        'nonatomic
        'atomic)))

(define (ctype-sizeof c)
  (unless (ctype? c)
    (raise-argument-error 'ctype-sizeof "ctype?" c))
  (case (ctype-s-exp c)
    [(void) 0]
    [(boolean int) 4]
    [(double) 8]
    [(float) 4]
    [(integer-8 unsigned-8) 1]
    [(integer-16 unsigned-16) 2]
    [(integer-32 unsigned-32) 4]
    [(integer-64 unsigned-64) 8]
    [(struct)
     (let ([base (ctype-basetype c)])
       (if (ctype? base)
           (ctype-sizeof base)
           (let ([align (lambda (size algn)
                          (let ([amt (modulo size algn)])
                            (if (zero? amt)
                                size
                                (+ size (- algn amt)))))])
             (let loop ([types base] [size 0] [max-align 1])
               (cond
                [(null? types) (align size max-align)]
                [else (let ([sz (ctype-sizeof (car types))]
                            [algn (ctype-alignof (car types))])
                        (loop (cdr types)
                              (+ (align size algn)
                                 sz)
                              (max algn max-align)))])))))]
    [else
     ;; Everything else is pointer-sized:
     (foreign-sizeof 'void*)]))

(define (ctype-alignof c)
  (unless (ctype? c)
    (raise-argument-error 'ctype-alignof "ctype?" c))
  (cond
   [(eq? 'struct (ctype-s-exp c))
    (let ([next (ctype-basetype c)])
      (if (ctype? next)
          (ctype-alignof next)
          (apply max (map ctype-alignof next))))]
   [else
    (ctype-sizeof c)]))

(define (cpointer-gcable? p)
  (unless (cpointer? p)
    (raise-argument-error 'cpointer-gcable? "cpointer?" p))
  ;; FIXME:
  #f)

(define (ffi-lib name . args)
  #f)

(define (ffi-lib? v)
  #f)

(define (ffi-obj sym lib)
  (raise
   (|#%app|
    exn:fail:filesystem
    (format "ffi-obj: counld't get ~a" sym)
    (current-continuation-marks))))

(define (ffi-obj? v)
  #f)

;; ----------------------------------------

(define ptr-ref
  (case-lambda
   [(p type)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-ref "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-ref "ctype?" type))
    (c->s type (foreign-ref* type p 0))]
   [(p type offset)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-ref "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-ref "ctype?" type))
    (unless (exact-integer? offset)
      (raise-argument-error 'ptr-ref "exact-integer?" offset))
    (c->s type (foreign-ref* type
                             p
                             (* (ctype-sizeof type) offset)))]
   [(p type abs-tag offset)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-ref "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-ref "ctype?" type))
    (unless (eq? abs-tag 'abs)
      (raise-argument-error 'ptr-ref "'abs" abs-tag))
    (unless (exact-integer? offset)
      (raise-argument-error 'ptr-ref "exact-integer?" offset))
    (c->s type (foreign-ref* type p offset))]))

(define (foreign-ref* type p offset)
  (let ([s-exp (ctype-s-exp type)]
        [p (extract-cpointer p)])
    ;; Disable interrupts to avoid a GC:
    (with-interrupts-disabled
     (let ([v (foreign-ref (if (eq? s-exp 'scheme-object)
                               'uptr
                               s-exp)
                           (cpointer-address p)
                           offset)])
       (if (eq? s-exp 'scheme-object)
           (addr->object v 1)
           v)))))

(define ptr-set!
  (case-lambda
   [(p type v)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-set! "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-set! "ctype?" type))
    (foreign-set!* type
                   p
                   0
                   v)]
   [(p type offset v)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-set! "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-set! "ctype?" type))
    (unless (exact-integer? offset)
      (raise-argument-error 'ptr-set! "exact-integer?" offset))
    (foreign-set!* type
                   p
                   (* (ctype-sizeof type) offset)
                   v)]
   [(p type abs-tag offset v)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-set! "cpointer?" p))
    (unless (ctype? type)
      (raise-argument-error 'ptr-set! "ctype?" type))
    (unless (eq? abs-tag 'abs)
      (raise-argument-error 'ptr-set! "'abs" abs-tag))
    (unless (exact-integer? offset)
      (raise-argument-error 'ptr-set! "exact-integer?" offset))
    (foreign-set!* type
                   p
                   offset
                   v)]))

(define (foreign-set!* type p offset v)
  (let ([v (s->c type v)]
        [s-exp (ctype-s-exp type)]
        [p (extract-cpointer p)])
    ;; Disable interrupts to avoid a GC:
    (with-interrupts-disabled
     (foreign-set! (if (eq? s-exp 'scheme-object)
                       'uptr
                       s-exp)
                   (cpointer-address p)
                   offset
                   (cond
                    [(eq? 'scheme-object s-exp) (object->addr v 1)]
                    [(eq? 'pointer s-exp) (cpointer-address v)]
                    [else v])))))

;; ----------------------------------------

;; With finalization through an ordered guardian,
;; a "late" weak hash table is just a hash table.
(define (make-late-weak-hasheq)
  (make-weak-hasheq))

;; Same for late weak boxes:
(define (make-late-weak-box b)
  (make-weak-box b))

(define malloc
  ;; Recognize common ordering as fast cases, and dispatch to
  ;; a general handler to arbtrary argument order
  (case-lambda
   [(arg1)
    (cond
     [(nonnegative-fixnum? arg1)
      (normalized-malloc arg1 'atomic)]
     [(ctype? arg1)
      (normalized-malloc (ctype-sizeof arg1) (ctype-malloc-mode arg1))]
     [else
      (do-malloc (list arg1))])]
   [(arg1 arg2)
    (cond
     [(and (nonnegative-fixnum? arg1)
           (ctype? arg2))
      (normalized-malloc (* arg1 (ctype-sizeof arg2)) (ctype-malloc-mode arg1))]
     [(and (ctype? arg1)
           (nonnegative-fixnum? arg2))
      (normalized-malloc (* arg2 (ctype-sizeof arg1)) (ctype-malloc-mode arg2))]
     [(and (nonnegative-fixnum? arg1)
           (malloc-mode? arg2))
      (normalized-malloc arg1 arg2)]
     [else
      (do-malloc (list arg1 arg2))])]
   [(arg1 arg2 arg3) (do-malloc (list arg1 arg2 arg3))]
   [(arg1 arg2 arg3 arg4) (do-malloc (list arg1 arg2 arg3 arg4))]
   [(arg1 arg2 arg3 arg4 arg5) (do-malloc (list arg1 arg2 arg3 arg4 arg5))]))

(define (do-malloc args)
  (define (duplicate-argument what a1 a2)
    (raise-arguments-error 'malloc
                           (string-append "mulitple " what " arguments")
                           "first" a1
                           "second" a2))
  (let loop ([args args] [count #f] [type #f] [copy-from #f] [mode #f] [fail-mode #f])
    (cond
     [(null? args)
      (let* ([len (* (or count 1) (if type (ctype-sizeof type) 1))]
             [p (normalized-malloc len
                                   (or mode (if type (ctype-malloc-mode type) 'atomic)))])
        (when copy-from
          (memcpy p copy-from len))
        p)]
     [(nonnegative-fixnum? (car args))
      (if count
          (duplicate-argument "size" count (car args))
          (loop (cdr args) (car args) type copy-from mode fail-mode))]
     [(ctype? (car args))
      (if type
          (duplicate-argument "type" type (car args))
          (loop (cdr args) count (car args) copy-from mode fail-mode))]
     [(and (cpointer? (car args)) (car args))
      (if copy-from
          (duplicate-argument "source for copy" copy-from (car args))
          (loop (cdr args) count type (car args) mode fail-mode))]
     [(malloc-mode? (car args))
      (if copy-from
          (duplicate-argument "mode" mode (car args))
          (loop (cdr args) count type copy-from (car args) fail-mode))]
     [(eq? (car args) 'failok)
      (if copy-from
          (duplicate-argument "failure mode" fail-mode (car args))
          (loop (cdr args) count type copy-from mode (car args)))]
     [else
      (raise-argument-error 'malloc
                            (string-append "(or/c (and/c exact-nonnegative-integer? fixnum?)\n"
                                           "      ctype? cpointer?\n"
                                           "      (or/c 'raw 'atomic 'nonatomic 'tagged\n"
                                           "            'atomic-interior 'interior\n"
                                           "            'stubborn 'uncollectable 'eternal)\n"
                                           "      'fail-ok)")
                            (car args))])))

(define (normalized-malloc size mode)
  (cond
   [(eq? mode 'raw)
    (make-cpointer (foreign-alloc size) #f)]
   [(eq? mode 'atomic)
    (make-cpointer (make-bytevector size) #f)]
   [(eq? mode 'atomic-interior)
    (let* ([p (foreign-alloc size)]
           [cp (make-cpointer p #f)])
      (free-guard cp p)
      cp)]
   [(eq? mode 'nonatomic)
    (make-cpointer (make-vector (quotient size 8)) #f)]
   [else
    (raise
     (|#%app|
      exn:fail:unsupported
      (format "malloc: '~a mode is not supported" mode)
      (current-continuation-marks)))]))

(define (free p)
  (foreign-free (cpointer-addr p)))

(define (malloc-immobile-cell v)
  (let ([vec (vector v)])
    (lock-object vec)
    (make-cpointer vec #f)))

(define (free-immobile-cell b)
  (unlock-object (cpointer-addr b)))

(define (malloc-mode? v)
  (chez:memq v '(raw atomic nonatomic tagged
                     atomic-interior interior
                     stubborn uncollectable eternal)))

(define free-guard (make-guardian))

(define (end-stubborn-change p)
  (raise-unsupported-error 'end-stubborn-change))

(define (extflvector->cpointer extfl-vector)
  (raise-unsupported-error 'extflvector->cpointer))

(define (vector->cpointer vec)
  (make-cpointer vec #f))

(define (flvector->cpointer flvec)
  (make-cpointer (flvector-bstr flvec) #f))

;; ----------------------------------------

(define-syntax define-foreign-not-yet-available
  (syntax-rules ()
    [(_ id)
     (define (id . args)
       (error 'id "foreign API not yet supported"))]
    [(_ id ...)
     (begin (define-foreign-not-yet-available id) ...)]))

(define-foreign-not-yet-available
  ffi-call
  ffi-callback
  ffi-callback?
  ffi-lib-name
  ffi-obj-lib
  ffi-obj-name
  lookup-errno
  make-array-type
  make-sized-byte-string
  make-union-type
  memcpy
  memmove
  memset
  saved-errno)

;; ----------------------------------------

(define process-global-table (make-hashtable equal-hash-code equal?))

;; FIXME: make atomic
(define (unsafe-register-process-global key val)
  (cond
   [(not val) (hashtable-ref process-global-table key #f)]
   [else
    (let ([old-val (hashtable-ref process-global-table key #f)])
      (cond
       [(not old-val)
        (hashtable-set! process-global-table key val)
        #f]
       [else old-val]))]))
