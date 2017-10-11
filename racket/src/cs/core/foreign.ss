
(define (cpointer? v)
  (or (authentic-cpointer? v)
      (not v)
      (bytes? v)
      (has-cpointer-property? v)))

;; A cpointer's `memory` is either a raw foreign address (i.e., a
;; number), a vector, or a byte string. A bytevector is used
;; for GCable atomic memory, and a vector is used for GCable
;; non-atomic memory.
(define-record-type (cpointer make-cpointer authentic-cpointer?)
  (fields memory (mutable tags)))
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
                                            (struct-type-total*-field-count p)
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

;; Gets a primitive cpointer type by following a `prop:evt` property
;; as needed. Call with function *before* disabling GC interrupts.
(define (unwrap-cpointer p)
  (cond
   [(authentic-cpointer? p) p]
   [(not p) p]
   [(bytes? p) p]
   [(ffi-callback? p) p]
   [else (let ([v (cpointer-property-ref p)])
           (cond
            [(exact-nonnegative-integer? v)
             (let ([v (unsafe-struct-ref p v)])
               (if (cpointer? v)
                   (unwrap-cpointer v)
                   #f))]
            [(procedure? v)
             (let ([p2 (v p)])
               (unless (cpointer? p2)
                 (raise-result-error 'prop:cpointer-accessor
                                     "cpointer?"
                                     p2))
               (unwrap-cpointer p2))]
            [else
             (unwrap-cpointer v)]))]))

;; Like `unwrap-cpointer*`, but also allows an integer as a raw
;; foreign address:
(define (unwrap-cpointer* p)
  (if (integer? p)
      p
      (unwrap-cpointer p)))

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

;; Convert a `memory` --- typically a raw foreign address, but possibly
;; a byte string or vector --- to a cpointer, using #f for a NULL
;; address:
(define (memory->cpointer x)
  (cond
   [(eqv? x 0) #f]
   [else (make-cpointer x #f)]))

;; ----------------------------------------

;; Hack: use `s_fxmul` as an identity function
;; to corece a bytevector's start to an address
(define bytevector->addr ; call with GC disabled
  (foreign-procedure "(cs)fxmul"
    (u8* uptr)
    uptr))
(define object->addr ; call with GC disabled
  (foreign-procedure "(cs)fxmul"
    (scheme-object uptr)
    uptr))
(define address->object ; call with GC disabled
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

;; Converts a primitive cpointer (normally the result of
;; `unwrap-cpointer`) to a raw foreign address. The
;; GC must be disabled while extracting an address,
;; which might be the address of a byte string that
;; could otherwise change due to a GC.
(define (cpointer-address p) ; call with GC disabled
  (cond
   [(not p) 0]
   [(bytes? p) (memory-address p)]
   [(cpointer+offset? p)
    (let ([memory (cpointer-memory p)])
      (+ (memory-address memory) (cpointer+offset-offset p)))]
   [(authentic-cpointer? p)
    (memory-address (cpointer-memory p))]
   [(ffi-callback? p)
    (foreign-callable-entry-point (callback-code p))]
   [else
    (error 'internal-error "bad case extracting a cpointer address")]))

;; Like `cpointer-address`, but allows a raw foreign
;; address to pass through:
(define (cpointer*-address p) ; call with GC disabled
  (if (number? p)
      p
      (cpointer-address p)))

;; Convert a `memory` (as in a cpointer) to a raw foreign address.
(define (memory-address memory) ; call with GC disabled
  (cond
   [(integer? memory) memory]
   [(bytes? memory) (bytevector->addr memory 1)]
   [else
    (+ (object->addr memory 1)
       vector-content-offset)]))

;; Convert a raw foreign address to a Scheme value on the
;; assumption that the address is the payload of a byte
;; string or vector:
(define (addr->gcpointer-memory v)  ; call with GC disabled
  (address->object (- v vector-content-offset) 1))

;; ----------------------------------------

(define (ptr-equal? p1 p2)
  (unless (cpointer? p1)
    (raise-argument-error 'ptr-equal? "cpointer?" p1))
  (unless (cpointer? p2)
    (raise-argument-error 'ptr-equal? "cpointer?" p2))
  (with-interrupts-disabled ; disable GC while extracting addresses
   (= (cpointer-address p1) (cpointer-address p2))))

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
    (do-ptr-add p (* n (ctype-sizeof type)) #t)]
   [(p n)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-add "cpointer?" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add "exact-integer?" n))
    (do-ptr-add p n #t)]))

(define (do-ptr-add p n save-tags?)
  (cond
   [(authentic-cpointer? p)
    (make-cpointer+offset (cpointer-memory p)
                          (and save-tags? (cpointer-tag p))
                          (+ n (if (cpointer+offset? p)
                                   (cpointer+offset-offset p)
                                   0)))]
   [(has-cpointer-property? p)
    (do-ptr-add (unwrap-cpointer p) n save-tags?)]
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
  (fields host-rep    ; host-Scheme representation description, 'struct, 'union, or 'array
          our-rep     ; Racket representation description
          basetype    ; parent ctype or the same as `our-rep`
          scheme->c   ; converter of values to `basetype`
          c->scheme)) ; converter of values from `basetype`

;; A `compound-ctype` is used for structs, unions, and arrays
(define-record-type (compound-ctype create-compound-ctype compound-ctype?)
  (parent ctype)
  (fields get-decls
          size
          alignment))

(define/who (make-ctype type racket-to-c c-to-racket)
  (check who ctype? type)
  (check who (procedure-arity-includes/c 1) :or-false racket-to-c)
  (check who (procedure-arity-includes/c 1) :or-false c-to-racket)
  (cond
   [(compound-ctype? type)
    (create-compound-ctype (ctype-host-rep type)
                           (ctype-our-rep type)
                           type
                           racket-to-c
                           c-to-racket
                           (compound-ctype-get-decls type)
                           (compound-ctype-size type)
                           (compound-ctype-alignment type))]
   [else
    (create-ctype (ctype-host-rep type)
                  (ctype-our-rep type)
                  type
                  racket-to-c
                  c-to-racket)]))

;; Apply all the conversion wrappers of `type` to the Scheme value `v`
(define (s->c type v)
  (let* ([racket-to-c (ctype-scheme->c type)]
         [v (if racket-to-c
                (racket-to-c v)
                v)]
         [next (ctype-basetype type)])
    (if (ctype? next)
        (s->c next v)
        v)))

;; Apply all the conversion wrapper of `type` to the C value `v`
(define (c->s type v)
  (let* ([next (ctype-basetype type)]
         [v (if (ctype? next)
                (c->s next v)
                v)]
         [c-to-racket (ctype-c->scheme type)])
    (if c-to-racket
        (c-to-racket v)
        v)))

;; ----------------------------------------

(define-syntax define-ctype
  (syntax-rules ()
    [(_ id host-rep basetype)
     (define id (create-ctype host-rep basetype basetype #f #f))]
    [(_ id host-rep basetype s->c)
     (define id (create-ctype host-rep basetype basetype s->c #f))]
    [(_ id host-rep basetype s->c c->s)
     (define id (create-ctype host-rep basetype basetype s->c c->s))]))

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

;; Unlike traditional Racket, copies when converting from C:
(define-ctype _bytes 'void* 'bytes
  (lambda (x) x)
  (lambda (x) (let loop ([i 0])
                (if (fx= 0 (foreign-ref 'unsigned-8 x i))
                    (let ([bstr (make-bytes i)])
                      (memcpy* bstr 0 x 0 i)
                      bstr)
                    (loop (add1 i))))))

(define-ctype _short_bytes 'void* 'bytes
  (lambda (x) x)
  (lambda (x) (let loop ([i 0])
                (if (fx= 0 (foreign-ref 'unsigned-16 x i))
                    (let ([bstr (make-bytes i)])
                      (memcpy* bstr 0 x 0 i)
                      bstr)
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

(define-ctype _longdouble 'double 'double
  (lambda (x) (bad-ctype-value '_longdouble x)))

(define-ctype _pointer 'void* 'pointer
  (lambda (v) v) ; resolved to an address later (with the GC disabled)
  (lambda (x) (memory->cpointer x)))

;; Treated specially by `ptr-ref`
(define-ctype _fpointer 'void* 'fpointer
  (lambda (v) v) ; resolved to an address later (with the GC disabled)
  (lambda (x)
    (if (ffi-obj? x) ; check for `ptr-ref` special case on `ffi-obj`s
        x
        (memory->cpointer x))))

(define-ctype _gcpointer 'void* 'gcpointer
  (lambda (v) v) ; like `_pointer`: resolved later
  (lambda (x)
    ;; `x` must have been converted to a bytevector or vector before
    ;; the GC was re-enabled
    (memory->cpointer x)))

;; FIXME:
(define-ctype _stdbool 'integer-8 'stdbool
  (lambda (x) (and x 0))
  (lambda (v) (not (zero? v))))

(define make-cstruct-type
  (case-lambda
   [(types) (make-cstruct-type types #f #f)]
   [(types abi) (make-cstruct-type types abi #f)]
   [(types abi alignment)
    (let ([make-decls
           (lambda (id)
             (let-values ([(reps decls) (types->reps types)])
               (append decls
                       `((define-ftype ,id
                           (struct ,@(map (lambda (rep)
                                            `[,(gensym) ,rep])
                                          reps)))))))])
      (let-values ([(size alignment) (ctypes-sizeof+alignof types alignment)])
        (create-compound-ctype 'struct
                               'struct
                               types
                               (lambda (s) s) ; like `_pointer`: resolved later
                               (lambda (c) c)
                               make-decls
                               size
                               alignment)))]))

(define/who (make-union-type . types)
  (for-each (lambda (type) (check who ctype? type))
            types)
  (let ([make-decls
         (lambda (id)
           (let-values ([(reps decls) (types->reps types)])
             (append decls
                     `((define-ftype ,id
                         (union ,@(map (lambda (rep)
                                         `[,(gensym) ,rep])
                                       reps)))))))]
        [size (apply max (map ctype-sizeof types))]
        [alignment (apply max (map ctype-alignof types))])
    (create-compound-ctype 'union
                           'union
                           types
                           (lambda (s) s) ; like `_pointer`: resolved later
                           (lambda (c) c)
                           make-decls
                           size
                           alignment)))

(define/who (make-array-type type count)
  (check who ctype? type)
  (check who exact-nonnegative-integer? count)
  (let ([make-decls
         (lambda (id)
           (let-values ([(reps decls) (types->reps (list type))])
             (append decls
                     `((define-ftype ,id
                         (array ,count ,(car reps)))))))]
        [size (* count (ctype-sizeof type))]
        [alignment (ctype-alignof type)])
    (create-compound-ctype 'array
                           'array
                           'array ; type
                           (lambda (s) s) ; like `_pointer`: resolved later
                           (lambda (c) (make-cpointer c #f)) ; was atomically converted to a bytestring
                           make-decls
                           size
                           alignment)))

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
  (let ([t (ctype-our-rep c)])
    (if (or (eq? t 'gcpointer) (eq? t 'scheme))
        'nonatomic
        'atomic)))

(define/who (ctype-sizeof c)
  (check who ctype? c)
  (case (ctype-host-rep c)
    [(void) 0]
    [(boolean int) 4]
    [(double) 8]
    [(float) 4]
    [(integer-8 unsigned-8) 1]
    [(integer-16 unsigned-16) 2]
    [(integer-32 unsigned-32) 4]
    [(integer-64 unsigned-64) 8]
    [else
     (if (compound-ctype? c)
         (compound-ctype-size c)
         ;; Everything else is pointer-sized:
         (foreign-sizeof 'void*))]))

(define (ctypes-sizeof+alignof base alignment)
  (let ([align (lambda (size algn)
                 (let ([amt (modulo size (or alignment algn))])
                   (if (zero? amt)
                       size
                       (+ size (- algn amt)))))])
    (let loop ([types base] [size 0] [max-align 1])
      (cond
       [(null? types) (values (align size max-align)
                              max-align)]
       [else (let ([sz (ctype-sizeof (car types))]
                   [algn (ctype-alignof (car types))])
               (loop (cdr types)
                     (+ (align size algn)
                        sz)
                     (max algn max-align)))]))))

(define/who (ctype-alignof c)
  (check who ctype? c)
  (cond
   [(compound-ctype? c)
    (compound-ctype-alignment c)]
   [else
    (ctype-sizeof c)]))

(define/who (cpointer-gcable? p)
  (check who cpointer? p)
  (and (authentic-cpointer? p)
       (let ([memory (cpointer-memory p)])
         (or (bytes? memory)
             (vector? memory)))))

;; ----------------------------------------

(define-record-type (ffi-lib make-ffi-lib ffi-lib?)
  (fields handle name))

(define ffi-lib*
  (case-lambda
   [(name) (ffi-lib* name #f #f)]
   [(name fail-as-false?) (ffi-lib* name fail-as-false? #f)]
   [(name fail-as-false? as-global?)
    (let ([name (if (string? name)
                    (string->immutable-string name)
                    name)])
      (ffi-get-lib 'ffi-lib
                   name
                   as-global?
                   fail-as-false?
                   (lambda (h)
                     (make-ffi-lib h name))))]))

(define-record-type (cpointer/ffi-obj make-ffi-obj ffi-obj?)
  (parent cpointer)
  (fields lib name))

(define/who (ffi-obj name lib)
  (check who bytes? name)
  (check who ffi-lib? lib)
  (let ([name (bytes->immutable-bytes name)])
    (ffi-get-obj who
                 (ffi-lib-handle lib)
                 (ffi-lib-name lib)
                 name
                 (lambda (addr)
                   (make-ffi-obj addr #f lib name)))))

(define (ffi-obj-name obj)
  (cpointer/ffi-obj-name obj))

(define (ffi-obj-lib obj)
  (cpointer/ffi-obj-lib obj))

(define ffi-get-lib
  ;; Placeholder implementation that either fails
  ;; or returns a dummy value:
  (lambda (who name as-global? fail-as-false? success-k)
    (if fail-as-false?
        #f
        (success-k #f))))

(define ffi-get-obj
  ;; Placeholder implementation that always fails:
  (lambda (who lib lib-name name success-k)
    (raise
     (|#%app|
      exn:fail:filesystem
      (format "~a: not yet ready\n  name: ~a" who name)
      (current-continuation-marks)))))

(define (set-ffi-get-lib-and-obj! do-ffi-get-lib do-ffi-get-obj)
  (set! ffi-get-lib do-ffi-get-lib)
  (set! ffi-get-obj do-ffi-get-obj))

;; ----------------------------------------

(define/who ptr-ref
  (case-lambda
   [(p type)
    (check who cpointer? p)
    (check who ctype? type)
    (c->s type (foreign-ref* type p 0))]
   [(p type offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who exact-integer? offset)
    (c->s type (foreign-ref* type
                             p
                             (* (ctype-sizeof type) offset)))]
   [(p type abs-tag offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    (c->s type (foreign-ref* type p offset))]))

(define (foreign-ref* type p offset)
  (cond
   [(and (ffi-obj? p)
         (eq? 'fpointer (ctype-our-rep type)))
    ;; Special case for `ptr-ref` on a function-type ffi-object:
    ;; cancel a level of indirection and preserve `ffi-obj`ness
    ;; to keep its name
    p]
   [else
    (cond
     [(compound-ctype? type)
      ;; Instead of copying, get a pointer within `p`:
      (do-ptr-add p offset #f)]
     [else
      (let ([p (unwrap-cpointer p)]
            [host-rep (ctype-host-rep type)])
        ;; Disable interrupts to avoid a GC:
        (with-interrupts-disabled
         ;; Special treatment is needed for 'scheme-object, since the
         ;; host Scheme rejects the use of 'scheme-object with
         ;; `foreign-ref`
         (let ([v (foreign-ref (if (eq? host-rep 'scheme-object)
                                   'uptr
                                   host-rep)
                               (cpointer-address p)
                               offset)])
           (case host-rep
             [(scheme-object) (address->object v 1)]
             [else
              (case (ctype-our-rep type)
                [(gcpointer) (addr->gcpointer-memory v)]
                [else v])]))))])]))

(define/who ptr-set!
  (case-lambda
   [(p type v)
    (check who cpointer? p)
    (check who ctype? type)
    (foreign-set!* type
                   p
                   0
                   v)]
   [(p type offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who exact-integer? offset)
    (foreign-set!* type
                   p
                   (* (ctype-sizeof type) offset)
                   v)]
   [(p type abs-tag offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    (foreign-set!* type
                   p
                   offset
                   v)]))

(define ptr-size-in-bytes (foreign-sizeof 'void*))
(define log-ptr-size-in-bytes (- (integer-length ptr-size-in-bytes) 4))

(define (foreign-set!* type p offset v)
  (let ([p (unwrap-cpointer p)])
    (cond
     [(compound-ctype? type)
      ;; Corresponds to a copy, since `v` is represented by a pointer
      (memcpy* p offset
               (s->c type v) 0
               (compound-ctype-size type))]
     [else
      (let ([host-rep (ctype-host-rep type)]
            [v (s->c type v)])
        (cond
         [(and (authentic-cpointer? p)
               (vector? (cpointer-memory p))
               (zero? (fxand offset (fx- ptr-size-in-bytes 1))))
          ;; For writing into a vector, use `vector-set!`
          ;; to trigger the write barrier
          (let ([host-rep (ctype-host-rep type)]
                [i (fxsrl offset log-ptr-size-in-bytes)])
            (vector-set! (cpointer-memory p)
                         i
                         (if (eq? host-rep 'scheme-object)
                             v
                             (cpointer-address v))))]
         [(and (authentic-cpointer? p)
               (vector? (cpointer-memory p)))
          (raise-unsupported-error 'ptr-set! "unsupported non-atomic memory update")]
         [else
          ;; Disable interrupts to avoid a GC:
          (with-interrupts-disabled
           ;; Special treatment is needed for 'scheme-object, since
           ;; the host Scheme rejects the use of 'scheme-object with
           ;; `foreign-set!`
           (foreign-set! (if (eq? host-rep 'scheme-object)
                             'uptr
                             host-rep)
                         (cpointer-address p)
                         offset
                         (case host-rep
                           [(scheme-object) (object->addr v 1)]
                           [(void*) (cpointer-address v)]
                           [else v])))]))])))

(define (memcpy* to to-offset from from-offset len)
  (let ([to (unwrap-cpointer* to)]
        [from (unwrap-cpointer* from)])
    (when (and (authentic-cpointer? to)
               (vector? (cpointer-memory to)))
      (raise-unsupported-error 'memcpy "unsupported non-atomic memory update"))
    (with-interrupts-disabled
     (let ()
       (let loop ([to (+ (cpointer*-address to) to-offset)]
                  [from (+ (cpointer*-address from) from-offset)]
                  [len len])
         (unless (fx= len 0)
           (cond
            [(fx>= len 8)
             (foreign-set! 'integer-64 to 0
                           (foreign-ref 'integer-64 from 0))
             (loop (fx+ to 8) (fx+ from 8) (fx- len 8))]
            [(fx>= len 4)
             (foreign-set! 'integer-32 to 0
                           (foreign-ref 'integer-32 from 0))
             (loop (fx+ to 4) (fx+ from 4) (fx- len 4))]
            [(fx>= len 2)
             (foreign-set! 'integer-16 to 0
                           (foreign-ref 'integer-16 from 0))
             (loop (fx+ to 2) (fx+ from 2) (fx- len 2))]
            [else
             (foreign-set! 'integer-8 to 0
                           (foreign-ref 'integer-8 from 0))
             (loop (fx+ to 1) (fx+ from 1) (fx- len 1))])))))))

(define memcpy
  (case-lambda
   [(cptr src-cptr count)
    (memcpy* cptr 0 src-cptr 0 count)]
   [(cptr offset src-cptr count)
    (memcpy* cptr offset src-cptr 0 count)]
   [(cptr offset src-cptr src-offset/count count/type)
    (if (ctype? count/type)
        (let ([sz (ctype-sizeof count/type)])
          (memcpy* cptr (* sz offset) src-cptr 0 (* src-offset/count sz)))
        (memcpy* cptr offset src-cptr src-offset/count count/type))]
   [(cptr offset src-cptr src-offset count type)
    (let ([sz (ctype-sizeof type)])
      (memcpy* cptr (* offset sz) src-cptr (* src-offset sz) (* count sz)))]))

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
      (normalized-malloc (* arg1 (ctype-sizeof arg2)) (ctype-malloc-mode arg2))]
     [(and (ctype? arg1)
           (nonnegative-fixnum? arg2))
      (normalized-malloc (* arg2 (ctype-sizeof arg1)) (ctype-malloc-mode arg1))]
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
          (memcpy* p 0 copy-from 0 len))
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
   [(eqv? size 0) #f]
   [(eq? mode 'raw)
    (make-cpointer (foreign-alloc size) #f)]
   [(eq? mode 'atomic)
    (make-cpointer (make-bytevector size) #f)]
   [(eq? mode 'nonatomic)
    (make-cpointer (make-vector (quotient size 8)) #f)]
   [(eq? mode 'atomic-interior)
    ;; This is not quite the same as traditional Racket, because
    ;; a finalizer is associated with the cpointer (as opposed to
    ;; the address that is wrapped by the cpointer)
    (let* ([bstr (make-bytevector size)]
           [p (make-cpointer bstr #f)])
      (lock-object bstr)
      (the-foreign-guardian p (lambda () (unlock-object bstr)))
      p)]
   [else
    (raise-unsupported-error 'malloc
                             (format "'~a mode is not supported" mode))]))

(define (free p)
  (foreign-free (cpointer-memory p)))

(define (malloc-immobile-cell v)
  (let ([vec (vector v)])
    (lock-object vec)
    (make-cpointer vec #f)))

(define (free-immobile-cell b)
  (unlock-object (cpointer-memory b)))

(define (malloc-mode? v)
  (chez:memq v '(raw atomic nonatomic tagged
                     atomic-interior interior
                     stubborn uncollectable eternal)))

(define (end-stubborn-change p)
  (raise-unsupported-error 'end-stubborn-change))

(define (extflvector->cpointer extfl-vector)
  (raise-unsupported-error 'extflvector->cpointer))

(define (vector->cpointer vec)
  (make-cpointer vec #f))

(define (flvector->cpointer flvec)
  (make-cpointer (flvector-bstr flvec) #f))

;; ----------------------------------------

(define the-foreign-guardian (make-guardian))

(define (poll-foreign-guardian)
  (let ([v (the-foreign-guardian)])
    (when v
      (v)
      (poll-foreign-guardian))))

;; ----------------------------------------

(define/who ffi-call
  (case-lambda
   [(p in-types out-type)
    (ffi-call p in-types out-type #f #f #f)]
   [(p in-types out-type abi)
    (ffi-call p in-types out-type abi #f #f)]
   [(p in-types out-type abi save-errno?)
    (ffi-call p in-types out-type abi save-errno? #f)]
   [(p in-types out-type abi save-errno? orig-place?)
    (ffi-call p in-types out-type abi save-errno? orig-place? #f)]
   [(p in-types out-type abi save-errno? orig-place? lock-name)
    (check who cpointer? p)
    (check who (lambda (l)
                 (and (list? l)
                      (andmap ctype? l)))
           :contract "(listof ctype?)"
           in-types)
    (check who ctype? out-type)
    (ffi-call/callable #t p in-types out-type abi)]))

(define (ffi-call/callable call? to-wrap in-types out-type abi)
  (let* ([conv (case abi
                 [(stdcall) '__stdcall]
                 [(sysv) '__cdecl]
                 [else #f])]
         [by-value? (lambda (type)
                      ;; An 'array rep is compound, but should be
                      ;; passed as a pointer, so only pass 'struct and
                      ;; 'union "by value":
                      (chez:memq (ctype-host-rep type) '(struct union)))]
         [array-rep-to-pointer-rep (lambda (host-rep)
                                     (if (eq? host-rep 'array)
                                         'void*
                                         host-rep))]
         [ids (map (lambda (in-type)
                     (and (by-value? in-type)
                          (gensym)))
                   in-types)]
         [ret-id (and (by-value? out-type)
                      (gensym))]
         [decls (let loop ([in-types in-types] [ids ids] [decls '()])
                  (cond
                   [(null? in-types) decls]
                   [(car ids)
                    (let ([id-decls ((compound-ctype-get-decls (car in-types)) (car ids))])
                      (loop (cdr in-types) (cdr ids) (append decls id-decls)))]
                   [else
                    (loop (cdr in-types) (cdr ids) decls)]))]
         [ret-decls (if ret-id
                        ((compound-ctype-get-decls out-type) ret-id)
                        '())]
         [ret-size (and ret-id (ctype-sizeof out-type))]
         [gen-proc+ret-maker+arg-makers
          (let ([expr `(let ()
                         ,@decls
                         ,@ret-decls
                         (list
                          (lambda (to-wrap)
                            (,(if call? 'foreign-procedure 'foreign-callable)
                             ,conv
                             to-wrap
                             ,(map (lambda (in-type id)
                                     (if id
                                         `(& ,id)
                                         (array-rep-to-pointer-rep
                                          (ctype-host-rep in-type))))
                                   in-types ids)
                             ,(if ret-id
                                  `(& ,ret-id)
                                  (array-rep-to-pointer-rep
                                   (ctype-host-rep out-type)))))
                          ,(and (not call?)
                                ret-id
                                `(lambda (p)
                                   (make-ftype-pointer ,ret-id p)))
                          ,@(if call?
                                (map (lambda (id)
                                       (and id
                                            `(lambda (p)
                                               (make-ftype-pointer ,id p))))
                                     ids)
                                '())))])
            (call-with-system-wind (lambda () (eval expr))))]
         [gen-proc (car gen-proc+ret-maker+arg-makers)]
         [ret-maker (cadr gen-proc+ret-maker+arg-makers)]
         [arg-makers (cddr gen-proc+ret-maker+arg-makers)])
    (cond
     [call?
      (let* ([proc-p (unwrap-cpointer to-wrap)])
        (lambda args
          (let* ([args (map (lambda (arg in-type)
                              (let ([arg (s->c in-type arg)])
                                (if (cpointer? arg)
                                    (unwrap-cpointer arg)
                                    arg)))
                            args in-types)]
                 [r (with-interrupts-disabled
                     (let ([r (apply (gen-proc (cpointer-address proc-p))
                                     (map (lambda (arg in-type maker)
                                            (let ([host-rep (array-rep-to-pointer-rep
                                                             (ctype-host-rep in-type))])
                                              (case host-rep
                                                [(void*) (cpointer-address arg)]
                                                [(struct union)
                                                 (maker (cpointer-address arg))]
                                                [else arg])))
                                          args in-types arg-makers))])
                       (cond
                        [ret-id
                         ;; result is a struct type; we want to copy and free before re-enabling interrupts
                         (let ([bstr (make-bytevector ret-size)]
                               [addr (ftype-pointer-address r)])
                           (memcpy* bstr 0 addr 0 ret-size)
                           (foreign-free addr)
                           (make-cpointer bstr #f))]
                        [(eq? (ctype-our-rep out-type) 'gcpointer)
                         (addr->gcpointer-memory r)]
                        [else r])))])
            (c->s out-type r))))]
     [else ; callable
      (gen-proc (lambda args
                  (let ([v (s->c
                            out-type
                            (apply to-wrap
                                   (let loop ([args args] [in-types in-types])
                                     (cond
                                      [(null? args) '()]
                                      [else
                                       (let* ([arg (car args)]
                                              [type (car in-types)]
                                              [arg (c->s type
                                                         (case (ctype-host-rep type)
                                                           [(struct union)
                                                            (let* ([size (compound-ctype-size type)]
                                                                   [addr (ftype-pointer-address arg)]
                                                                   [bstr (make-bytevector size)])
                                                              (memcpy* bstr 0 addr 0 size)
                                                              (foreign-free addr)
                                                              (make-cpointer bstr #f))]
                                                           [else
                                                            (cond
                                                             [(eq? (ctype-our-rep type) 'gcpointer)
                                                              (addr->gcpointer-memory arg)]
                                                             [else arg])]))])
                                         (cons arg (loop (cdr args) (cdr in-types))))]))))])
                    (if ret-maker
                        (ret-maker (let* ([size (compound-ctype-size out-type)]
                                          [addr (foreign-alloc size)])
                                     (memcpy* addr 0 v 0 size)
                                     (ret-maker addr)))
                        (case (ctype-host-rep out-type)
                          [(void*) (cpointer-address v)]
                          [else v])))))])))

(define (types->reps types)
  (let loop ([types types] [reps '()] [decls '()])
    (cond
     [(null? types) (values (reverse reps) decls)]
     [else
      (let ([type (car types)])
        (if (compound-ctype? type)
            (let* ([id (gensym)]
                   [id-decls ((compound-ctype-get-decls type) id)])
              (loop (cdr types) (cons id reps) (append id-decls decls)))
            (loop (cdr types) (cons (ctype-host-rep type) reps) decls)))])))

;; ----------------------------------------

(define-record-type (callback create-callback ffi-callback?)
  (fields code))

(define/who ffi-callback
  (case-lambda
   [(proc in-types out-type)
    (ffi-callback proc in-types out-type #f #f #f)]
   [(proc in-types out-type abi)
    (ffi-callback proc in-types out-type abi #f #f)]
   [(proc in-types out-type abi atomic?)
    (ffi-callback proc in-types out-type abi atomic? #f)]
   [(proc in-types out-type abi atomic async-apply)
    (check who procedure? proc)
    (check who (lambda (l)
                 (and (list? l)
                      (andmap ctype? l)))
           :contract "(listof ctype?)"
           in-types)
    (check who ctype? out-type)
    (let* ([code (ffi-call/callable #f proc in-types out-type abi)]
           [cb (create-callback code)])
      (lock-object code)
      (the-foreign-guardian cb (lambda () (unlock-object code)))
      cb)]))

;; ----------------------------------------

(define-syntax define-foreign-not-yet-available
  (syntax-rules ()
    [(_ id)
     (define (id . args)
       (error 'id "foreign API not yet supported"))]
    [(_ id ...)
     (begin (define-foreign-not-yet-available id) ...)]))

(define-foreign-not-yet-available
  lookup-errno
  make-sized-byte-string
  memmove
  memset
  saved-errno)

;; ----------------------------------------

(define process-global-table (make-hashtable equal-hash-code equal?))
(define process-table-lock (make-mutex))

(define (unsafe-register-process-global key val)
  (with-interrupts-disabled
   (mutex-acquire process-table-lock)
   (let ([result (cond
                  [(not val)
                   (hashtable-ref process-global-table key #f)]
                  [else
                   (let ([old-val (hashtable-ref process-global-table key #f)])
                     (cond
                      [(not old-val)
                       (hashtable-set! process-global-table key val)
                       #f]
                      [else old-val]))])])
     (mutex-release process-table-lock)
     result)))
