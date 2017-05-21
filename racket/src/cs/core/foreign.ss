
(define-values (prop:cpointer has-cpointer-property? cpointer-property-ref)
  (make-struct-type-property 'cpointer))

(define-record-type (ctype create-ctype ctype?)
  (fields s-exp basetype scheme->c c->scheme))
(define-record-type (cpointer make-cpointer authentic-cpointer?)
  (fields addr (mutable tag)))

(define (cpointer? v)
  (or (authentic-cpointer? v)
      (not v)
      (bytes? v)
      (has-cpointer-property? v)))

(define (make-ctype type racket-to-c c-to-racket)
  (unless (ctype? type)
    (raise-argument-error 'make-ctype "ctype?" type))
  (create-ctype (ctype-s-exp type) type racket-to-c c-to-racket))

(define-syntax define-ctype
  (syntax-rules ()
    [(_ id s-exp)
     (define id (create-ctype s-exp #f #f #f))]
    [(_ id s-exp s->c)
     (define id (create-ctype s-exp #f s->c #f))]
    [(_ id s-exp s->c c->s)
     (define id (create-ctype s-exp #f s->c c->s))]))

(define-ctype _bool 'boolean)
(define-ctype _bytes 'u8*)
(define-ctype _double 'double)
(define-ctype _fixnum 'fixnum)
(define-ctype _float 'float)
(define-ctype _int8 'integer-8)
(define-ctype _int16 'integer-16)
(define-ctype _int32 'integer-32)
(define-ctype _int64 'integer-64)
(define-ctype _uint8 'unsigned-8)
(define-ctype _uint16 'unsigned-16)
(define-ctype _uint32 'unsigned-32)
(define-ctype _uint64 'unsigned-64)
(define-ctype _scheme 'scheme-object)
(define-ctype _string/ucs-4 (if (system-big-endian?) 'utf-32be 'utf-32le))
(define-ctype _string/utf-16 (if (system-big-endian?) 'utf-16be 'utf-16le))
(define-ctype _void 'void)

(define (bad-ctype-value type-name v)
  (raise-arguments-error 'apply
                         "bad value for conversion"
                         "ctype" type-name
                         "value" v))

(define-ctype _double* 'double
  (lambda (x) (if (real? x)
                  (exact->inexact x)
                  (bad-ctype-value '_double* x))))
(define-ctype _ufixnum 'fixnum
  (lambda (x) (if (and (fixnum? x) (fx> x 0))
                  x
                  (bad-ctype-value x '_ufixnum))))
(define-ctype _fixint 'integer
  (lambda (x) (if (fixnum? x)
                  x
                  (bad-ctype-value x '_fixint))))
(define-ctype _ufixint 'unsigned
  (lambda (x) (if (fixnum? x)
                  x
                  (bad-ctype-value x '_ufixint))))

(define-ctype _symbol 'string
  (lambda (x) (if (symbol? x)
                  (symbol->string x)
                  (bad-ctype-value '_symbol x)))
  (lambda (s) (string->symbol s)))

;; FIXME:
(define-ctype _path 'string)

(define-ctype _longdouble 'double
  (lambda (x) (bad-ctype-value '_longdouble x)))

(define-ctype _pointer 'void*
  (lambda (v) (cond
               [(cpointer? v) (cpointer-addr v)]
               [(not v) 0]
               ;; FIXME:
               [(bytes? v) v]))
  (lambda (x) (cond
               [(zero? x) #f]
               [else (make-cpointer x #f)])))

;; FIXME:
(define-ctype _fpointer 'void*)

(define-ctype _gcpointer 'void*
  (lambda (v) (cond
               [(cpointer? v) (cpointer-addr v)]
               [(not v) 0]
               [(bytes? v) v]))
  (lambda (x) (cond
               [(zero? x) #f]
               [else (make-cpointer x #f)])))

;; FIXME:
(define-ctype _stdbool 'integer-8
  (lambda (x) (and x 0))
  (lambda (v) (not (zero? v))))

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

(define (cpointer-gcable? p)
  (unless (cpointer? p)
    (raise-argument-error 'cpointer-gcable? "cpointer?" p))
  ;; FIXME:
  #f)

;; FIXME:
(define (make-stubborn-will-executor)
  (make-will-executor))

(define-syntax define-foreign-not-yet-available
  (syntax-rules ()
    [(_ id)
     (define (id . args)
       (error 'id "foreign API not yet supported"))]
    [(_ id ...)
     (begin (define-foreign-not-yet-available id) ...)]))

(define-foreign-not-yet-available
  ctype-sizeof
  ctype-alignof
  end-stubborn-change
  extflvector->cpointer
  ffi-call
  ffi-callback
  ffi-callback?
  ffi-lib
  ffi-lib-name
  ffi-lib?
  ffi-obj
  ffi-obj-lib
  ffi-obj-name
  ffi-obj?
  flvector->cpointer
  free
  free-immobile-cell
  lookup-errno
  make-array-type
  make-cstruct-type
  make-late-weak-box
  make-late-weak-hasheq
  make-sized-byte-string
  make-union-type
  malloc
  malloc-immobile-cell
  memcpy
  memmove
  memset
  offset-ptr?
  ptr-add
  ptr-add!
  ptr-equal?
  ptr-offset
  ptr-ref
  ptr-set!
  saved-errno
  set-cpointer-tag!
  set-ptr-offset!
  vector->cpointer)
